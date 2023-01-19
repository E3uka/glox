package scanner

import (
	glox_err "glox/error"
	"glox/token"
	"strconv"
)

type scanner struct {
	path    *string
	source  string
	tokens  []token.Token
	start   int
	current int
	line    int
}

func New(path *string, source *string) (*scanner, error) {
	scanner := &scanner{
		path:    path,
		source:  *source,
		tokens:  []token.Token{},
		start:   0,
		current: 0,
		line:    1, // beginning line of file
	}
	if err := scanner.init(); err != nil {
		return nil, err
	}
	return scanner, nil
}

func (s *scanner) Tokens() *[]token.Token {
	return &s.tokens
}

func (s *scanner) is_at_end() bool {
	return s.current >= len(s.source)
}

func is_digit(char rune) bool {
	return char >= '0' && char <= '9'
}

func is_alpha(char rune) bool {
	return (char >= 'a' && char <= 'z') ||
		(char >= 'A' && char <= 'Z') ||
		char == '_'
}

func is_alphanumeric(char rune) bool {
	return is_alpha(char) || is_digit(char)
}

func (s *scanner) init() error {
	for !s.is_at_end() {
		s.start = s.current
		if err := s.scan(); err != nil {
			return err
		}
	}
	// append EOF token at end of source file
	s.add_token(token.EOF, "")
	return nil
}

func (s *scanner) scan() error {
	char := s.step()

	switch char {
	case '\n':
		s.line += 1
		break

	// ignore the following
	case ' ', '\r', '\t':

	// single character lexemes
	case '(':
		s.add_token(token.LPAREN, "(")
	case ')':
		s.add_token(token.RPAREN, ")")
	case '[':
		s.add_token(token.LBRACK, "[")
	case ']':
		s.add_token(token.RBRACK, "]")
	case '{':
		s.add_token(token.LBRACE, "{")
	case '}':
		s.add_token(token.RBRACE, "}")
	case ',':
		s.add_token(token.COMMA, ",")
	case '.':
		s.add_token(token.PERIOD, ".")
	case ';':
		s.add_token(token.SEMICOLON, ";")
	case '*':
		s.add_token(token.STAR, "*")

	// double-character lexemes
	case '+':
		if s.match_advance('+') {
			s.add_token(token.INCR, "++")
		} else if s.match_advance('=') {
			s.add_token(token.INCRBY, "+=")
		} else {
			s.add_token(token.ADD, "+")
		}
	case '-':
		if s.match_advance('>') {
			s.add_token(token.FUNRETURN, "->")
		} else if s.match_advance('-') {
			s.add_token(token.DECR, "--")
		} else if s.match_advance('=') {
			s.add_token(token.DECRYBY, "-=")
		} else {
			s.add_token(token.SUB, "-")
		}
	case '!':
		if s.match_advance('=') {
			s.add_token(token.NEQ, "!=")
		} else {
			s.add_token(token.NOT, "!")
		}
	case '=':
		if s.match_advance('=') {
			s.add_token(token.EQL, "==")
		} else {
			s.add_token(token.ASSIGN, "=")
		}
	case '<':
		if s.match_advance('=') {
			s.add_token(token.LEQ, "<=")
		} else if s.match_advance('-') {
			s.add_token(token.CAST, "<-")
		} else {
			s.add_token(token.LSS, "<")
		}
	case '>':
		if s.match_advance('=') {
			s.add_token(token.GEQ, ">=")
		} else {
			s.add_token(token.GTR, ">")
		}
	case ':':
		if s.match_advance(':') {
			s.add_token(token.FUNASSIGN, "::")
		} else if s.match_advance('=') {
			s.add_token(token.WALRUS, ":=")
		} else {
			s.add_token(token.COLON, ":")
		}
	case '&':
		if s.match_advance('&') {
			s.add_token(token.AND, "&&")
		} else {
			s.add_token(token.BITAND, "&")
		}
	case '|':
		if s.match_advance('|') {
			s.add_token(token.OR, "||")
		} else {
			s.add_token(token.BITOR, "|")
		}
	case '/':
		if s.match_advance('/') {
			// step past all remaining characters until newline
			for s.peek() != '\n' && !s.is_at_end() {
				s.step()
			}
			// currently at the newline char, step again and update start
			// position to move past newline
			s.line += 1
			s.step()
			s.start = s.current
		} else if s.match_advance('*') {
			// keep it stepping
			for !s.is_at_end() {
				if s.peek() == '\n' {
					s.line += 1
				}
				s.step()

				if s.peek() == '*' {
					if s.peek_next() == '/' {
						s.step()
						break
					}
				}
			}
			if s.is_at_end() {
				return glox_err.ScanError(
					s.path,
					s.line,
					"unterminated block comment",
				)
			}
			// currently at the '/' end of newline char, step again and update
			// start position to move past it.
			s.step()
			s.start = s.current
		} else {
			s.add_token(token.QUO, "/")
		}

		// string literals
	case '"':
		s.scan_string()

	default:
		if is_digit(char) {
			s.scan_number()
		} else if is_alpha(char) {
			s.scan_identifier()
		} else {
			return glox_err.ScanError(s.path, s.line, "unexpected character")
		}
	}
	return nil
}

func (s *scanner) step() rune {
	s.current += 1
	return rune(s.source[s.current-1])
}

func (s *scanner) match_advance(char rune) bool {
	if s.is_at_end() {
		return false
	} else if rune(s.source[s.current]) != char {
		return false
	} else {
		// a match is found, it is safe to step
		s.current += 1
		return true
	}
}

func (s *scanner) peek() rune {
	if s.is_at_end() {
		return '\x00' // null terminated string in go
	}
	return rune(s.source[s.current])
}

func (s *scanner) peek_next() rune {
	if s.current+1 >= len(s.source) {
		return '\x00'
	}
	return rune(s.source[s.current+1])
}

func (s *scanner) add_token(
	tok_type token.TokenType,
	literal string,
) {
	tok := token.Token{
		Type:    tok_type,
		Literal: literal,
		Start:   s.start,
		End:     s.start + len(literal),
		Line:    s.line,
	}
	s.tokens = append(s.tokens, tok)
}

func (s *scanner) scan_string() error {
	for s.peek() != '"' && !s.is_at_end() {
		if s.peek() == '\n' {
			s.line += 1
		}
		s.step()
	}
	if s.is_at_end() {
		return glox_err.ScanError(s.path, s.line, "unterminated string")
	}
	// step past the end of the string to the newline char
	s.step()
	// trim the surrounding quotes
	value := s.source[s.start+1 : s.current-1]
	s.add_token(token.STRING, value)
	return nil
}

func (s *scanner) scan_number() error {
	is_float := false
	for is_digit(s.peek()) {
		s.step()
	}
	// look for the fractional part of the number
	if s.peek() == '.' && is_digit(s.peek_next()) {
		is_float = true
		s.step() // consume the '.'
	}
	for is_digit(s.peek()) {
		s.step()
	}
	value := s.source[s.start:s.current]
	if is_float {
		if _, err := strconv.ParseFloat(value, 64); err != nil {
			return glox_err.ScanError(
				s.path,
				s.line,
				"could not parse string value",
			)
		}
		s.add_token(token.F64, value)
		return nil
	}
	if _, err := strconv.ParseInt(value, 10, 64); err != nil {
		return glox_err.ScanError(
			s.path,
			s.line,
			"could not parse string value",
		)
	}
	s.add_token(token.S64, value)
	return nil
}

func (s *scanner) scan_identifier() {
	for is_alphanumeric(s.peek()) {
		s.step()
	}
	value := s.source[s.start:s.current]
	tok := token.LookupKeyword(value)
	s.add_token(tok, value)
}
