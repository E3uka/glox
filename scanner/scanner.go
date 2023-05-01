package scanner

import (
	"fmt"
	g_err "glox/error"
	"glox/token"
	"strconv"
)

type scanner struct {
	path    *string
	source  string
	tokens  token.Tokens
	start   int
	current int
	line    int
}

func New(path string, source []byte) *scanner {
	return &scanner{
		path:    &path,
		source:  string(source),
		tokens:  token.Tokens{},
		start:   0,
		current: 0,
		line:    1, // beginning line of file
	}
}

func is_alpha(char rune) bool {
	return (char >= 'a' && char <= 'z') ||
		(char >= 'A' && char <= 'Z') ||
		char == '_'
}
func is_alphanumeric(char rune) bool { return is_alpha(char) || is_digit(char) }
func is_digit(char rune) bool        { return char >= '0' && char <= '9' }

func (s *scanner) add_token(tok_type token.TokenType, literal []byte) {
	s.tokens.Tokens = append(s.tokens.Tokens, tok_type)
	s.tokens.Lit = append(s.tokens.Lit, literal)
	s.tokens.Line = append(s.tokens.Line, uint16(s.line))
}

func (s *scanner) Tokens() *token.Tokens {
	for !s.is_at_end() {
		s.start = s.current
		s.scan()
	}
	// append EOF token at end of source file
	s.tokens.Tokens = append(s.tokens.Tokens, token.EOF)
	s.tokens.Lit = append(s.tokens.Lit, []byte{})
	s.tokens.Line = append(s.tokens.Line, uint16(s.line))
	return &s.tokens
}

func (s *scanner) is_at_end() bool { return s.current >= len(s.source) }

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
		return '\x00' /* null terminated string in go */
	}
	return rune(s.source[s.current])
}

func (s *scanner) peek_next() rune {
	if s.current+1 >= len(s.source) {
		return '\x00'
	}
	return rune(s.source[s.current+1])
}

func (s *scanner) scan() {
	char := s.step()

	switch char {
	case '\n':
		s.line += 1

	// ignore the following
	case ' ', '\r', '\t':

	// single character lexemes
	case '(':
		s.add_token(token.LPAREN, []byte("("))
	case ')':
		s.add_token(token.RPAREN, []byte(")"))
	case '[':
		s.add_token(token.LBRACK, []byte("["))
	case ']':
		s.add_token(token.RBRACK, []byte("]"))
	case '{':
		s.add_token(token.LBRACE, []byte("{"))
	case '}':
		s.add_token(token.RBRACE, []byte("}"))
	case ',':
		s.add_token(token.COMMA, []byte(","))
	case '.':
		s.add_token(token.PERIOD, []byte("."))
	case ';':
		s.add_token(token.SEMICOLON, []byte(";"))
	case '*':
		s.add_token(token.STAR, []byte("*"))

	// double-character lexemes
	case '+':
		if s.match_advance('+') {
			s.add_token(token.INCR, []byte("++"))
		} else if s.match_advance('=') {
			s.add_token(token.INCRBY, []byte("+="))
		} else {
			s.add_token(token.ADD, []byte("+"))
		}
	case '-':
		if s.match_advance('>') {
			s.add_token(token.FUNRETURN, []byte("->"))
		} else if s.match_advance('-') {
			s.add_token(token.DECR, []byte("--"))
		} else if s.match_advance('=') {
			s.add_token(token.DECRYBY, []byte("-="))
		} else {
			s.add_token(token.SUB, []byte("-"))
		}
	case '!':
		if s.match_advance('=') {
			s.add_token(token.NEQ, []byte("!="))
		} else {
			s.add_token(token.NOT, []byte("!"))
		}
	case '=':
		if s.match_advance('=') {
			s.add_token(token.EQL, []byte("=="))
		} else {
			s.add_token(token.ASSIGN, []byte("="))
		}
	case '<':
		if s.match_advance('=') {
			s.add_token(token.LEQ, []byte("<="))
		} else if s.match_advance('-') {
			s.add_token(token.CAST, []byte("<-"))
		} else {
			s.add_token(token.LSS, []byte("<"))
		}
	case '>':
		if s.match_advance('=') {
			s.add_token(token.GEQ, []byte(">="))
		} else {
			s.add_token(token.GTR, []byte(">"))
		}
	case ':':
		if s.match_advance(':') {
			s.add_token(token.FUNASSIGN, []byte("::"))
		} else if s.match_advance('=') {
			s.add_token(token.WALRUS, []byte(":="))
		} else {
			s.add_token(token.COLON, []byte(":"))
		}
	case '&':
		if s.match_advance('&') {
			s.add_token(token.AND, []byte("&&"))
		} else {
			s.add_token(token.BITAND, []byte("&"))
		}
	case '|':
		if s.match_advance('|') {
			s.add_token(token.OR, []byte("||"))
		} else {
			s.add_token(token.BITOR, []byte("|"))
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
				g_err.ScanPanic(s.path, s.line, "unterminated block comment")
			}
			// currently at the '/' end of newline char, step again and update
			// start position to move past it.
			s.step()
			s.start = s.current
		} else {
			s.add_token(token.QUO, []byte("/"))
		}

	// string literals
	case '"':
		s.scan_string()

	// number or identifier
	default:
		if is_digit(char) {
			s.scan_number()
		} else if is_alpha(char) {
			s.scan_identifier()
		} else {
			g_err.ScanPanic(
				s.path,
				s.line,
				fmt.Sprintf("unexpected character %v", string(char)),
			)
		}
	}
}

func (s *scanner) scan_identifier() {
	for is_alphanumeric(s.peek()) {
		s.step()
	}
	value := []byte(s.source[s.start:s.current])
	tok := token.LookupKeyword(value)
	s.add_token(tok, value)
}

func (s *scanner) scan_number() {
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
			g_err.ScanPanic(s.path, s.line, "could not parse string value")
		}
		s.add_token(token.F64, []byte(value))
		return
	}
	if _, err := strconv.ParseInt(value, 10, 64); err != nil {
		g_err.ScanPanic(s.path, s.line, "could not parse string value")
	}
	s.add_token(token.S64, []byte(value))
}

func (s *scanner) scan_string() {
	for s.peek() != '"' && !s.is_at_end() {
		if s.peek() == '\n' {
			s.line += 1
		}
		s.step()
	}
	if s.is_at_end() {
		g_err.ScanPanic(s.path, s.line, "unterminated string")
	}
	// step past the end of the string to the newline char
	s.step()
	// trim the surrounding quotes
	value := []byte(s.source[s.start+1 : s.current-1])
	s.add_token(token.STRING, value)
}
