package lexer

import (
	gloxError "glox/error"
	"glox/token"
	"strconv"
)

type lexer struct {
	path    *string
	source  string
	tokens  []token.Token
	start   int
	current int
	line    int
}

func New(path *string, source *string) (*lexer, error) {
	lexer := &lexer{
		path:    path,
		source:  *source,
		tokens:  []token.Token{}, // TODO: use a channel to pipe lexed tokens
		start:   0,
		current: 0,
		line:    1, // beginning line of file
	}

	if err := lexer.init(); err != nil {
		return nil, err
	}
	return lexer, nil
}

func (l *lexer) Tokens() *[]token.Token {
	return &l.tokens
}

func (l *lexer) is_at_end() bool {
	return l.current >= len(l.source)
}

func (l lexer) is_digit(char rune) bool {
	return char >= '0' && char <= '9'
}

func (l lexer) is_alpha(char rune) bool {
	return (char >= 'a' && char <= 'z') ||
		(char >= 'A' && char <= 'Z') ||
		char == '_'
}

func (l lexer) is_alphanumeric(char rune) bool {
	return l.is_alpha(char) || l.is_digit(char)
}

func (l *lexer) init() error {
	var err error

	for !l.is_at_end() {
		l.start = l.current
		err = l.lex()
		if err != nil {
			return err
		}
	}
	eof := token.Token{
		Type:    token.EOF,
		Lexeme:  "",
		Literal: struct{}{},
		Line:    l.line, // append EOF at end of source file
	}
	l.tokens = append(l.tokens, eof)

	return nil
}

func (l *lexer) lex() error {
	char := l.step()

	switch char {
	case '\n':
		l.line += 1
		break

	// ignore the following
	case ' ':
	case '\r':
	case '\t':

	// single character lexemes
	case '(':
		l.add_token(token.LPAREN)
	case ')':
		l.add_token(token.RPAREN)
	case '[':
		l.add_token(token.LBRACK)
	case ']':
		l.add_token(token.RBRACK)
	case '{':
		l.add_token(token.LBRACE)
	case '}':
		l.add_token(token.RBRACE)
	case ',':
		l.add_token(token.COMMA)
	case '.':
		l.add_token(token.PERIOD)
	case ';':
		l.add_token(token.SEMICOLON)
	case '*':
		l.add_token(token.MUL)

	// double-character lexemes
	case '+':
		if l.match_advance('+') {
			l.add_token(token.INCR)
		} else if l.match_advance('=') {
			l.add_token(token.INCRBY)
		} else {
			l.add_token(token.ADD)
		}
	case '-':
		if l.match_advance('>') {
			l.add_token(token.FUNRET)
		} else if l.match_advance('-') {
			l.add_token(token.DECR)
		} else if l.match_advance('=') {
			l.add_token(token.DECRYBY)
		} else {
			l.add_token(token.SUB)
		}
	case '!':
		if l.match_advance('=') {
			l.add_token(token.NEQ)
		} else {
			l.add_token(token.NOT)
		}
	case '=':
		if l.match_advance('=') {
			l.add_token(token.EQL)
		} else {
			l.add_token(token.ASSIGN)
		}
	case '<':
		if l.match_advance('=') {
			l.add_token(token.LEQ)
		} else {
			l.add_token(token.LSS)
		}
	case '>':
		if l.match_advance('=') {
			l.add_token(token.GEQ)
		} else {
			l.add_token(token.GTR)
		}
	case ':':
		if l.match_advance(':') {
			l.add_token(token.FUNASSIGN)
		} else {
			l.add_token(token.COLON)
		}
	case '&':
		if l.match_advance('&') {
			l.add_token(token.AND)
		} else {
			l.add_token(token.BITAND)
		}
	case '|':
		if l.match_advance('|') {
			l.add_token(token.OR)
		} else {
			l.add_token(token.BITOR)
		}
	case '/':
		if l.match_advance('/') {
			// step past all remaining characters until newline
			for l.peek() != '\n' && !l.is_at_end() {
				l.step()
			}
			// currently at the newline char, step again and update start
			// position to move past newline
			l.step()
			l.start = l.current
		} else if l.match_advance('*') {
			// keep it stepping
			for !l.is_at_end() {
				if l.peek() == '\n' {
					l.line += 1
				}
				l.step()

				if l.peek() == '*' {
					if l.peek_next() == '/' {
						l.step()
						break
					}
				}
			}

			if l.is_at_end() {
				return gloxError.LexError(l.path, l.line, "unterminated block comment")
			}

			// currently at the '/' end of newline char, step again and update
			// start position to move past it.
			l.step()
			l.start = l.current
		} else {
			l.add_token(token.QUO)
		}

		// string literals
	case '"':
		l.lex_string()

	default:
		if l.is_digit(char) {
			l.lex_float()
		} else if l.is_alpha(char) {
			l.lex_identifier()
		} else {
			return gloxError.LexError(l.path, l.line, "unexpected character")
		}
	}
	return nil
}

func (l *lexer) step() rune {
	l.current += 1
	return rune(l.source[l.current-1])
}

func (l *lexer) match_advance(char rune) bool {
	if l.is_at_end() {
		return false
	} else if rune(l.source[l.current]) != char {
		return false
	} else {
		// a match is found, it is safe to step
		l.current += 1
		return true
	}
}

func (l *lexer) peek() rune {
	if l.is_at_end() {
		return '\x00' // null terminated string in go
	}
	return rune(l.source[l.current])
}

func (l *lexer) peek_next() rune {
	if l.current+1 >= len(l.source) {
		return '\x00'
	}
	return rune(l.source[l.current+1])
}

func (l *lexer) add_token(tok_type token.TOKEN_TYPE) {
	l.add_token_with_literal(tok_type, struct{}{})
}

func (l *lexer) add_token_with_literal(
	tok_type token.TOKEN_TYPE,
	literal interface{},
) {
	text := l.source[l.start:l.current]
	tok := token.Token{
		Type:    tok_type,
		Lexeme:  text,
		Literal: literal,
		Line:    l.line,
	}
	l.tokens = append(l.tokens, tok)
}

func (l *lexer) lex_string() error {
	for l.peek() != '"' && !l.is_at_end() {
		if l.peek() == '\n' {
			l.line += 1
		}
		l.step()
	}

	if l.is_at_end() {
		return gloxError.LexError(l.path, l.line, "unterminated string")

	}

	// step past the end of the string to the newline char
	l.step()

	// trim the surrounding quotes
	value := l.source[l.start+1 : l.current-1]
	l.add_token_with_literal(token.STRING, value)
	return nil
}

func (l *lexer) lex_float() error {
	for l.is_digit(l.peek()) {
		l.step()
	}

	// look for the fractional part of the number
	if l.peek() == '.' && l.is_digit(l.peek_next()) {
		l.step() // consume the '.'
	}

	for l.is_digit(l.peek()) {
		l.step()
	}

	value := l.source[l.start:l.current]
	floatVal, err := strconv.ParseFloat(value, 64)
	if err != nil {
		return gloxError.LexError(l.path, l.line, "could not parse string value")
	}
	l.add_token_with_literal(token.FLOAT, floatVal)
	return nil
}

func (l *lexer) lex_identifier() {
	for l.is_alphanumeric(l.peek()) {
		l.step()
	}

	value := l.source[l.start:l.current]
	tok := token.Lookup(value)

	if tok == token.NIL {
		l.add_token_with_literal(tok, nil)
	} else if tok == token.TRUE {
		l.add_token_with_literal(tok, true)
	} else if tok == token.FALSE {
		l.add_token_with_literal(tok, false)
	} else {
		l.add_token(tok)
	}
}
