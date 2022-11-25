package scanner

import (
	gloxError "glox/error"
	"glox/token"
	"strconv"
)

type scanner struct {
	source  string
	tokens  []token.Token
	start   int
	current int
	line    int
}

func New(source string) *scanner {
	return &scanner{
		source:  source,
		tokens:  []token.Token{},
		start:   0,
		current: 0,
		line:    1, // beginning line of file.
	}
}

func (s *scanner) Tokens() []token.Token {
	return s.tokens
}

func (s *scanner) isAtEnd() bool {
	return s.current >= len(s.source)
}

func (s scanner) isDigit(char rune) bool {
	return char >= '0' && char <= '9'
}

func (s scanner) isAlpha(char rune) bool {
	return (char >= 'a' && char <= 'z') ||
		(char >= 'A' && char <= 'Z') ||
		char == '_'
}

func (s scanner) isAlphaNumeric(char rune) bool {
	return s.isAlpha(char) || s.isDigit(char)
}

func (s *scanner) ScanTokens() []token.Token {
	for !s.isAtEnd() {
		s.start = s.current
		s.scan()
	}

	eof := token.Token{
		TokType: token.EOF,
		Lexeme:  "",
		Literal: struct{}{},
		Line:    s.line, // append EOF at end of source file
	}
	s.tokens = append(s.tokens, eof)

	return s.tokens
}

func (s *scanner) scan() {
	char := s.step()

	switch char {
	case '\n':
		s.line += 1
		break

	// ignore the following
	case ' ':
	case '\r':
	case '\t':

	// single character lexemes
	case '(':
		s.addToken(token.LPAREN)
	case ')':
		s.addToken(token.RPAREN)
	case '[':
		s.addToken(token.LBRACK)
	case ']':
		s.addToken(token.RBRACK)
	case '{':
		s.addToken(token.LBRACE)
	case '}':
		s.addToken(token.RBRACE)
	case ',':
		s.addToken(token.COMMA)
	case '.':
		s.addToken(token.PERIOD)
	case ';':
		s.addToken(token.SEMICOLON)
	case '*':
		s.addToken(token.MUL)

	// double-character lexemes
	case '+':
		if s.checkAndStep('+') {
			s.addToken(token.INCR)
		} else if s.checkAndStep('=') {
			s.addToken(token.INCRBY)
		} else {
			s.addToken(token.ADD)
		}
	case '-':
		if s.checkAndStep('>') {
			s.addToken(token.FUNRET)
		} else if s.checkAndStep('-') {
			s.addToken(token.DECR)
		} else if s.checkAndStep('=') {
			s.addToken(token.DECRYBY)
		} else {
			s.addToken(token.SUB)
		}
	case '!':
		if s.checkAndStep('=') {
			s.addToken(token.NEQ)
		} else {
			s.addToken(token.NOT)
		}
	case '=':
		if s.checkAndStep('=') {
			s.addToken(token.EQL)
		} else {
			s.addToken(token.ASSIGN)
		}
	case '<':
		if s.checkAndStep('=') {
			s.addToken(token.LEQ)
		} else {
			s.addToken(token.LSS)
		}
	case '>':
		if s.checkAndStep('=') {
			s.addToken(token.GEQ)
		} else {
			s.addToken(token.GTR)
		}
	case ':':
		if s.checkAndStep('=') {
			s.addToken(token.WALRUS)
		} else if s.checkAndStep(':') {
			s.addToken(token.MODASSIGN)
		} else {
			s.addToken(token.COLON)
		}
	case '&':
		if s.checkAndStep('&') {
			s.addToken(token.AND)
		} else {
			s.addToken(token.BITAND)
		}
	case '|':
		if s.checkAndStep('|') {
			s.addToken(token.OR)
		} else {
			s.addToken(token.BITOR)
		}
	case '/':
		if s.checkAndStep('/') {
			// step past all remaining characters until newline
			for s.peek() != '\n' && !s.isAtEnd() {
				s.step()
			}
			// currently at the newline char, step again and update start
			// position to move past newline
			s.step()
			s.start = s.current
		} else if s.checkAndStep('*') {
			// keep it stepping
			for !s.isAtEnd() {
				if s.peek() == '\n' {
					s.line += 1
				}
				s.step()

				if s.peek() == '*' {
					if s.peekNext() == '/' {
						s.step()
						break
					}
				}
			}

			if s.isAtEnd() {
				gloxError.Error(s.line, "Unterminated block comment.")
				return
			}

			// currently at the '/' end of newline char, step again and update
			// start position to move past it.
			s.step()
			s.start = s.current
		} else {
			s.addToken(token.QUO)
		}

		// string literals
	case '"':
		s.scanString()

	default:
		if s.isDigit(char) {
			s.scanFloat()
		} else if s.isAlpha(char) {
			s.scanIdentifier()
		} else {
			gloxError.Error(s.line, "Unexpected character.")
		}
	}

}

func (s *scanner) step() rune {
	s.current += 1
	return rune(s.source[s.current-1])
}

func (s *scanner) checkAndStep(char rune) bool {
	if s.isAtEnd() {
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
	if s.isAtEnd() {
		return '\x00' // null terminated string in go
	}
	return rune(s.source[s.current])
}

func (s *scanner) peekNext() rune {
	if s.current+1 >= len(s.source) {
		return '\x00'
	}
	return rune(s.source[s.current+1])
}

func (s *scanner) addToken(tokType token.TokenType) {
	s.addTokenWithLiteral(tokType, struct{}{})
}

func (s *scanner) addTokenWithLiteral(
	tokType token.TokenType,
	literal interface{},
) {
	text := s.source[s.start:s.current]
	tok := token.Token{
		TokType: tokType,
		Lexeme:  text,
		Literal: literal,
		Line:    s.line,
	}
	s.tokens = append(s.tokens, tok)
}

func (s *scanner) scanString() {
	for s.peek() != '"' && !s.isAtEnd() {
		if s.peek() == '\n' {
			s.line += 1
		}
		s.step()
	}

	if s.isAtEnd() {
		gloxError.Error(s.line, "Unterminated string.")
		return
	}

	// step past the end of the string to the newline char.
	s.step()

	// trim the surrounding quotes
	value := s.source[s.start+1 : s.current-1]
	s.addTokenWithLiteral(token.STRING, value)
}

func (s *scanner) scanFloat() {
	for s.isDigit(s.peek()) {
		s.step()
	}

	// look for the fractional part of the number
	if s.peek() == '.' && s.isDigit(s.peekNext()) {
		s.step() // consume the '.'
	}

	for s.isDigit(s.peek()) {
		s.step()
	}

	value := s.source[s.start:s.current]
	floatVal, err := strconv.ParseFloat(value, 64)
	if err != nil {
		gloxError.Error(s.line, "Could not parse string value.")
		return
	}
	s.addTokenWithLiteral(token.FLOAT, floatVal)
}

func (s *scanner) scanIdentifier() {
	for s.isAlphaNumeric(s.peek()) {
		s.step()
	}

	value := s.source[s.start:s.current]
	tok := token.Lookup(value)
	s.addToken(tok)
}
