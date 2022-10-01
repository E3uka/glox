package scanner

import (
	gloxError "glox/error"
	"glox/token"
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
		line:    1, // beginning line of the file
	}
}

func (s scanner) isAtEnd() bool {
	return s.current >= len(s.source)
}

func (s *scanner) ScanTokens() []token.Token {
	for !s.isAtEnd() {
		s.Scan()
	}

	eof := token.Token{
		TokType: token.EOF,
		Lexeme:  "",
		Literal: struct{}{},
		Line:    s.line,
	}
	s.tokens = append(s.tokens, eof)

	return s.tokens
}

func (s *scanner) Scan() {
	char := s.step()

	switch char {

	// ignore the following chars
	case ' ':
	case '\r':
	case '\t':

	//
	case '\n':
		s.line += 1

	// single character lexemes
	case '(':
		s.addToken(token.LPAREN)
	case ')':
		s.addToken(token.RPAREN)
	case '{':
		s.addToken(token.LBRACE)
	case '}':
		s.addToken(token.RBRACE)
	case ',':
		s.addToken(token.COMMA)
	case '.':
		s.addToken(token.PERIOD)
	case '-':
		s.addToken(token.SUB)
	case '+':
		s.addToken(token.ADD)
	case ';':
		s.addToken(token.SEMICOLON)
	case '*':
		s.addToken(token.MUL)

	// double-character lexemes
	case '!':
		if s.stepAndCheck('=') {
			s.addToken(token.NEQ)
		} else {
			s.addToken(token.NOT)
		}
	case '=':
		if s.stepAndCheck('=') {
			s.addToken(token.EQL)
		} else {
			s.addToken(token.ASSIGN)
		}
	case '<':
		if s.stepAndCheck('=') {
			s.addToken(token.LEQ)
		} else {
			s.addToken(token.LSS)
		}
	case '>':
		if s.stepAndCheck('=') {
			s.addToken(token.GEQ)
		} else {
			s.addToken(token.GTR)
		}
	case '/':
		if s.stepAndCheck('/') {
			// comments go the to end of the line
			for !s.isAtEnd() && s.peek() != '\n' {
				s.step()
			}
			s.addToken(token.GEQ)
		} else {
			s.addToken(token.QUO)
		}

		// string literals
	case '"':
		s.scanString()

	default:
		gloxError.Error(s.line, "Unexpected character.")
	}

}

func (s *scanner) step() rune {
	next := s.current + 1
	return rune(s.source[next])
}

func (s *scanner) stepAndCheck(char rune) bool {
	if s.isAtEnd() {
		return false
	} else if rune(s.source[s.current+1]) != char {
		return false
	} else {
		s.current += 1
		return true
	}
}

func (s *scanner) peek() rune {
	if s.isAtEnd() {
		return '\n'
	}
	return rune(s.source[s.current])
}

func (s *scanner) addToken(token token.TokenType) {
	s.addTokenWithLiteral(token, struct{}{})
}

func (s *scanner) addTokenWithLiteral(
	tok token.TokenType,
	literal interface{},
) {
	text := s.source[s.start : s.current-s.start]
	token := token.Token{
		TokType: tok,
		Lexeme:  text,
		Literal: literal,
		Line:    s.line,
	}
	s.tokens = append(s.tokens, token)
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

	// Trim the surrounding quotes
	value := s.source[s.start+1 : s.current-1]
	s.addTokenWithLiteral(token.STRING, value)
}
