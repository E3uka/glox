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
		tokens:  []token.Token{},
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

func (l *lexer) isAtEnd() bool {
	return l.current >= len(l.source)
}

func (l lexer) isDigit(char rune) bool {
	return char >= '0' && char <= '9'
}

func (l lexer) isAlpha(char rune) bool {
	return (char >= 'a' && char <= 'z') ||
		(char >= 'A' && char <= 'Z') ||
		char == '_'
}

func (l lexer) isAlphaNumeric(char rune) bool {
	return l.isAlpha(char) || l.isDigit(char)
}

func (l *lexer) init() error {
	var err error

	for !l.isAtEnd() {
		l.start = l.current
		err = l.lex()
		if err != nil {
			return err
		}
	}
	eof := token.Token{
		TokType: token.EOF,
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
		l.addToken(token.LPAREN)
	case ')':
		l.addToken(token.RPAREN)
	case '[':
		l.addToken(token.LBRACK)
	case ']':
		l.addToken(token.RBRACK)
	case '{':
		l.addToken(token.LBRACE)
	case '}':
		l.addToken(token.RBRACE)
	case ',':
		l.addToken(token.COMMA)
	case '.':
		l.addToken(token.PERIOD)
	case ';':
		l.addToken(token.SEMICOLON)
	case '*':
		l.addToken(token.MUL)

	// double-character lexemes
	case '+':
		if l.checkAndStep('+') {
			l.addToken(token.INCR)
		} else if l.checkAndStep('=') {
			l.addToken(token.INCRBY)
		} else {
			l.addToken(token.ADD)
		}
	case '-':
		if l.checkAndStep('>') {
			l.addToken(token.FUNRET)
		} else if l.checkAndStep('-') {
			l.addToken(token.DECR)
		} else if l.checkAndStep('=') {
			l.addToken(token.DECRYBY)
		} else {
			l.addToken(token.SUB)
		}
	case '!':
		if l.checkAndStep('=') {
			l.addToken(token.NEQ)
		} else {
			l.addToken(token.NOT)
		}
	case '=':
		if l.checkAndStep('=') {
			l.addToken(token.EQL)
		} else {
			l.addToken(token.ASSIGN)
		}
	case '<':
		if l.checkAndStep('=') {
			l.addToken(token.LEQ)
		} else {
			l.addToken(token.LSS)
		}
	case '>':
		if l.checkAndStep('=') {
			l.addToken(token.GEQ)
		} else {
			l.addToken(token.GTR)
		}
	case ':':
		if l.checkAndStep('=') {
			l.addToken(token.WALRUS)
		} else if l.checkAndStep(':') {
			l.addToken(token.MODASSIGN)
		} else {
			l.addToken(token.COLON)
		}
	case '&':
		if l.checkAndStep('&') {
			l.addToken(token.AND)
		} else {
			l.addToken(token.BITAND)
		}
	case '|':
		if l.checkAndStep('|') {
			l.addToken(token.OR)
		} else {
			l.addToken(token.BITOR)
		}
	case '/':
		if l.checkAndStep('/') {
			// step past all remaining characters until newline
			for l.peek() != '\n' && !l.isAtEnd() {
				l.step()
			}
			// currently at the newline char, step again and update start
			// position to move past newline
			l.step()
			l.start = l.current
		} else if l.checkAndStep('*') {
			// keep it stepping
			for !l.isAtEnd() {
				if l.peek() == '\n' {
					l.line += 1
				}
				l.step()

				if l.peek() == '*' {
					if l.peekNext() == '/' {
						l.step()
						break
					}
				}
			}

			if l.isAtEnd() {
				return gloxError.LexError(l.path, l.line, "unterminated block comment")
			}

			// currently at the '/' end of newline char, step again and update
			// start position to move past it.
			l.step()
			l.start = l.current
		} else {
			l.addToken(token.QUO)
		}

		// string literals
	case '"':
		l.lexString()

	default:
		if l.isDigit(char) {
			l.lexFloat()
		} else if l.isAlpha(char) {
			l.lexIdentifier()
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

func (l *lexer) checkAndStep(char rune) bool {
	if l.isAtEnd() {
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
	if l.isAtEnd() {
		return '\x00' // null terminated string in go
	}
	return rune(l.source[l.current])
}

func (l *lexer) peekNext() rune {
	if l.current+1 >= len(l.source) {
		return '\x00'
	}
	return rune(l.source[l.current+1])
}

func (l *lexer) addToken(tokType token.TokenType) {
	l.addTokenWithLiteral(tokType, struct{}{})
}

func (l *lexer) addTokenWithLiteral(
	tokType token.TokenType,
	literal interface{},
) {
	text := l.source[l.start:l.current]
	tok := token.Token{
		TokType: tokType,
		Lexeme:  text,
		Literal: literal,
		Line:    l.line,
	}
	l.tokens = append(l.tokens, tok)
}

func (l *lexer) lexString() error {
	for l.peek() != '"' && !l.isAtEnd() {
		if l.peek() == '\n' {
			l.line += 1
		}
		l.step()
	}

	if l.isAtEnd() {
		return gloxError.LexError(l.path, l.line, "unterminated string")

	}

	// step past the end of the string to the newline char
	l.step()

	// trim the surrounding quotes
	value := l.source[l.start+1 : l.current-1]
	l.addTokenWithLiteral(token.STRING, value)
	return nil
}

func (l *lexer) lexFloat() error {
	for l.isDigit(l.peek()) {
		l.step()
	}

	// look for the fractional part of the number
	if l.peek() == '.' && l.isDigit(l.peekNext()) {
		l.step() // consume the '.'
	}

	for l.isDigit(l.peek()) {
		l.step()
	}

	value := l.source[l.start:l.current]
	floatVal, err := strconv.ParseFloat(value, 64)
	if err != nil {
		return gloxError.LexError(l.path, l.line, "could not parse string value")
	}
	l.addTokenWithLiteral(token.FLOAT, floatVal)
	return nil
}

func (l *lexer) lexIdentifier() {
	for l.isAlphaNumeric(l.peek()) {
		l.step()
	}

	value := l.source[l.start:l.current]
	tok := token.Lookup(value)
	l.addToken(tok)
}
