package parser

import (
	"fmt"
	"glox/ast"
	"glox/token"
	g_err "glox/error"
)

type parser struct {
	path    *string
	tokens  []token.Token
	current int
	expr    ast.Expr
	trace   bool
}

func New(path *string, tokens *[]token.Token) *parser {
	parser := &parser{path: path, tokens: *tokens, current: 0, expr: nil, trace: true}
	return parser
}

func (p *parser) Parse() []ast.Node {
	var nodes []ast.Node
	for !p.is_at_end() {
		// wrapped in anonymous function so block is called as unit - enables
		// panic recovery to occur mutiple times instead of short-circuiting on
		// first capture; recovery advances to start of next viable Node token,
		// the operation continues from that point
		func() {
			defer func() {
				if r := recover(); r != nil {
					g_err.ParsePanicRecover(fmt.Sprint(r))
					recover_and_sync(p)
				}
			}()
			// Safety: panics
			// parse with lower precedence than will encounter during recursive
			// parse operation to ensure all latter Nodes will be of higher
			// precedence (binding power) and thus accepted
			node := p.parse_node(INIT)
			nodes = append(nodes, node)
			p.trace_node("PARSED NODE", node)
		}()
	}
	return nodes
}

func recover_and_sync(parser *parser) {
	next_tok := parser.advance()
	for !parser.is_at_end() {
		// step past end Node boundary ';' to continue parse operation
		if next_tok.Type == token.SEMICOLON {
			parser.expect(token.SEMICOLON)
			return
		}
		// potential beginning Node boundaries (if formatted correctly)
		switch parser.peek().Type {
		case token.CLASS, token.STRUCT, token.FOR, token.IF, token.CONST,
			token.RETURN, token.WHILE, token.FUNASSIGN:
			return
		}
		// advance until boundary is found
		next_tok = parser.advance()
	}
}

/* PARSE HELPERS */

func (p *parser) advance() token.Token { p.current++; return p.peek() }
func (p *parser) peek() token.Token { return p.tokens[p.current] }
func (p *parser) is_at_end() bool { return p.peek().Type == token.EOF }
func (p *parser) expect(tok token.TokenType) {
	if p.peek().Type != tok { p.report_expect_error(p.peek(), tok, "expected '%v'") }
	p.advance()
}

/* PANIC REPORTERS */

func (p *parser) report_parse_error(token token.Token, format_string string) {
	msg := fmt.Sprintf(format_string, token)
	g_err.ParsePanic(p.path, token.Line, msg)
}
func (p *parser) report_offset_parse_error(offset int, format_string string) {
	index := p.current + offset
	tok_at_idx := p.tokens[index]
	msg := fmt.Sprintf(format_string, tok_at_idx)
	g_err.ParsePanic(p.path, tok_at_idx.Line, msg)
}
func (p *parser) report_expect_error(
	token token.Token,
	expected token.TokenType,
	format_string string,
) {
	msg := fmt.Sprintf(format_string, expected)
	g_err.ParsePanic(p.path, token.Line, msg)
}

/* TRACE HELPERS */

func (p *parser) trace_node(s string, node ast.Node) {
	if p.trace { fmt.Printf("\n--- %v: [%#v] ---\n\n", s, node) }
}
func (p *parser) trace_nd(nd_ident string, t token.Token) {
	if p.trace { fmt.Printf("%v: cur: %v, next: %v\n", nd_ident, t.Lit, p.peek()) }
}
func (p *parser) trace_ld(ld_ident string, tt token.TokenType) {
	if p.trace { fmt.Printf("%v: operator: %v\n", ld_ident, tt) }
}
func (p *parser) trace_head(prior_precedence precedence, cur_tok token.Token) {
	if p.trace {
		fmt.Printf(
			"p_head: prev_prec %v, cur_prec: %v, cur_tok: %v, next_tok: %v\n",
			prior_precedence,
			prec_map[cur_tok.Type],
			cur_tok,
			p.peek().Type,
		)
	}
}

// Top Down Operator Precedence Algorithm - Vaughan R. parser, 1973
// https://dl.acm.org/doi/10.1145/512927.512931
func (p *parser) parse_node(prior_precedence precedence) ast.Node {
	var left ast.Node
	cur_tok := p.peek()
	// NOTE: step past the first token to parse the subexpression - this
	// results in later dispatch methods typically not needing to handle
	// their first 'expected' tokens, parsing should assume it is consumed
	p.advance()
	p.trace_head(prior_precedence, cur_tok)
	left = null_deno[cur_tok.Type](p, cur_tok)
	// recursively parse and re-assign both current token (used for future
	// precedence calculations) and the top level expresssion but only if the
	// subexpression has a higher precedence (binding power)
	for !p.is_at_end() && prior_precedence < prec_map[p.peek().Type] {
	INNER:
		cur_tok = p.peek()
		// skip past '}'
		if cur_tok.Type == token.RBRACE {
			cur_tok = p.advance()
			goto INNER
		}
		// terminate at ';'
		if cur_tok.Type == token.SEMICOLON {
			p.advance()
			break
		}
		left = left_deno[cur_tok.Type](p, cur_tok.Type, left)
	}
	return left
}