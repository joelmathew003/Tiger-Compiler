signature Expr_TOKENS =
sig
type ('a,'b) token
type svalue
val DONE:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val TO:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val CLBRACKET:  'a * 'a -> (svalue,'a) token
val OPBRACKET:  'a * 'a -> (svalue,'a) token
val DIVI:  'a * 'a -> (svalue,'a) token
val NEWLINE:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val VAR: (string) *  'a * 'a -> (svalue,'a) token
val CONST: (int) *  'a * 'a -> (svalue,'a) token
end
signature Expr_LRVALS=
sig
structure Tokens : Expr_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
