package main

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	Name() string
}

type TcExpr interface {
	Expr
	Type() Type
}

type TcStructField struct {
	Name string
	Type Type
}

type TcVariableSym struct {
	Name string
	Type Type
}

type TcStructDef struct {
	Name   string
	Fields []TcStructField
}

type TcProcArgument struct {
	Name string
	Type Type
}

type TcCodeBlock struct {
	Items []TcExpr
}

type TcSymbol struct {
	Name string
	typ  Type
}

func (sym TcSymbol) Type() Type {
	return sym.typ
}

type TcProcSymbol struct {
	Name string
	Impl *TcProcDef
}

type TcCall struct {
	Sym  TcProcSymbol
	Args []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcProcDef struct {
	Name       string
	Args       []TcProcArgument
	ResultType Type
	Body       TcExpr
}

type TcPackageDef struct {
	Name     string
	TypeDefs []TcStructDef
	ProcDefs []TcProcDef
}

func (sym TcSymbol) expression()      {}
func (block TcCodeBlock) expression() {}

//func (lit TcStrLit) expression() {}
func (call TcCall) expression() {}
