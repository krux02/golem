package main

import (
	"strings"
)

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	AstNode
	ManglePrint(*strings.Builder) // print for name magling
	DefaultValue(tc *TypeChecker, context AstNode) TcExpr
	typenode()
}

type TcExpr interface {
	AstNode
	Type() Type
}

type TcStructField struct {
	AbstractAstNode
	Name string
	Type Type
}

type TcEnumDef struct {
	AbstractAstNode
	Name   string
	Values []TcSymbol

	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this type as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

type TcStructDef struct {
	AbstractAstNode
	Name   string
	Fields []TcStructField

	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this type as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

func (structDef *TcStructDef) GetField(name string) (resField TcStructField, idx int) {
	for i, field := range structDef.Fields {
		if field.Name == name {
			return field, i
		}
	}
	return TcStructField{}, -1
}

type TcCodeBlock struct {
	AbstractAstNode
	Items []TcExpr
}

// TODO unify TcProcSym with the othe symbol types

type TcProcSymbol struct {
	AbstractAstNode
	Name string
	Impl *TcProcDef
}

type SymbolKind int

// not the type or properties of a symbol, just where it has been declared
const (
	SkLet SymbolKind = iota
	SkVar
	SkConst
	SkProcArg
	SkLoopIterator
	SkEnum
)

type TcSymbol struct {
	AbstractAstNode
	Kind SymbolKind
	Typ  Type
}

type TcForLoopStmt struct {
	AbstractAstNode
	LoopSym    TcSymbol
	Collection TcExpr
	Body       TcExpr
}

type TcIfStmt struct {
	AbstractAstNode
	Condition TcExpr
	Body      TcExpr
}

type TcIfElseExpr struct {
	AbstractAstNode
	Condition TcExpr
	Body      TcExpr
	Else      TcExpr
}

type TcDotExpr struct {
	AbstractAstNode
	Lhs TcExpr
	Rhs TcStructField
}

type TcCall struct {
	AbstractAstNode
	Sym  TcProcSymbol
	Args []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcVariableDefStmt struct {
	AbstractAstNode
	Sym   TcSymbol
	Value TcExpr
}

type TcReturnStmt struct {
	AbstractAstNode
	Value TcExpr
}

type TcProcDef struct {
	AbstractAstNode
	Name string

	// TODO these are C backend specific fields and should not be bart of a general proc def node
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator call
	// example3 "somearray[", ", ", "]"  array access
	Prefix, Infix, Postfix string

	Args       []TcSymbol
	ResultType Type
	Body       TcExpr

	// TODO This code here is supposed to be temporary code. TcProcDef should
	// be a tidy clean minimal definition. This code must be optimized
	// for reading. Do NOT bloat it with shit for lots of of exceptions
	// in the language.
	// Exception for printf. It has varargs that are at this point in
	// time no language feature yet.
	printfargs         bool
	generateAsOperator bool

	// This code here is still supposed to be temporary. But from here on is used
	// for the code generator

	// this value is set to true in the code generator to mark this function as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same function multiple times.
	scheduledforgeneration bool
}

type TcArrayLit struct {
	AbstractAstNode
	Items    []TcExpr
	ElemType Type
}

type TcStructLit struct {
	AbstractAstNode
	Items []TcExpr
	typ   *TcStructDef
}

type TcEnumSetLit struct {
	AbstractAstNode
	Items    []TcExpr
	ElemType *TcEnumDef
}

type TcPackageDef struct {
	AbstractAstNode
	Name       string
	StructDefs []*TcStructDef
	EnumDefs   []*TcEnumDef
	VarDefs    []TcVariableDefStmt
	ProcDefs   []*TcProcDef
	Main       *TcProcDef // main entry point
}

func (sym TcDotExpr) expression()          {}
func (sym TcSymbol) expression()           {}
func (stmt TcVariableDefStmt) expression() {}
func (stmt TcReturnStmt) expression()      {}
func (stmt TcForLoopStmt) expression()     {}
func (stmt TcIfStmt) expression()          {}
func (stmt TcIfElseExpr) expression()      {}
func (block TcCodeBlock) expression()      {}
func (call TcCall) expression()            {}
func (call TcArrayLit) expression()        {}
func (call TcEnumSetLit) expression()      {}
func (expr TcStructLit) expression()       {}

func (typ *TcStructDef) typenode() {}
func (typ *TcEnumDef) typenode()   {}
func (typ *EnumSetType) typenode() {}
func (typ TypeGroup) typenode()    {}
func (typ *BuiltinType) typenode() {}
func (typ *ArrayType) typenode()   {}
