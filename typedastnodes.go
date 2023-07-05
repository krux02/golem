package main

import (
	"strings"
)

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	PrettyPrint(*AstPrettyPrinter)
	ManglePrint(*strings.Builder) // print for name magling
	DefaultValue(tc *TypeChecker, context AstNode) TcExpr
	AppendToGroup(*TypeGroupBuilder) bool // return true if the type group became `TypeUnspecified`
	TypeName() string                     // no generic arguments here
}

type TcExpr interface {
	AstNode
	GetType() Type
}

// node to wrap code that can't be typechecked
type TcErrorNode struct {
	SourceNode Expr
}

type TcStructField struct {
	Source string
	Name   string
	Type   Type
}

type TcEnumDef struct {
	Source string
	Name   string
	Values []TcSymbol
}

type TcStructDef struct {
	Source string
	Name   string
	Fields []TcStructField
}

// TODO: rename to GenericParamType
type TcGenericTypeParam struct {
	Source     string
	Name       string
	Constraint Type // TypeGroup or TypeUnspecified
}

type TcCodeBlock struct {
	Source string
	Items  []TcExpr
}

// TODO unify TcProcSym with the othe symbol types
// TODO maybe Store Signature instead of Impl
type TcProcSymbol struct {
	Source string
	Impl   *TcProcDef
}

type SymbolKind int

// not the type or properties of a symbol, just where it has been declared
const (
	SkInvalid SymbolKind = iota
	SkLet
	SkVar
	SkConst
	SkProcArg
	SkLoopIterator
	SkEnum
)

type TcSymbol struct {
	Source string
	Kind   SymbolKind
	Type   Type
}

type TcForLoopStmt struct {
	Source     string
	LoopSym    TcSymbol
	Collection TcExpr
	Body       TcExpr
}

type TcIfStmt struct {
	Source    string
	Condition TcExpr
	Body      TcExpr
}

type TcIfElseExpr struct {
	Source    string
	Condition TcExpr
	Body      TcExpr
	Else      TcExpr
}

type TcDotExpr struct {
	Source string
	Lhs    TcExpr
	Rhs    TcStructField
}

type TcCall struct {
	Source string
	Sym    TcProcSymbol
	Args   []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcVariableDefStmt struct {
	Source string
	Sym    TcSymbol
	Value  TcExpr
}

type TcReturnStmt struct {
	Source string
	Value  TcExpr
}

// Not an Ast node, but part of the ast as a normal member, therefore not the Tc prefix
type ProcSignature struct {

	// type placeholders within this procedure that need to be substituted with
	// real types in generic instantiations.
	GenericParams []Type
	Params        []TcSymbol
	ResultType    Type

	// NOTE: Varargs and Validator is currently only used for printf.
	Varargs   bool
	Validator PostResolveValidator
	// TODO signature will be the identical data structure for ProcDef, TemplateDef and MacroDef,
	// Impl pointing to TcProcDef is temporaray
	Impl *TcProcDef
}

type TcProcDef struct {
	Source    string
	Name      string
	Signature ProcSignature
	Body      TcExpr

	// TODO these are C backend specific fields and should not be bart of a general proc def node
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator call
	// example3 "somearray[", ", ", "]"  array access
	Prefix, Infix, Postfix string

	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this proc as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

type TcArrayLit struct {
	Source   string
	Items    []TcExpr
	ElemType Type
}

type TcStructLit struct {
	Source string
	Items  []TcExpr
	Type   *StructType
}

type TcEnumSetLit struct {
	Source   string
	Items    []TcExpr
	ElemType *EnumType
}

type TcPackageDef struct {
	Source     string
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

func (typ *StructType) TypeName() string         { return typ.Impl.Name }
func (typ *EnumType) TypeName() string           { return typ.Impl.Name }
func (typ *EnumSetType) TypeName() string        { return "EnumSet" }
func (typ *TypeGroup) TypeName() string          { return "TypeGroup" }
func (typ *BuiltinType) TypeName() string        { return typ.Name }
func (typ *ArrayType) TypeName() string          { return "Array" }
func (typ *TcGenericTypeParam) TypeName() string { return typ.Source }

func (arg TcErrorNode) GetSource() string       { return arg.SourceNode.GetSource() }
func (arg TcDotExpr) GetSource() string         { return arg.Source }
func (arg TcSymbol) GetSource() string          { return arg.Source }
func (arg TcProcSymbol) GetSource() string      { return arg.Source }
func (arg TcVariableDefStmt) GetSource() string { return arg.Source }
func (arg TcReturnStmt) GetSource() string      { return arg.Source }
func (arg TcForLoopStmt) GetSource() string     { return arg.Source }
func (arg TcIfStmt) GetSource() string          { return arg.Source }
func (arg TcIfElseExpr) GetSource() string      { return arg.Source }
func (arg TcCodeBlock) GetSource() string       { return arg.Source }
func (arg TcCall) GetSource() string            { return arg.Source }
func (arg TcArrayLit) GetSource() string        { return arg.Source }
func (arg TcEnumSetLit) GetSource() string      { return arg.Source }
func (arg TcProcDef) GetSource() string         { return arg.Source }
func (arg TcStructLit) GetSource() string       { return arg.Source }

func (arg TcPackageDef) GetSource() string { return arg.Source }

func (arg *TcStructDef) GetSource() string        { return arg.Source }
func (arg *TcEnumDef) GetSource() string          { return arg.Source }
func (arg *TcGenericTypeParam) GetSource() string { return arg.Source }

// func (arg *EnumSetType) GetSource() string { return arg.Source }
// func (arg TypeGroup) GetSource() string    { return arg.Source }
// func (arg *BuiltinType) GetSource() string { return arg.Source }
// func (arg *ArrayType) GetSource() string   { return arg.Source }
