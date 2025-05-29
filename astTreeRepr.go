package main

import (
	"fmt"
	"reflect"
)

func AstTreeFormat(node Expr) string {

	if node == nil {
		return "<nil>" // this branch is a compiler bug. Crashing would be more helpful to fix it than this.
	}
	builder := &AstPrettyPrinter{}
	m := make(map[uintptr]int)
	astTreeFormat(builder, m, reflect.ValueOf(node))
	return filterOutEmptyLines(builder.String())
}

func isEmpty(node reflect.Value) bool {
	switch node.Kind() {
	case reflect.Invalid:
		return true
	case reflect.Bool, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr, reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128:
		return false
	case reflect.Array, reflect.String:
		return node.Len() == 0
	case reflect.Chan:
		return false // ???
	case reflect.Func:
		return false // ???
	case reflect.Map:
		return false // ???
	case reflect.Interface:
		return node.IsNil() || isEmpty(node.Elem())
	case reflect.Pointer, reflect.UnsafePointer:
		return node.IsNil()
	case reflect.Slice:
		return node.Len() == 0
	case reflect.Struct:
		return false
	}
	return false
}

func astTreeFormat(builder *AstPrettyPrinter, visitedNodes map[uintptr]int, node reflect.Value) {
	switch node.Kind() {
	case reflect.Invalid:
		builder.WriteString("<invalid>")
	case reflect.Bool, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr, reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128, reflect.String:
		fmt.Fprintf(builder, "%v", node)
	case reflect.Array:
		fmt.Fprintf(builder, "not implemented %v", node.Kind())
	case reflect.Chan:
		fmt.Fprintf(builder, "not implemented %v", node.Kind())
	case reflect.Func:
		fmt.Fprintf(builder, "not implemented %v", node.Kind())
	case reflect.Map:
		fmt.Fprintf(builder, "not implemented %v", node.Kind())
	case reflect.Interface:
		// builder.WriteString(" ->")
		// if idx, ok := visitedNodes[node.Pointer()]; ok {
		// 	fmt.Fprintf(builder, "visited on Line %d", idx+1)
		// } else {
		// 	visitedNodes[node.Pointer()] = builder.LineIdx
		astTreeFormat(builder, visitedNodes, node.Elem())
		//}
	case reflect.Pointer:
		// builder.WriteString(" ->")
		if idx, ok := visitedNodes[node.Pointer()]; ok {
			fmt.Fprintf(builder, "visited on Line %d", idx+1)
		} else {
			visitedNodes[node.Pointer()] = builder.LineIdx
			astTreeFormat(builder, visitedNodes, node.Elem())
		}

	case reflect.Slice:
		//builder.WriteString("[")
		builder.Indentation += 1
		for i := range node.Len() {
			value := node.Index(i)
			builder.NewlineAndIndent()
			astTreeFormat(builder, visitedNodes, value)
		}
		builder.Indentation -= 1
		if node.Len() > 0 {
			builder.NewlineAndIndent()
		}
		//builder.WriteString("]")

	case reflect.Struct:
		builder.WriteString(node.Type().Name())
		// builder.WriteString("{")
		builder.Indentation += 1
		for i := range node.NumField() {
			value := node.Field(i)
			if isEmpty(value) {
				continue
			}
			name := node.Type().Field(i).Name
			builder.NewlineAndIndent()
			builder.WriteString(name)
			builder.WriteString(": ")
			astTreeFormat(builder, visitedNodes, value)
		}
		builder.Indentation -= 1
		if node.NumField() > 0 {
			builder.NewlineAndIndent()
		}
		// builder.WriteString("}")
	case reflect.UnsafePointer:
		fmt.Fprintf(builder, "not implemented %v", node.Kind())
	}

}
