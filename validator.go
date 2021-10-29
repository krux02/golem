package main

import (
	"fmt"
	"reflect"
)

// ensure recursively that all nodes have `source` set, and that
// `source` is substring of outer string.

func validateSourceSet(code string, pak AstNode) {
	validateSourceSetInternal(code, reflect.ValueOf(pak))
}

func shortenMessage(msg string, maxLen int) string {
	if len(msg) > maxLen {
		return msg[:maxLen]
	}
	return msg
}

func validateSourceSetInternal(code string, node reflect.Value) {
	typ := node.Type()
	switch node.Kind() {
	case reflect.String, reflect.Int, reflect.Bool, reflect.Int32:
		// skipping
	case reflect.Struct:
		// direct ast node
		// validateSourceSetInternal(code, fieldValue)
		if typ.Field(0).Type.Field(0).Name != "source" {
			panic("expected field `source` here")
		}
		sourceString := node.Field(0).Field(0).String()
		msg := fmt.Sprintf("%s(source: %#v)", typ.Name(), sourceString)
		fmt.Println(shortenMessage(msg, 80))
		N := node.NumField()
		for i := 1; i < N; i++ {
			validateSourceSetInternal(code, node.Field(i))
		}
	case reflect.Interface:
		tmp := reflect.ValueOf(node.Interface())
		validateSourceSetInternal(code, tmp)
	case reflect.Slice:
		N := node.Len()
		for i := 0; i < N; i++ {
			validateSourceSetInternal(code, node.Index(i))
		}
	default:
		fmt.Println(typ.Name, ": ", node.Kind())
		panic("not implemented")
	}

}
