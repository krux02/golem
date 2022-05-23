package main

import (
	"fmt"
	"reflect"
	"unsafe"
)

// joins str1 and str2 within parent. Both str1 and str2 must be
// direct substrings in parent. Does not allocate a new string object
func joinSubstr(parent, str1, str2 string) (result string) {
	header1 := (*reflect.StringHeader)(unsafe.Pointer(&str1))
	header2 := (*reflect.StringHeader)(unsafe.Pointer(&str2))

	// do rage checks
	if *header1 == *header2 {
		return str1
	}

	if header2.Data <= header1.Data {
		panic(fmt.Sprintf("illegal argument order of %#v and %#v", str1, str2))
	}

	header0 := (*reflect.StringHeader)(unsafe.Pointer(&parent))
	begin := header0.Data
	end := header0.Data + uintptr(header0.Len)

	if header1.Data < begin || end < (header1.Data+uintptr(header1.Len)) {
		panic("str1 out of range")
	}
	if header2.Data < begin || end < (header2.Data+uintptr(header2.Len)) {
		panic("str1 out of range")
	}

	resultHeader := (*reflect.StringHeader)(unsafe.Pointer(&result))
	resultHeader.Data = header1.Data
	resultHeader.Len = int(header2.Data-header1.Data) + header2.Len
	return
}

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
	if node.Kind() == reflect.Invalid {
		return
	}
	typ := node.Type()
	switch node.Kind() {
	case reflect.String, reflect.Int, reflect.Bool, reflect.Int32, reflect.Float64:
		// skipping
	case reflect.Struct:
		// direct ast node
		// validateSourceSetInternal(code, fieldValue)
		if typ.Field(0).Type.Field(0).Name != "source" {
			panic("expected field `source` here")
		}
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
