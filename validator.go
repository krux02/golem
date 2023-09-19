package main

import (
	"fmt"
	"reflect"
	"unsafe"
)

// joins str1 and str2 within parent. Both str1 and str2 must be
// direct substrings in parent. Does not allocate a new string object
func joinSubstr(parent, str1, str2 string) string {
	//header1 := (*reflect.StringHeader)(unsafe.Pointer(&str1))
	//header2 := (*reflect.StringHeader)(unsafe.Pointer(&str2))
	data1 := uintptr(unsafe.Pointer(unsafe.StringData(str1)))
	data2 := uintptr(unsafe.Pointer(unsafe.StringData(str2)))

	// do rage checks
	if data1 == data2 && len(str1) == len(str2) {
		return str1
	}

	if data2 <= data1 {
		fmt.Printf("context: %#v\n", parent)
		panic(fmt.Sprintf("illegal argument order of %#v and %#v", str1, str2))
	}

	begin := uintptr(unsafe.Pointer(unsafe.StringData(parent)))
	end := begin + uintptr(len(parent))

	if data1 < begin || end < (data1+uintptr(len(str1))) {
		panic("str1 out of range")
	}
	if data2 < begin || end < (data2+uintptr(len(str2))) {
		panic("str1 out of range")
	}

	return parent[int(data1-begin) : int(data2-begin)+len(str2)]
}

// ensure recursively that all nodes have `source` set, and that
// `source` is substring of outer string. Purely a debugging function.
// TODO use this fucntion again, currently deactivated
func validateSourceSet(code string, pak AstNode) {
	validateSourceSetInternal(code, reflect.ValueOf(pak))
}

func validateSourceSetInternal(code string, node reflect.Value) {
	if node.Kind() == reflect.Invalid {
		return
	}
	typ := node.Type()
	switch node.Kind() {
	case reflect.Ptr:
		validateSourceSetInternal(code, node.Elem())
	case reflect.String, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64, reflect.Bool, reflect.Float32, reflect.Float64:
		// skipping
	case reflect.Struct:
		// direct ast node
		// validateSourceSetInternal(code, fieldValue)
		name := typ.Name()
		if name == "BuiltinType" {
			return
		}
		if typ.Field(0).Name != "Source" {
			panic(fmt.Errorf("type %s expected field `Source` at the beginning", typ.Name()))
		}
		if name == "IntLit" {
			// this would be eteral recursion as long as the typo of an IntLit is the IntLit itself
			return
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
		panic(fmt.Errorf("not implemented: %s: %s", typ.Name(), node.Kind()))
	}

}
