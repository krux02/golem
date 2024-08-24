package main

import (
	"fmt"
	"reflect"
	//"strings"
)

func (builder *AstPrettyPrinter) DebugNodeFormat(visitedNodes map[uintptr]int, node reflect.Value) {
	switch node.Kind() {
	case reflect.Ptr:
		p := node.Pointer()
		if oldIdx, visited := visitedNodes[p]; visited {
			fmt.Fprintf(builder, "ref %d", oldIdx)
			// builder.WriteString("...")
		} else {
			if p == 0 {
				builder.WriteString("<nil>")
			} else {
				visitedNodes[p] = builder.LineIdx
				builder.DebugNodeFormat(visitedNodes, node.Elem())
			}
		}
	case reflect.String:
		builder.WriteString(node.String())
	case reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64, reflect.Int:
		WriteIntLit(&builder.Builder, node.Int())
	case reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uint:
		WriteUIntLit(&builder.Builder, node.Uint())
	case reflect.Float32, reflect.Float64:
		fmt.Fprintf(&builder.Builder, "%f", node.Float())
	case reflect.Bool:
		if node.Bool() {
			builder.WriteString("true")
		} else {
			builder.WriteString("false")
		}
	case reflect.Struct:
		typ := node.Type()
		typeName := typ.Name()

		if typeName == "Ident" {
			builder.WriteString(typeName)
			builder.WriteString(" ")
			builder.WriteString(node.FieldByName("Source").String())
			return
		}

		builder.WriteString(typeName)
		N := node.NumField()
		builder.Indentation += 1
		for i := 0; i < N; i++ {
			field := typ.Field(i)
			value := node.Field(i)

			// skip default values
			switch field.Type.Kind() {
			case reflect.Bool:
				if value.Bool() == false {
					continue
				}
			case reflect.String:
				if value.String() == "" {
					continue
				}
			case reflect.Slice:
				if value.Len() == 0 {
					continue
				}
			case reflect.Interface, reflect.Pointer:
				if value.IsNil() {
					continue
				}
			}

			fieldName := field.Name

			builder.NewlineAndIndent()
			builder.WriteString(fieldName)
			builder.WriteString(": ")
			builder.DebugNodeFormat(visitedNodes, value)
			//value.IsValid()
			//value.IsNil()
			value.IsZero()
		}
		builder.Indentation -= 1
	case reflect.Interface:
		// builder.WriteString("i{")
		builder.DebugNodeFormat(visitedNodes, node.Elem())
		// builder.WriteString("}")
	case reflect.Slice:
		N := node.Len()
		if N == 1 {
			builder.DebugNodeFormat(visitedNodes, node.Index(0))
		} else {
			builder.Indentation += 1
			for i := 0; i < N; i++ {
				builder.NewlineAndIndent()
				builder.DebugNodeFormat(visitedNodes, node.Index(i))
			}
			builder.Indentation -= 1
		}
	case reflect.Invalid:
		builder.WriteString("<invalid>")
	default:
		panic(fmt.Errorf("not implemented: %s", node.Kind()))
	}
}

func DebugAstFormat(node AstNode) string {
	printer := &AstPrettyPrinter{}
	printer.DebugNodeFormat(map[uintptr]int{}, reflect.ValueOf(node))
	return printer.String()
}
