package main

import (
	"fmt"
	"os"
)

func Must[T any](arg T, err error) T {
	if err != nil {
		panic(err)
	}
	return arg
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("need exactly one argument")
		os.Exit(1)
	}

	compileAndRunFile(os.Args[1], true)
}
