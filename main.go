package main

import (
	"os"
)

func main() {
	if len(os.Args) == 2 {
		compileAndRunFile(os.Args[1], true)
	} else if len(os.Args) == 3 && os.Args[1] == "errortest" {
		errortest(os.Args[2])
	} else {
		panic("program needs one argument only, the input file")
	}
}
