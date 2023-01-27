package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
)

func main() {

	if len(os.Args) < 2 {
		fmt.Println("not enough arguments")
		flag.PrintDefaults()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]
	switch command {
	case "build":
		buildCmd := flag.NewFlagSet("build", flag.ExitOnError)
		buildCmd.Parse(args)
		absPath, _ := filepath.Abs(args[0])
		compileAndRunFile(absPath, true)
	case "test":
		testCmd := flag.NewFlagSet("test", flag.ExitOnError)
		testCmd.Parse(args)
		runAllTests()
	case "errortest":
		errortest(args[0])
	default:
		fmt.Println("expected 'build', 'test' or 'errortest' subcommands")
		os.Exit(1)
	}

}
