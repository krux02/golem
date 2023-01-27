package main

import (
	"flag"
	"fmt"
	"os"
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
		compileAndRunFile(args[0], true)
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
