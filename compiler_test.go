package main

import (
	"testing"
)

func TestMain(t *testing.T) {
	tests := globForTests()
	if len(tests) == 0 {
		t.Errorf("No tests found\n")
		return
	}
	runTests(t, tests)
}
