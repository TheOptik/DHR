.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

SWIPL := swipl

version:
	@echo $(version)

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

test.separate:
	@find ./test/examples -type f -exec $(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s {} \;
