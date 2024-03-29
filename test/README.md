# TAP Input/Output Test Suite

Definition of input/output tests for the `dhr` package following the [Test Anything Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command, called from the project's root directory:

```shell
swipl -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl
```

This produces a TAP-compatible output like the following:

```
TAP version 13
1..2
ok 1 - gcd(6),gcd(9)
ok 2 - gcd(9),gcd(6)
```

The identifier is the tested call with the mentioned input constraints.

## Define Tests

In the file `test.pl`, all files `examples/*.pl` are consulted. Each of it contains a CHR program and corresponding test cases of the following form:

```prolog
Constraints => Store.
```

Given the comma-separated list of `Constraints`, the program should terminate with the constraint store `Store`. Though the constraint store is compared regardless its order, the input constraints are added in the given order. Test cases prefixed by `~/1` are tested with all permutations of their input constraints. E.g., the previously presented TAP output is generated by the following single test case:

```prolog
~ gcd(9), gcd(6) => gcd(3).
```
