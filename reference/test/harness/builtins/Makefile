PP=../../../examples/ParseAndPrint
TY=../../../examples/TypeCheck
all: parse
# typecheck not fully supported yet

parse:
	$(PP) test.c > test.out
	cat test.c | grep -v '^/\*' | grep -v '^$$' > test.expect
	diff -u test.expect test.out
typecheck:
	$(TY) test.c 2>&1 | tee test_ty.out
	diff -u test_ty.expect test_ty.out
clean:
	rm -f *.out
