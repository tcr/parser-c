PP=../../../examples/ParseAndPrint
TY=../../../examples/TypeCheck
all: parse typecheck
parse:
	$(PP) test.c > test.out
	diff -u test.expect test.out
typecheck:
	$(TY) test_ty.c >test_ty.out 2>&1 || echo "Typecheck failed as expected"
	diff -u test_ty.expect test_ty.out
clean:
	rm -f *.out
