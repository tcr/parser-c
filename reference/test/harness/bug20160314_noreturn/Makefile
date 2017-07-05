PP=../../../examples/ParseAndPrint
TY=../../../examples/TypeCheck
all: test
test:
	$(PP) test.c > test_pp.out 2>&1
	diff -u test_pp.expect test_pp.out
	$(TY) test.c > test_ty.out 2>&1
	diff -u test_ty.expect test_ty.out
clean:
	rm -f *.out
