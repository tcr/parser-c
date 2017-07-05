PP=../../../examples/ParseAndPrint
all:
	$(PP) test.c > test.out
	diff -u test.expect test.out
clean:
	rm -f test.out