PP=../../../examples/ParseAndPrint
TY=../../../examples/TypeCheck
all:
	$(PP) test.c > test.out
	$(TY) test.c >> test.out
	diff -u test.expect test.out
check_host_cc_build:
	$(CC) $(CFLAGS) -o /dev/null test.c

clean:
	rm -f *.out
