PP=../../../examples/ParseAndPrint
TY=../../../examples/TypeCheck
all:
	$(TY) test.i > test.out 2>&1
	diff -u test.expect test.out
preprocess:
	$(CC) -E test.c > test.i
check_host_cc_build:
	$(CC) $(CFLAGS) -o /dev/null test.c

clean:
	rm -f *.out
