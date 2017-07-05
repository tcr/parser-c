SCANFILE=../../../examples/ScanFile
PROG_C_1=static_extern.c other.c
PROG_CS=scopes.c
ERROR_CS=error_extern_init.c error_extern_no_linkage_1.c error_extern_no_linkage_2.c error_inner_scope_duple_def.c

export TRACE_EVENTS=1

all: test
test:
	for f in $(PROG_C_1); do $(SCANFILE) $$f; done
	for f in $(PROG_CS); do $(SCANFILE) $$f; done
	for f in $(ERROR_CS); do ( $(SCANFILE) $$f; sh ../expect_error $$? "Expecting error when analysing $$f"); done	
test-gcc:
	gcc -o prog $(PROG_C_1)
	gcc -c $(PROG_CS)
	for f in $(ERROR_CS); do (gcc -c $$f 2>.log ; sh ../expect_error $$? `cat .log`); done
clean:
