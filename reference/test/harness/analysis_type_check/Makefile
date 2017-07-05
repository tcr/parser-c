SCANFILE=../../../examples/ScanFile
CORRECT_CS=good_sinit.c good_sinit2.c good_anonunion.c good_return.c \
           good_bug29.c good_bug29_2.c
ERROR_CS=bad_args.c bad_sinit1.c bad_bug29.c

export TRACE_EVENTS=1

all: test
test:
	@for f in $(CORRECT_CS); do $(SCANFILE) $$f 2> $$f.out; diff $$f.out $$f.expect; done
	@for f in $(ERROR_CS); do ( $(SCANFILE) $$f 2> $$f.out; sh ../expect_error $$? "Expecting error when analysing $$f"); done	
test-gcc:
	@gcc -c $(CORRECT_CS)
	@for f in $(ERROR_CS); do (gcc -c $$f 2>.log ; sh ../expect_error $$? `cat .log`); done
clean:
	rm -f *.out *.o
