PROJECT_DIR=../../..
OPT=-O2
include $(PROJECT_DIR)/test/config.mk
all:
	$(HC) $(HFLAGS) --make Test.hs
	./Test > test.c
	gcc -o test_out test.c
	./test_out > test.out
	diff test.expect test.out
clean:
	rm -rf Test test.c test_out *.o *.hi *.dyn_hi *.dyn_o *.out
