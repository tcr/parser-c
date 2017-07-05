PROJECT_DIR=../../..
OPT=-O2
include $(PROJECT_DIR)/test/config.mk
all:
	$(HC) $(HFLAGS) Test.hs
	./Test > test.out
	diff test.expect test.out
clean:
	rm -rf Test.o Test.hi Test test.out
