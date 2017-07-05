PROJECT_DIR=../../..
OPT=-O2
include $(PROJECT_DIR)/test/config.mk
all:
	chmod 444 input.c
	$(HC) $(HFLAGS) --make Test.hs
	./Test input.c
clean:
	rm -rf Test.o Test.hi Test
