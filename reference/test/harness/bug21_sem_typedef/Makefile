MAIN=typedef
SCANFILE=../../../examples/ScanFile
all:
	$(SCANFILE) $(MAIN).c > $(MAIN).out
	diff -u $(MAIN).expect $(MAIN).out
clean:
	rm -f *.out
