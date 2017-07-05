SCANFILE=../../../examples/ScanFile
all:
	$(SCANFILE) enum.c > enum.out
	diff -u enum.expect enum.out
clean:
	rm -f *.out
