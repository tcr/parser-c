export BINDIR=../../bin
export NO_SEMANTIC_ANALYSIS=1
export CC_FLAGS=-Wall
TMPDIR:=/tmp/
export TMPDIR
all: deprecated osx
osx:
	$(BINDIR)/CTest osx-1.c
deprecated:
	sh $(BINDIR)/compile_log.sh deprecated
	sh $(BINDIR)/compile_log.sh deprecated-2
	sh $(BINDIR)/compile_log.sh deprecated-3
	sh $(BINDIR)/compile_log.sh deprecated-bitfield-init
#	$(BINDIR)/compile_log.sh fun_decl # fails
clean:
	rm -rf *.log
