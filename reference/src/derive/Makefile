GHC=ghc
GHC_FLAGS=-O
DERIVE_TARGET=Derive
.PHONY: all clean
all: $(DERIVE_TARGET)
$(DERIVE_TARGET): | objdir
	$(GHC) $(GHC_FLAGS) --make -outputdir objdir -o $(DERIVE_TARGET) $(DERIVE_TARGET).hs
objdir:
	mkdir -p $@
test: $(DERIVE_TARGET)
	./$(DERIVE_TARGET) -a DeriveTest.hs
	diff DeriveTest.hs.expect DeriveTest.hs
clean:
	rm -f $(DERIVE_TARGET)
	rm -rf objdir
