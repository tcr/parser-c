# Build test binaries
BINARIES=CTest CParse CRoundTrip CEquiv RenderTests ReportFatal CheckGccArgs

PROJECT_DIR=..
BIN_DIR=bin
INPLACE_PACKAGE_CONF=$(PROJECT_DIR)/dist/package.conf.inplace

all: $(INPLACE_PACKAGE_CONF)
	cabal configure --package-db $(INPLACE_PACKAGE_CONF)
	cabal build
	for b in $(BINARIES) ; do cp dist/build/$$b/$$b $(BIN_DIR) ; done

clean:
	rm -f $(addprefix $(BIN_DIR)/, $(BINARIES))
	cabal clean
	make -C harness clean

../dist/package.conf.inplace:
	echo "Please run cd .. && cabal build first"
	exit 1
