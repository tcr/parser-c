#!/bin/sh
source ./configuration

if [ $# -eq 0 ];  then
	echo "Usage: run-dg-list.sh dg-test-1.c ..."
	exit 1
fi

TEST_SUITE="gcc-dg-selection"
sh clear_test_suite $TEST_SUITE
source $CTEST_BINDIR/set_test_suite $TEST_SUITE
export CTEST_DRIVER=CRoundTrip

BASE_DIR=`pwd`
cd gcc.dg
DG_DIR=`pwd`
for cf in $@ ; do
for f in `find . -name $cf | grep -v noncompile`; do
	echo "[INFO] Running Test $f"
	# grep -e "^$f" $BASE_DIR/dg-ignore.txt
	# if [ $? -eq 0 ]; then echo " ... skipped"; continue; fi
	# grep -e "__attribute__" $f >/dev/null
	# if [ $? -ne 0 ]; then continue; fi

	gcc -I$DG_DIR -I$DG_DIR/cpp -fsyntax-only -std=gnu9x $f 2>/dev/null
	if [ $? -eq 0 ] ; then 
		bash run-test $f
	else
		echo "[ERROR] Not running Test $f"
		gcc -I$DG_DIR -I$DG_DIR/cpp -fsyntax-only -std=gnu9x $f
		echo "[EXIT]"
		exit 1
	fi
done
done
