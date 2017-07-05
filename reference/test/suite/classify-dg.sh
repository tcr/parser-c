#!/bin/bash
source ./configuration

BASE_DIR=`pwd`

if [ ! -d gcc_dg ] ; then
    if [ -e gcc_dg.tgz ] ; then
        echo "Extracting gcc_dg.tgz"
        tar xzf ${BASE_DIR}/gcc_dg.tgz
        cd gcc_dg
    else
        echo "gcc_dg / gcc_dg.tgz not found" >&2
        exit 1
    fi
else
    cd gcc_dg
fi
DG_DIR=`pwd`
echo $DG_DIR

for cf in `find . -name '*.c'`; do
	cd $DG_DIR/`dirname $cf`
	f=`basename $cf`
        echo "Processing $f"
	grep -e "^$f" $BASE_DIR/dg-ignore.txt
	if [ $? -eq 0 ]; then echo " ... skipped"; continue; fi

	COMPLIANCE=
	gcc -c -ansi -pedantic-errors  $f 2>/dev/null
	if [ $? -eq 0 ] ; then COMPLIANCE=c89; fi
	if [ -z $COMPLIANCE ] ; then
		gcc -c -std=c99 -pedantic-errors  $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=c99; fi
	fi
	if [ -z $COMPLIANCE ] ; then
		gcc -c -std=gnu9x -pedantic-errors  $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=gnu99; fi
	fi
	if [ -z $COMPLIANCE ] ; then
		gcc -c -std=c11 -pedantic-errors  $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=c11; fi
	fi
	if [ -z $COMPLIANCE ] ; then
		gcc -c -std=gnu11 -pedantic-errors  $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=gnu11; fi
	fi
	if [ -z $COMPLIANCE ] ; then
		gcc -c -std=gnu9x $f 2>/dev/null
		if [ $? -eq 0 ] ; then COMPLIANCE=incompliant; fi
	fi
	if [ ! -z $COMPLIANCE ] ; then
	    echo "[INFO] Classified Test $f as ($COMPLIANCE)"
            mkdir -p "$BASE_DIR/gcc-dg-$COMPLIANCE"
            cp "$DG_DIR/$cf" "$BASE_DIR/gcc-dg-$COMPLIANCE/./"
	fi
done
