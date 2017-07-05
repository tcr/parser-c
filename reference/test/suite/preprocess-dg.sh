 #!/bin/bash
source ./configuration

if [ -z $2 ] ; then
    echo "Usage: $0 gcc_dg gcc_pre" >&2
    exit 1
fi

BASE_DIR=`pwd`
IN_DIR=`pwd`/$1
IN_ARCHIVE=$1.tgz
OUT_DIR=`pwd`/$2

if [ ! -d "${IN_DIR}" ] ; then
    if [ -e "${IN_ARCHIVE}" ] ; then
        echo "Extracting ${IN_ARCHIVE}"
        tar xzf ${BASE_DIR}/"${IN_ARCHIVE}"
        cd gcc_dg
    else
        echo "${IN_DIR} / ${IN_ARCHIVE} not found" >&2
        exit 1
    fi
else
    if [ ! -e "${OUT_DIR}" ] ; then
        mkdir -p ${OUT_DIR}
    fi
    if [ ! -d "${OUT_DIR}" ] ; then
        echo "Not a directory: ${OUT_DIR}" >&2
        exit 1
    fi
    cd "$IN_DIR"
fi

echo $IN_DIR to $OUT_DIR

for cf in `find . -name '*.c'`; do
	cd $IN_DIR/`dirname $cf`
	f=`basename $cf`
        echo "Processing $f"
	grep -e "^$f" $BASE_DIR/dg-ignore.txt
	if [ $? -eq 0 ]; then echo " ... skipped"; continue; fi
	gcc -E -std=gnu9x $f -o "$OUT_DIR/${f/.c/.i}"
done
