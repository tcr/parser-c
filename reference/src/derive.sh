#!/bin/bash
set -o errexit
DERIVE=./derive/Derive
DERIVE_PATCH_VERSION=2.4.2
if ghc-pkg find-module Data.DeriveMain | grep -q '^[ ]*derive-'; then
    (cd derive && make)
fi
if [ ! -e ${DERIVE} ] ; then
    echo "Warning: Could not find ${DERIVE}, and derive >= 2.5 is not installed">&2
    echo "Please install derive 2.5.* (tested with 2.5.23)" >&2
    exit 1
fi
TARGETS="Language/C/Syntax/AST.hs Language/C/Analysis/SemRep.hs"
for T in ${TARGETS} ; do
	echo "Appending derived instances to ${T}"
	$DERIVE -a "${T}"
done
