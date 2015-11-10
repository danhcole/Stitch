#!/bin/sh
#Stitch language regression test suite

SINGER="./toolkit/singer"
TESTS="./tests/*"
TARGETS="./targets"
OUTPUT="./test_log.txt"

for test in $TESTS
do
	ROOT=`basename $test | cut -d'.' -f1`
	$SINGER $test
	diff $test $TARGETS/$ROOT\_target.c > OUTPUT
done