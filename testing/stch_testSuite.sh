#!/bin/sh
#Stitch language regression test suite

SINGER="./toolkit/singer"
TESTS="./tests/*"
TARGETS="./targets"
OUTPUT="./test_log.txt"

for test in $TESTS
do
	echo "Starting Test $test"
	echo "==================="
	echo "Starting Test $test" >> $OUTPUT
	echo "===================" >> $OUTPUT
	ROOT=`basename $test | cut -d'.' -f1`
	$SINGER $test
	echo "DIFFing results"
	echo "==============="
	echo "DIFFing results" >> $OUTPUT
	echo "===============" >> $OUTPUT
	diff $test $TARGETS/$ROOT\_target.c >> $OUTPUT
done