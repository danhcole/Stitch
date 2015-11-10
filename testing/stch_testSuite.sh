#!/bin/sh
#Stitch language regression test suite

SINGER="./toolkit/singer"
TESTS="./_tests/*"
TARGETS="./_targets"
OUTPUTS="./_outputs"
LOG="./`date +%h%d_%H%M%S`_test_log.txt"

clear
echo "Starting Stitch test suite"
echo "\n\n"

for test in $TESTS
do
	echo "Starting Test $test"
	echo "==================="
	echo "Starting Test $test" >> $LOG
	echo "===================" >> $LOG
	ROOT=`basename $test | cut -d'.' -f1`
	$SINGER $test
	echo "\nDIFFing Generated Code"
	echo "==============="
	echo "\nDIFFing Generated Code" >> $LOG
	echo "===============" >> $LOG
	diff $ROOT.c $TARGETS/$ROOT\_target.c >> $LOG
	./$ROOT > $OUTPUTS/$ROOT\_gen.txt
	echo "\nDIFFing Output"
	echo "==============="
	echo "\nDIFFing Output" >> $LOG
	echo "===============" >> $LOG
	diff $OUTPUTS/$ROOT\_gen.txt $OUTPUTS/$ROOT\_out.txt >> $LOG
	echo "\n\n"
done