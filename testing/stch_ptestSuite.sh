#!/bin/sh

#Stitch Lang Regression Test Suite for Parser

STITCH="./../ocaml/stitch"
DECTESTS="./_ptests/dec*"
FUNCTESTS="./_ptests/fun*"

echo "Starting Stitch parese test suite"
echo "\n"

echo "Declaration Tests"

for test in $DECTESTS
do 
	echo "Starting test $test"
	$STITCH $test
	echo "\n"
done

echo "Function Tests"

for test in $FUNCTESTS
do 
	echo "Starting test $test"
	$STITCH $test
	echo "\n"
done

rm _ptests/*.c 


