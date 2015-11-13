#!/bin/sh

#Stitch Lang Regression Test Suite for Parser

STITCH="./../ocaml/stitch"
DECTESTS="./_ptests/dec*"
FUNCTESTS="./_ptests/fun*"
LOOPTESTS="./_ptests/loop*"

echo "Making the compiler..."
cd ../ocaml
make all > /dev/null 
cd ../testing


echo "Starting Stitch parse test suite"
echo "\n"

echo "Declaration Tests"

for test in $DECTESTS
do 
	echo "Starting test $test"
	echo $(head -n 1 $test)
	$STITCH $test 
	echo "\n"
done

echo "Function Tests"

for test in $FUNCTESTS
do 
	echo "Starting test $test"
	echo $(head -n 1 $test)
	$STITCH $test
	echo "\n"
done

for test in $LOOPTESTS
do 
	echo "Starting test $test"
	echo $(head -n 1 $test)
	$STITCH $test
	echo "\n"
done

rm _ptests/*.c 


