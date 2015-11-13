#!/bin/sh

#Stitch Lang Regression Test Suite for Parser

COL='\033[0;34m'
SUCC='\033[1;32m'
FAIL='\033[0;31m'
NC='\033[0m'

STITCH="./../ocaml/stitch"
DECTESTS="./_ptests/dec*"
FUNCTESTS="./_ptests/fun*"
LOOPTESTS="./_ptests/loop*"

function echoResult {

	if [ $1 -eq 0 ]; then
		echo "${SUCC}TEST SUCCESSFUL!${NC}"
	else
		echo "${FAIL}TEST FAILED!${NC}"
	fi
}

function printTest {

	echo $COL$(head -n 1 $1) $NC
}

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
	printTest $test
	$STITCH $test
	echoResult $? 
	echo "\n"
done

echo "Function Tests"

for test in $FUNCTESTS
do 
	echo "Starting test $test"
	printTest $test
	$STITCH $test
	echoResult $?
	echo "\n"
done

for test in $LOOPTESTS
do 
	echo "Starting test $test"
	printTest $test
	$STITCH $test
	echoResult $?
	echo "\n"
done

rm _ptests/*.c 


