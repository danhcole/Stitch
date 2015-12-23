#!/bin/sh

#Stitch Lang Regression Test Suite for Parser
#
# Author: Megan Skrypek

COL='\033[0;34m'		#Blue color for description
SUCC='\033[1;32m'		#Green color for success
FAIL='\033[0;31m'		#Red color for failure
NC='\033[0m'			#No color - to clear the color after

STITCH="./../ocaml/stitch"
DECTESTS="./_ptests/dec*"
FUNCTESTS="./_ptests/fun*"
LOOPTESTS="./_ptests/loop*"

#print whether we succeeded or failed the test
function echoResult {

	if [ $1 -eq 0 ]; then
		echo "${SUCC}TEST SUCCESSFUL!${NC}"
	else
		echo "${FAIL}TEST FAILED!${NC}"
	fi
}

#print the information about each tests
function printTest {

	echo $COL$(head -n 1 $1) $NC
}


#run all tests based on path passed in
function runTests {

	for test in $@
	do 
		echo "Starting test $test"
		printTest $test
		$STITCH $test
		echoResult $? 
		echo "\n"
	done


}

#---------------------------------#
#SCRIPT STARTS HERE               #
#---------------------------------#

#Make the compiler if it isn't already made
echo "Making the compiler..."
cd ../ocaml
make all > /dev/null 
cd ../testing


echo "Starting Stitch parse test suite"
echo "\n"

echo "Declaration Tests" #declaration tests
runTests $DECTESTS
echo "Function Tests" #function tests
runTests $FUNCTESTS
echo "Loop Tests"      #loop tests
runTests $LOOPTESTS


rm _ptests/*.c 


