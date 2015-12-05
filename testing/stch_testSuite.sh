#!/bin/sh
#Stitch language regression test suite

COL='\033[0;34m'		#Blue color for description
SUCC='\033[1;32m'		#Green color for success
FAIL='\033[0;31m'		#Red color for failure
NC='\033[0m'			#No color - to clear the color after

SINGER="./toolkit/singer"
TESTS="./_tests/*"
TARGETS="./_targets"
OUTPUTS="./_outputs"
BIN="./_bin"
LOG="./_log/`date +%h%d_%H%M%S`_test_log.txt"

#print whether we succeeded or failed the test
function echoResult {

	if [ $1 -eq 0 ]; then
		echo "${SUCC}TEST SUCCESSFUL!${NC}"
		echo "TEST SCCESSFUL!" >> $LOG
	else
		echo "${FAIL}TEST FAILED!${NC}"
		echo "TEST SCCESSFUL!" >> $LOG
	fi
}

function checkComp {

	if [ $1 -eq 0 ]; then
		echo "${SUCC}COMPILE SUCCESSFUL!${NC}"
		echo "COMPILE SUCCESSFUL!" >> $LOG
	else
		echo "${FAIL}COMPILE FAILED!${NC}"
		echo "COMPILE FAILED!" >> $LOG
		break
	fi
}

######################
# SCRIPT STARTS HERE #
######################

#Make the compiler if it isn't already made
clear
echo "Making the compiler..."
cd ../ocaml
make all > /dev/null 
cd ../testing

for test in $TESTS
do
	echo "Starting Test $test" 2>&1 | tee -a $LOG
	echo "===================" 2>&1 | tee -a $LOG
	ROOT=`basename $test | cut -d'.' -f1`
	$SINGER $test
	checkComp $?
	mv ./\_tests/$ROOT.stch.c ./\_targets
	mv ./$ROOT $BIN
	$BIN/$ROOT > $OUTPUTS/$ROOT\_gen.txt
	echo "\nDIFFing Output"  2>&1 | tee -a $LOG
	echo "==============="  2>&1 | tee -a $LOG
	diff -w $OUTPUTS/$ROOT\_gen.txt $OUTPUTS/$ROOT\_out.txt
	echoResult $? 
	echo "\n\n" 2>&1 | tee -a $LOG
done

cd $OUTPUTS
rm *_gen.txt
cd ../$TARGETS
rm *.c
