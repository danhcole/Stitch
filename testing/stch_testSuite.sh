#!/bin/sh
#Stitch language regression test suite

COL='\033[0;34m'		#Blue color for description
SUCC='\033[1;32m'		#Green color for success
FAIL='\033[0;31m'		#Red color for failure
NC='\033[0m'			#No color - to clear the color after

SINGER="./toolkit/singer"
STITCH="../ocaml/stitch"
TESTS="./_tests/*"
NTESTS="./_ntests/*"
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
		echo "TEST FAILED!" >> $LOG
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

function checkNComp {

	echo "${SUCC}COMPILE FAILED!${NC}"
	echo "COMPILE FAILED!" >> $LOG
	break
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
	$BIN/$ROOT > $OUTPUTS/$ROOT\_gen.txt 2>&1
	echo "\nDIFFing Output"  2>&1 | tee -a $LOG
	echo "==============="  2>&1 | tee -a $LOG
	diff -w $OUTPUTS/$ROOT\_gen.txt $OUTPUTS/$ROOT\_out.txt
	echoResult $? 
	echo "\n\n" 2>&1 | tee -a $LOG
done

trap checkNComp ERR

for test in $NTESTS
do
	echo "Starting Negative Test $test" 2>&1 | tee -a $LOG
	echo "============================" 2>&1 | tee -a $LOG
	$STITCH $test || true
	echo "\n\n" 2>&1 | tee -a $LOG
done

cd $OUTPUTS
rm *_gen.txt
cd ../$TARGETS
rm *.c
cd ../$BIN
rm *
