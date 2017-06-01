#!/usr/bin/env bash

# This is a script that evaluates the given program code in <language name>
# You can use this to check the output of a program quickly

# For example, `./evaluate.sh "1+1"` should print the following:
# ..
# 2

if [[ $# -ne 1 ]]; then
	echo "usage: $0 <code to evaluate>"
	exit 1
fi

INPUT=$1
shift

FILENAME=output/evaluated_program

make clean main && \
	rm -f $FILENAME.run && \
	./main <(echo "$INPUT") > $FILENAME.s && \
	make $FILENAME.run && \
	$FILENAME.run
