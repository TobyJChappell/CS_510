#!/bin/bash

LINE1=`grep -n "Frame" NW1.csv | cut -d : -f 1 | head -1`
LINE2=`grep -n "Frame" NW1.csv | cut -d : -f 1 | tail -1`
DIFF=`$LINE2 - $LINE1`
echo $DIFF
echo `grep -n -A $DIFF "Frame" NW1.csv`
