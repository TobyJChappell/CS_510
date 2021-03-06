#!/bin/bash
# Splits NW1.csv into two files

File=$1

F1_L1=$(grep -n "Frame" $File | cut -d : -f 1 | head -1)
F1_L2=$(grep -n "Trajectories" $File | cut -d : -f 1 | tail -1)
tail -n "+$F1_L1" $File | head -n "$((F1_L2-F1_L1))" > NW1-1.csv

F2_L1=$(grep -n "Frame" $File | cut -d : -f 1 | tail -1)
tail -n "+$F2_L1" $File > NW1-2.csv
