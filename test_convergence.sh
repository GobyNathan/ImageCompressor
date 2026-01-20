#!/bin/bash

if [ ! -f "complex_test.txt" ]; then
    echo "Error: complex_test.txt file not found."
    echo "Please run the Python script to generate the complex dataset first."
    exit 1
fi

if [ ! -f "./imageCompressor" ]; then
    echo "Building imageCompressor..."
    make
fi

echo "Testing with convergence limit of 0..."
time ./imageCompressor -n 10 -l 0 -f complex_test.txt > result_l0.txt

echo "Testing with convergence limit of 0.1..."
time ./imageCompressor -n 10 -l 0.1 -f complex_test.txt > result_l01.txt

echo "Testing with convergence limit of 1..."
time ./imageCompressor -n 10 -l 1 -f complex_test.txt > result_l1.txt

echo "Testing complete. Results:"
echo "Size of result with l=0: $(wc -l < result_l0.txt) lines"
echo "Size of result with l=0.1: $(wc -l < result_l01.txt) lines"
echo "Size of result with l=1: $(wc -l < result_l1.txt) lines"

echo "First few lines of result with l=0:"
head -n 10 result_l0.txt

echo "First few lines of result with l=0.1:"
head -n 10 result_l01.txt

echo "First few lines of result with l=1:"
head -n 10 result_l1.txt