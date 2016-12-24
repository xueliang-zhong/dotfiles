#!/bin/bash

# Make commands

# AOSP
echo [AOSP]
echo hmm
echo make
echo ART_TEST_JIT=true m test-art-host
echo ART_TEST_JIT=true m test-art-host-run-test-001-HelloWorld
echo art/test/run-test --host 001-HelloWorld
echo mma -j40 test-art-host
echo art/tools/cpplint.py art/compiler/optimizing/

echo

# Linaro
echo [Linaro] 
echo scripts/tests/run_art_test.sh --default

