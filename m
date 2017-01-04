#!/bin/bash

# Make commands

# AOSP
echo [AOSP]
echo hmm
echo make
echo mm test-art-host-run-test
echo mm test-art-host-gtest
echo mma test-art-host -j40
echo ART_TEST_JIT=true m test-art-host
echo ART_TEST_JIT=true m test-art-host-run-test-001-HelloWorld
echo art/test/run-test --host 001-HelloWorld
echo mm cpplint-art
echo art/tools/cpplint.py art/compiler/optimizing/

echo

# Linaro
echo [Linaro]
echo scripts/tests/test_art_host.sh

