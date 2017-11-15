#!/bin/bash

# Make commands

# AOSP
echo [AOSP]
echo hmm
echo make
echo mm test-art-host-run-test
echo mm test-art-host-gtest
echo mma test-art-host -j40
echo ART_USE_VIXL_ARM_BACKEND=true mm test-art-target-run-test
echo ART_TEST_JIT=true m test-art-host-run-test-001-HelloWorld
echo art/test/run-test --host 001-HelloWorld
echo mm test-art-host-gtest-scheduler_test64 -j20
echo mm cpplint-art -j20
echo art/tools/cpplint.py art/compiler/optimizing/

echo

# Linaro
echo [Linaro]
echo scripts/tests/test_art_host.sh
echo scripts/tests/test_art_target.sh --64bit --optimizing
echo scripts/tests/test_art_target.sh --64bit --gtest --optimizing --jit --libcore
echo scripts/tests/test_art_target.sh --32bit --single-test test-art-target-run-test-ndebug-prebuild-optimizing-relocate-ntrace-cms-checkjni-picimage-npictest-debuggable-082-inline-execute32
echo scripts/tests/test_art_target.sh --64bit --single-test test-art-target-run-test-ndebug-prebuild-optimizing-no-relocate-ntrace-cms-checkjni-picimage-npictest-ndebuggable-no-jvmti-449-checker-bce64
echo scripts/tests/test_art_target.sh --64bit --single-test test-art-target-run-test-ndebug-prebuild-optimizing-no-relocate-ntrace-cms-checkjni-picimage-npictest-ndebuggable-no-jvmti-706-checker-scheduler64
echo scripts/tests/test_art_target.sh --64bit --keep-failures --single-test test-art-target-run-test-ndebug-prebuild-optimizing-no-relocate-ntrace-cms-checkjni-picimage-npictest-ndebuggable-no-jvmti-706-checker-scheduler64
echo scripts/tests/test_art_target.sh --64bit --single-test test-art-target-run-test-debug-prebuild-optimizing-no-relocate-ntrace-gcstress-checkjni-picimage-npictest-ndebuggable-no-jvmti-667-checker-simd-alignment64
