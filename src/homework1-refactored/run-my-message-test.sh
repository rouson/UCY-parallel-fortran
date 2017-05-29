#!/usr/bin/env bash
# This script exists to facilitate input redirection for a CTest test
cd src/homework1-refactored
output=$(cafrun -n 4 ./test_my_message < greeting.txt)
if [[ "$output" == *"Test passed"* ]]; then
  echo "Test passed."
fi
