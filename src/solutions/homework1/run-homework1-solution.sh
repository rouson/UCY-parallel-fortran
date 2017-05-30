#!/usr/bin/env bash
# This script exists to facilitate input redirection for a CTest test
cd src/solutions/homework1
output=$(cafrun -n 4 ./homework1_solution < greeting.txt)
if [[ "$output" == *"Test passed"* ]]; then
  echo "Test passed."
fi
