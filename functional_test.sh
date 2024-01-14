#!/bin/bash

BINARY_PATH="./glados"
EXIT_CODE=0

perform_test() {
    input_file="$1"
    expected_output_file="$2"

    # Run the binary with the input file
    actual_output=$("$BINARY_PATH" "$input_file")

    # Compare the actual output with the expected output from the file
    expected_output=$(cat "$expected_output_file")
    if [ "$actual_output" = "$expected_output" ]; then
        echo "Test passed for input file: $input_file"
    else
        echo "Test failed for input file: $input_file"
        echo "Expected output from file: $expected_output_file"
        echo "Actual output: $actual_output"
        EXIT_CODE=84
    fi
}

perform_test "Testfile/test_assign/assign1.ccs" "Testfile/outputs/compiler/assign1.txt"
perform_test "Testfile/test_assign/assign2.ccs" "Testfile/outputs/compiler/assign2.txt"
perform_test "Testfile/test_assign/assign3.ccs" "Testfile/outputs/compiler/assign3.txt"
# perform_test "test_input2.txt" "expected_output2.txt"
# perform_test "test_input3.txt" "expected_output3.txt"

# Add more tests as needed

# Clean up any temporary files or resources if necessary
# ...

# Exit with a success code if all tests pass, or a failure code if any test fails
# Replace "exit 0" with the appropriate exit code based on your test results
exit $EXIT_CODE
