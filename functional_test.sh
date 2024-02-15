#!/bin/bash

BINARY_PATH="./glados"
EXIT_CODE=0
NC='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'

perform_test() {
    input_file="$1"
    expected_output_file="$2"
    isvm="$3"

    if [ "$isvm" = "vm" ]; then
        BINARY_PATH="./glados-vm"
        actual_output=$("$BINARY_PATH" "$input_file")
        expected_output=$(cat "$expected_output_file")
        if [ "$actual_output" = "$expected_output" ]; then
            echo -e "TEST ${GREEN}PASSED${NC} for input file: $input_file"
        else
            echo -e "TEST ${RED}FAILED${NC} for input file: $input_file"
            echo -e "${RED}Expected output from file:${NC}"
            cat  $expected_output_file
            echo
            echo -e "${RED}Actual output:${NC}\n$actual_output"
            EXIT_CODE=84
        fi
        return
    fi

    # Run the compiler on the input file with -d flag
    $BINARY_PATH -d "$input_file" > /dev/null
    $BINARY_PATH "$input_file" > /dev/null
    actual_output=$(echo "$input_file" | sed 's/.*\///' | sed 's/\..*//' | sed 's/$/.debug/')
    actual_output=$(cat "$actual_output")

    # Compare the actual output with the expected output from the file
    expected_output=$(cat "$expected_output_file")
    if [ "$actual_output" = "$expected_output" ]; then
        echo -e "TEST ${GREEN}PASSED${NC} for input file: $input_file"
    else
        echo -e "TEST ${RED}FAILED${NC} for input file: $input_file"
        echo -e "${RED}Expected output from file:${NC}"
        cat  $expected_output_file
        echo
        echo -e "${RED}Actual output:${NC}\n$actual_output"
        EXIT_CODE=84
    fi
}

echo "COMPILATION TESTS STARTING"
echo "-----------------------"

for file in Testfile/examples/*.ccs; do
    perform_test "$file" "Testfile/outputs/compilation_debug_outputs/$(basename "$file" .ccs).txt"
done

echo "-----------------------"
if [ "$EXIT_CODE" = 84 ]; then
    echo -e "\n${RED}COMPILER TESTS FAILED\n${NC}"
    return
else
    echo -e "\n${GREEN}COMPILER TESTS PASSED\n${NC}"
fi
echo "VM TESTS STARTING"
echo "-----------------------"

for file in ./*.dz; do
    perform_test "$file" "Testfile/outputs/vm_outputs/$(basename "$file" .dz).txt" "vm"
done

echo "-----------------------"

# check if .debug files existed
rm -f *.debug

rm -f *.dz

exit $EXIT_CODE
