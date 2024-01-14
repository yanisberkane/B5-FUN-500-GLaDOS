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
    fi

    # Run the binary with the input file
    actual_output=$("$BINARY_PATH" "$input_file" 2>&1)

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

perform_test "Testfile/test_assign/assign1.ccs" "Testfile/outputs/compiler/assign1.txt"
perform_test "Testfile/test_assign/assign2.ccs" "Testfile/outputs/compiler/assign2.txt"
perform_test "Testfile/test_assign/assign3.ccs" "Testfile/outputs/compiler/assign3.txt"

perform_test "Testfile/test_define/define1.ccs" "Testfile/outputs/compiler/define1.txt"
perform_test "Testfile/test_define/define2.ccs" "Testfile/outputs/compiler/define2.txt"
perform_test "Testfile/test_define/define3.ccs" "Testfile/outputs/compiler/define3.txt"
perform_test "Testfile/test_define/define4.ccs" "Testfile/outputs/compiler/define4.txt"
perform_test "Testfile/test_define/define5.ccs" "Testfile/outputs/compiler/define5.txt"

perform_test "Testfile/test_if/if1.ccs" "Testfile/outputs/compiler/if1.txt"
perform_test "Testfile/test_if/if2.ccs" "Testfile/outputs/compiler/if2.txt"
perform_test "Testfile/test_if/if3.ccs" "Testfile/outputs/compiler/if3.txt"
perform_test "Testfile/test_if/if4.ccs" "Testfile/outputs/compiler/if4.txt"
perform_test "Testfile/test_if/if5.ccs" "Testfile/outputs/compiler/if5.txt"
perform_test "Testfile/test_if/if6.ccs" "Testfile/outputs/compiler/if6.txt"

perform_test "Testfile/test_call/call1.ccs" "Testfile/outputs/compiler/call1.txt"
perform_test "Testfile/test_call/call2.ccs" "Testfile/outputs/compiler/call2.txt"
perform_test "Testfile/test_call/call3.ccs" "Testfile/outputs/compiler/call3.txt"

perform_test "Testfile/test_lambda/lambda1.ccs" "Testfile/outputs/compiler/lambda1.txt"
perform_test "Testfile/test_lambda/lambda2.ccs" "Testfile/outputs/compiler/lambda2.txt"
perform_test "Testfile/test_lambda/lambda3.ccs" "Testfile/outputs/compiler/lambda3.txt"
perform_test "Testfile/test_lambda/lambda4_ss.ccs" "Testfile/outputs/compiler/lambda4_ss.txt"

perform_test "Testfile/test_math_op/math_op1.ccs" "Testfile/outputs/compiler/math_op1.txt"
perform_test "Testfile/test_math_op/operator1.ccs" "Testfile/outputs/compiler/operator1.txt"
perform_test "Testfile/test_math_op/operator2.ccs" "Testfile/outputs/compiler/operator2.txt"

mv *.dz Testfile/outputs/compiled/

echo "-----------------------"
echo -e "${GREEN}COMPILER TESTS PASSED${NC}"
echo "-----------------------"
echo "VM TESTS STARTING"
echo "-----------------------"

# VM TESTS

perform_test "Testfile/outputs/compiled/assign1.dz" "Testfile/outputs/vmoutputs/assign1.txt" "vm"
perform_test "Testfile/outputs/compiled/assign2.dz" "Testfile/outputs/vmoutputs/assign2.txt" "vm"
perform_test "Testfile/outputs/compiled/assign3.dz" "Testfile/outputs/vmoutputs/assign3.txt" "vm"

perform_test "Testfile/outputs/compiled/define1.dz" "Testfile/outputs/vmoutputs/define1.txt" "vm"
perform_test "Testfile/outputs/compiled/define2.dz" "Testfile/outputs/vmoutputs/define2.txt" "vm"
# perform_test "Testfile/outputs/compiled/define3.dz" "Testfile/outputs/vmoutputs/define3.txt" "vm"
perform_test "Testfile/outputs/compiled/define4.dz" "Testfile/outputs/vmoutputs/define4.txt" "vm"
perform_test "Testfile/outputs/compiled/define5.dz" "Testfile/outputs/vmoutputs/define5.txt" "vm"

perform_test "Testfile/outputs/compiled/if1.dz" "Testfile/outputs/vmoutputs/if1.txt" "vm"
perform_test "Testfile/outputs/compiled/if2.dz" "Testfile/outputs/vmoutputs/if2.txt" "vm"
perform_test "Testfile/outputs/compiled/if3.dz" "Testfile/outputs/vmoutputs/if3.txt" "vm"
perform_test "Testfile/outputs/compiled/if4.dz" "Testfile/outputs/vmoutputs/if4.txt" "vm"
perform_test "Testfile/outputs/compiled/if5.dz" "Testfile/outputs/vmoutputs/if5.txt" "vm"
perform_test "Testfile/outputs/compiled/if6.dz" "Testfile/outputs/vmoutputs/if6.txt" "vm"

perform_test "Testfile/outputs/compiled/call1.dz" "Testfile/outputs/vmoutputs/call1.txt" "vm"
# perform_test "Testfile/outputs/compiled/call2.dz" "Testfile/outputs/vmoutputs/call2.txt" "vm"
# perform_test "Testfile/outputs/compiled/call3.dz" "Testfile/outputs/vmoutputs/call3.txt" "vm"

perform_test "Testfile/outputs/compiled/lambda1.dz" "Testfile/outputs/vmoutputs/lambda1.txt" "vm"
perform_test "Testfile/outputs/compiled/lambda2.dz" "Testfile/outputs/vmoutputs/lambda2.txt" "vm"
perform_test "Testfile/outputs/compiled/lambda3.dz" "Testfile/outputs/vmoutputs/lambda3.txt" "vm"
perform_test "Testfile/outputs/compiled/lambda4_ss.dz" "Testfile/outputs/vmoutputs/lambda4_ss.txt" "vm"

# perform_test "Testfile/outputs/compiled/math_op1.dz" "Testfile/outputs/vmoutputs/math_op1.txt" "vm"
perform_test "Testfile/outputs/compiled/operator1.dz" "Testfile/outputs/vmoutputs/operator1.txt" "vm"
perform_test "Testfile/outputs/compiled/operator2.dz" "Testfile/outputs/vmoutputs/operator2.txt" "vm"

# perform_test "test" "test" "vm"
# perform_test "test_input2.txt" "expected_output2.txt"
# perform_test "test_input3.txt" "expected_output3.txt"

# Add more tests as needed

# Clean up any temporary files or resources if necessary
# ...

# Exit with a success code if all tests pass, or a failure code if any test fails
# Replace "exit 0" with the appropriate exit code based on your test results
exit $EXIT_CODE
