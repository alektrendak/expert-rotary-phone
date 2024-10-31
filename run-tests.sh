#!/bin/bash

for source_file in "tests"/*.b; do
    test_name=$(basename "$source_file" .b)

    input_files=("tests/${test_name}".*.input)
    if [ -e "${input_files[0]}" ]; then
        for input_file in "${input_files[@]}"; do
            index=$(basename "$input_file" .input | awk -F. '{print $2}')
            output_file="tests/${test_name}.${index}.output"
            tmp_file="tests/${test_name}.${index}.tmp"

            runhaskell Main.hs "$source_file" < "$input_file" > "$tmp_file"

            if diff "$tmp_file" "$output_file" > /dev/null; then
                echo "Test passed for $source_file with input $input_file"
            else
                echo "Test failed for $source_file with input $input_file"
                diff "$tmp_file" "$output_file"
            fi

            rm "$tmp_file"
        done
    else
        tmp_file="tests/${test_name}.tmp"
        output_file="tests/${test_name}.output"

        runhaskell Main.hs "$source_file" > "$tmp_file"

        if diff "$tmp_file" "$output_file" > /dev/null; then
            echo "Test passed for $source_file"
        else
            echo "Test failed for $source_file"
            diff "$tmp_file" "$output_file"
        fi

        rm "$tmp_file"
    fi
done