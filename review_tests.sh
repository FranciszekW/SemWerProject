#!/usr/bin/env bash

# Function to process a directory
process_directory() {
    local dir=$1
    echo -e "Processing directory: $dir\n"
    for test_file in "$dir"/*.fj; do
        if [ -f "$test_file" ]; then
            base_name=$(basename "$test_file" .fj)
            echo -e "\n--------------------------------------------------"
            echo -e "Test: $dir/$base_name"
            echo -e "--------------------------------------------------\n"
            echo -e "File: $dir/$base_name.fj\n"
            cat "$dir/$base_name.fj"
            echo -e "\n\nFile: $dir/$base_name.out\n"
            cat "$dir/$base_name.out"
            echo -e "\n\nFile: $dir/$base_name.err\n"
            cat "$dir/$base_name.err"
        fi
    done
}

# Process 'good' directory
process_directory "good"

# Process 'bad' directory
process_directory "bad"