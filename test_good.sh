#!/bin/bash

MY_BINARY="latc"

default_good_files_folder="lattests/good"

# Check if correct number of arguments is provided
if [ "$#" -eq 0 ]; then
    echo "No arguments provided. Using default folder paths."
    good_files_folder="$default_good_files_folder"
elif [ "$#" -eq 1 ]; then
    good_files_folder="$1"
else
    echo "Usage: $0 <path_to_good_files>"
    exit 1
fi

# Function to test the binary on files in a given folder
test_binary_on_files() {
    local folder="$1"
    local exit_code="$2"

    echo "Testing files in $folder..."

    for file in "$folder"/*.lat; do
        if [ -f "$file" ]; then
            ./$MY_BINARY "$file" >/dev/null 
            if [ "$?" -eq "$exit_code" ]; then
                echo "PASS: $file"
            else
                echo "FAIL: $file"
            fi
        fi
    done

    echo ""
}

# Test on "good" files with exit code 0
test_binary_on_files "$good_files_folder" 0
