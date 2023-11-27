#!/bin/bash

MY_BINARY="latc"

default_bad_files_folder="lattests/bad"

# Check if correct number of arguments is provided
if [ "$#" -eq 0 ]; then
    echo "No arguments provided. Using default folder paths."
    bad_files_folder="$default_bad_files_folder"
elif [ "$#" -eq 1 ]; then
    bad_files_folder="$1"
else
    echo "Usage: $0 <path_to_bad_files>"
    exit 1
fi

# Function to test the binary on files in a given folder
test_binary_on_files() {
    local folder="$1"
    local exit_code="$2"

    echo "Testing files in $folder..."
    
    selected_files=()

    for file in "$folder"/*.lat; do
        if [ -f "$file" ]; then
            ./$MY_BINARY "$file" >/dev/null 2>/dev/null
            if [ "$?" -eq "$exit_code" ]; then
                echo "PASS: $file"
            else
                echo "FAIL: $file"
                selected_files+=("$file")
            fi
        fi
    done
    echo "##############################################"
    # Print the selected files
    echo "Files satisfying the condition:"
    for selected_file in "${selected_files[@]}"; do
        echo "$selected_file"
    done

    echo ""
}

# Test on "bad" files with exit code 0
test_binary_on_files "$bad_files_folder" 1
