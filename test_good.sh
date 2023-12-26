#!/bin/bash

MY_BINARY="latc_x86"

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

# Function to check if an element is in an array
element_in_array() {
    local target_element="$1"
    shift  # Shift to remove the target element from the parameters
    
    for element in "$@"; do
        if [[ "$element" == "$target_element" ]]; then
            return 0  # Element found
        fi
    done
    
    return 1  # Element not found
}

# Function to test the binary on files in a given folder
test_binary_on_files() {
    local folder="$1"
    local exit_code="$2"
    
    fails_that_failed_to_compile=("lattests/good/core016.lat")
    
    echo "Testing files in $folder..."
    
    for file in "$folder"/*.lat; do
        if [ -f "$file" ]; then
            ./$MY_BINARY "$file" >/dev/null
            if [ "$?" -eq "$exit_code" ]; then
                echo "PASS: $file"
            else
                echo "FAIL: $file"
                fails_that_failed_to_compile+=("$file")
            fi
        fi
    done
    
    # Print the selected files
    echo "##########################################"
    echo "Files that failed to compile:"
    for selected_file in "${fails_that_failed_to_compile[@]}"; do
        echo "$selected_file"
    done
    
    echo ""
    echo "####################################################"
    echo "EXECUTING FILES"
    files_that_failed=()
    for file in "$folder"/*.lat; do
        if [ -f "$file" ] && ! element_in_array "$file" "${fails_that_failed_to_compile[@]}"; then
            echo "testing ${file}"
            # Extract the base name without extension
            executable_name="${file%.lat}"
            
            
            touch "${executable_name}.input"
            
            ./"$executable_name" < "${executable_name}.input" > "${executable_name}.output_actual"
            
            if [ "$?" -eq 0 ]; then
                
                if diff -q "${executable_name}.output_actual" "${executable_name}.output" >/dev/null; then
                    echo "PASS: $file"
                    rm -f "${executable_name}.output_actual"
                    
                else
                    echo "FAIL (missmatched output): $file"
                    files_that_failed+=("$file")
                fi
            else
                echo "FAIL (exit code != 0): $file"
                files_that_failed+=("$file")
            fi
        fi
    done
    
    echo "##########################################"
    echo "Files that failed:"
    for selected_file in "${files_that_failed[@]}"; do
        echo "$selected_file"
    done
    
    
    
    echo ""
}

# Test on "good" files with exit code 0
test_binary_on_files "$good_files_folder" 0
