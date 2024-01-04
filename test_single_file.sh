#!/bin/bash

MY_BINARY="latc_x86"

# Check if correct number of arguments is provided
if [ ! "$#" -eq 1 ]; then
    echo "Usage: $0 <path_to_file>"
    exit 1
fi

# Function to test the binary on files in a given folder
test_single_file() {
   file=$1
   ./$MY_BINARY "$file" >/dev/null

   if [ "$?" -eq 0 ]; then
       echo "COMPILED: $file"
   else
       echo "FAILED TO COMPILE: $file"
       exit 1
   fi
    
   executable_name="${file%.lat}"
   
   
   touch "${executable_name}.input"
   
   ./"$executable_name" < "${executable_name}.input" > "${executable_name}.output_actual"
   if [ "$?" -eq 0 ]; then
       if diff -q "${executable_name}.output_actual" "${executable_name}.output" >/dev/null; then
           echo "PASS: $file"
           rm -f "${executable_name}.output_actual"
       else
           echo "FAIL (missmatched output): $file"
           exit 1
       fi
   else
       echo "FAIL (exit code != 0): $file"
       exit 1
   fi
   echo "ALL OK!"
}

# Test on "good" files with exit code 0
test_single_file "$1"
