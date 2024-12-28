#!/bin/bash

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

generate_output() {
    local file=$1
    local base=${file%.fj}

    echo "Generating ${base}.out"
    ./interpreter "$file" > "${base}.out" 2> "${base}.err"

    echo "Generating ${base}.err"
    ./interpreter "$file" > "${base}.out" 2> "${base}.err"
}

for file in ./good/*.fj; do
    generate_output "$file"
done

for file in ./bad/*.fj; do
    generate_output "$file"
done

rm $TMP_OUT $TMP_ERR