#!/usr/bin/env bash

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

for file in ./good/*.fj
do
    ./interpreter $file > $TMP_OUT 2> $TMP_ERR

    if diff ${file%.fj}.out $TMP_OUT >/dev/null
    then 
        echo -e "$file: OK"
    else 
        echo -e "$file: ERROR IN OUTPUT"
    fi

    if diff ${file%.fj}.err $TMP_ERR >/dev/null
    then 
        echo -e "$file: OK"
    else 
        echo -e "$file: ERROR IN ERROR"
    fi

    echo ""
done

for file in ./bad/*.fj
do
    ./interpreter $file > $TMP_OUT 2> $TMP_ERR

    if diff ${file%.fj}.out $TMP_OUT >/dev/null
    then 
        echo -e "$file: OK"
    else 
        echo -e "$file: ERROR IN OUTPUT"
    fi

    if diff ${file%.fj}.err $TMP_ERR >/dev/null
    then 
        echo -e "$file: OK"
    else 
        echo -e "$file: ERROR IN ERROR"
    fi

    echo ""
done

rm $TMP_OUT $TMP_ERR