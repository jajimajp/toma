#!/usr/bin/env bash
# author: Soichi Yajima

# Regression test

mkdir -p ./tmp
file=./tmp/$(date +"test_snap%Y%m%d%H%M%S.txt")

find . | grep trs | sort -u | while read -r p;
do (timeout 1 toma --completion-with-parsable-output $p 2>&1 >/dev/null && echo "$p $?" || echo "$p $?")
done > $file

echo "diff:"
diff ./test_snapshot.txt $file
