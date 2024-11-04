#!/usr/bin/env bash
# author: Soichi Yajima

# Regression test

mkdir -p ./tmp
file=./tmp/$(date +"perf%Y%m%d%H%M%S.txt")

function run() {
  find . | grep trs | sort -u | while read -r p;
  do (timeout 1 toma --completion-with-parsable-output $p 2>&1 >/dev/null && echo "$p $?" || echo "$p $?")
  done
}

{ time run ; } 2> $file

echo "diff:"
diff ./perf.txt $file
