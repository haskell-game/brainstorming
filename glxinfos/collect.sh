#!/bin/bash

for filename in {dev,contrib}/*.glxinfo
do
  cat ${filename} | grep 'GL_' | tr ',' '\n' | grep -v '^ *$' | sed 's/ *//g' | sort -u
done | sort | uniq -c | sed 's/^ *//' | sort -n -r
