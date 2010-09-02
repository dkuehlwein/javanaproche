#!/bin/sh

. ./env.sh

echo "Compiling Javanaproche"
# javac src/java/net/naproche/GUI/*.java -d .
javac src/java/net/naproche/preparser/*.java -d .

echo "Javanaproche Module: Example"
java -Djava.library.path=.:$PLLIBDIR net/naproche/preparser/Example
