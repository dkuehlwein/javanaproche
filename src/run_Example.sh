#!/bin/sh

cd java
rm *.class # force recompilation
. ../env.sh

run Example

