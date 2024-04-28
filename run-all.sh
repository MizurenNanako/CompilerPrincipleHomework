#!/bin/sh
echo '' > run.log
for i in $(ls input-examples/inputs/*.cmm); do
    echo $i >> run.log;
    ./plot-ast.sh $i >> run.log;
done;