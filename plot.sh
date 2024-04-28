#!/bin/sh

if [ -z $1 ]; then
    echo "usage: $0 <cmm filename>";
    echo "The output will be an png file in same diractory.";
    echo "requires dot command from graphviz.";
    exit 0;
fi

dune exec cph $1 > /tmp/a
if [[ $? -eq 0 ]]; then
    dot -Tpng /tmp/a > $1.png
else
    cat /tmp/a;
    exit -1;
fi