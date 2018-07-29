#!/bin/bash
echo $1

if [[ "$1" == *2.hs ]]
then
    stack build hatch:part2 && stack exec part2
else
    stack build hatch:part1 && stack exec part1
fi
