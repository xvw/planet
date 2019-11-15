#!/bin/bash

i3status | while :
do
    read line
    now=`~/Programmation/planet/now.exe`
    echo "$line | $now" || exit 1
done
