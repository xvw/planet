#!/bin/bash

i3status | while :
do
    read line
    now=`~/Programmation/planet/now.exe`
    echo "$now | $line" || exit 1
done
