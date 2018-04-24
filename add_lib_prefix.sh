#!/bin/bash

argv=$1
dir_name=${argv%%/*}
for i in `ls $1`
do
    file_name=${i##*/}
    old_file=$dir_name/$file_name
    prefix=${file_name:0:3}
    if [ "lib" != "$prefix" ] 
    then
        new_file_name=lib$file_name
        new_file=$dir_name/$new_file_name 
        mv $old_file $new_file
    fi
done
