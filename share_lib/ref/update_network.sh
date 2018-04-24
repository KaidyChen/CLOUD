#!/bin/bash

file="/etc/sysconfig/network-scripts/ifcfg-eth0"
filebak=$file".bak"

if [ ! -f "$filebak" ]
then
    cp "$file" "$filebak"
fi

while getopts ":a:b:c:d:" opt 
do
    case $opt in
        a)
            IPADDR=$OPTARG;;
        b)
            NETMASK=$OPTARG;;
        c)
            GATEWAY=$OPTARG;;
        d) 
            DNS1=$OPTARG;;
        ?)
            echo "error"
            exit 1;;
    esac
done


if [ -n "$IPADDR" ] 
then
    sed -i "s/\(IPADDR=\).*/\1$IPADDR/g" $file
fi

if [ -n "$NETMASK" ]
then
    sed -i "s/\(NETMASK=\).*/\1$NETMASK/g" $file
fi

if [ -n "$GATEWAY" ]
then
    sed -i "s/\(GATEWAY=\).*/\1$GATEWAY/g" $file
fi

if [ -n "$DNS1" ]
then
    sed -i "s/\(DNS1=\).*/\1$DNS1/g" $file
fi




