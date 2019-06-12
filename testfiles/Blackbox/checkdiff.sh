#!/bin/bash
echo "======== IR ========"
iry=0
for f in *.ir
do
	r=$(diff $f expected/$f)
	if [ "$r" != "" ]; then
		echo "File: $f fail"
		iry=$(expr $iry + 1)
    fi
done
if [ $iry -ne 0 ]; then
	echo "Total failures for IR: $iry"
fi 

echo "======= TAPN ======="
irt=0
for f in *.tapn
do
	r=$(diff $f expected/$f	)
	if [ "$r" != "" ]; then
		echo "File: $f fail"
		irt=$(expr $irt + 1)
	fi
done
if [ $irt -ne 0 ]; then
	echo "Total failures for TAPN: $irt"
fi 

