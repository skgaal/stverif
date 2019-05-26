#!/bin/bash

files=$(find . \( -name '*.typ' -o -name '*.st' -o -name '*.var' -o -name '*.fun' \))
errors=0
filen=0

for file in $files
do
	echo "Parsing: "$file
	o=$(./TestPlc -s $file)
	if [[ $o == *"Parse failed"* ]]; then
		echo "Failed parsing: "$file
		let "errors++"
		# ./TestPlc $file
	fi
	let "filen++"
done

echo "------- "$errors"/"$filen" -------- Failures"
