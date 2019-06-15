#!/bin/bash
tapn=verifydtapn64
f=$2

#for f in *.xml
#do
	if [[ $f == cur_net.xml ]]; then
		continue
	fi
	q=query.q
	t=$1
	k=10
	diff=$t
	thigh=$t
	tlow=0
	echo $f
	rm cur_net.xml
	sed 's/\*\*PLACEHOLDER\*\*/'$t'/g' $f > cur_net.xml
	rm query.q
	echo 'AF watchdog_ok__ = 1' | sed 's/.xml//' > query.q

	result=$(./$tapn -k $k cur_net.xml $q)
	while [[ $result == *"Max number of tokens found in any reachable marking: >"* ]] || [[ $result == *"The specified k-bound is less than the number of tokens in the initial markings."* ]]; do
		k=$(expr $k + 10)
		result=$(./$tapn -k $k cur_net.xml $q)
	done
	echo $k
	
	result=$(./$tapn -k $k cur_net.xml $q)
	while [[ $result == *"Query is NOT satisfied"* ]]; do
		thigh=$(expr $thigh + $thigh)
		tlow=$t
		t=$thigh
		rm cur_net.xml
		sed 's/\*\*PLACEHOLDER\*\*/'$t'/g' $f > cur_net.xml
		result=$(./$tapn -k $k cur_net.xml $q)
		echo "$tlow - $thigh"
	done
	
	time=-1
	while [[ $diff -gt 0 ]]; do
		t=$(expr \( $thigh + $tlow \) / 2)
		rm cur_net.xml
		sed 's/\*\*PLACEHOLDER\*\*/'$t'/g' $f > cur_net.xml
		timestart=`date +%s`
		result=$(./$tapn -k $k cur_net.xml $q)
		time=$((`date +%s`-timestart))
		if [[ $result == *"Query is satisfied"* ]]; then
			thigh=$t
		else
			tlow=$(expr $t + 1)
		fi
		echo "$tlow - $thigh"
		diff=$(expr $thigh - $tlow)
	done
	echo "File $f Query $q Maxt $thigh Time $time"


#done
