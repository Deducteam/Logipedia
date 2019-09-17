files=$*
regex="def match_(.*) :" # get inductive names
theory="cic"
for f in $files
do
    md=${f##*/}
    md=${md%.*}
    while read line; do
	if [[ $line =~ $regex ]]
	then
	    ind=${BASH_REMATCH[1]}
	    echo "[] ${md}.match_$ind $theory.prop --> ${md}.match_${ind}_prop."
	    echo "[] ${md}.match_$ind $theory.type --> ${md}.match_${ind}_type."
	    echo "[] ${md}.filter_$ind $theory.prop --> ${md}.filter_${ind}_prop."
	    echo "[] ${md}.filter_$ind $theory.type --> ${md}.filter_${ind}_type."
	fi
    done < $f
done
