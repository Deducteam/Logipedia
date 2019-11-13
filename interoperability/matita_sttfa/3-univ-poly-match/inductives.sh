files=$*
regex="def match_(.*) :" # regexp to get inductive names
theory="cic"
for f in $files
do
    md=${f##*/}
    md=${md%.*}
    while read line; do
	if [[ $line =~ $regex ]]
	then
	    ind=${BASH_REMATCH[1]} # capture the name of the inductive
	    echo "[] ${md}.match_$ind $theory.star --> ${md}.match_${ind}_star."
	    echo "[] ${md}.match_$ind $theory.box --> ${md}.match_${ind}_box."
	    #echo "[] ${md}.match_$ind $theory.kind --> ${md}.match_${ind}_kind."
	    echo "[] ${md}.filter_$ind $theory.star --> ${md}.filter_${ind}_star."
	    echo "[] ${md}.filter_$ind $theory.box --> ${md}.filter_${ind}_box."
	    #echo "[] ${md}.filter_$ind $theory.diamond --> ${md}.filter_${ind}_diamond."
	fi
    done < $f
done
