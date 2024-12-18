#!/bin/bash
ramin=1
ramax=5
rastep=1
rbmin=1 #7
rbmax=1
rbstep=1
rcmin=1
rcmax=1
rcstep=1
rdmin=1 #1
rdmax=1 #8
rdstep=1
remin=1
remax=1
restep=1
rfmin=1
rfmax=1
rfstep=1
rgmin=1
rgmax=1
rgstep=1
rhmin=1
rhmax=1
rhstep=1

filename="parameters_mod8.txt"

echo "Study: sweeping rx and ry parameters "
echo "   - ra from $ramin to $ramax with $rastep increment"
echo "   - rb from $rbmin to $rbmax with $rbstep increment"
echo "   - rc from $rcmin to $rcmax with $rcstep increment"
echo "   - rd from $rdmin to $rdmax with $rdstep increment"
echo "   - re from $remin to $remax with $restep increment"
echo "   - rf from $rfmin to $rfmax with $rfstep increment"
echo "   - rg from $rgmin to $rgmax with $rgstep increment"
echo "   - rh from $rhmin to $rhmax with $rhstep increment"
echo "Saving parameters in ${filename}"

rm -f ${filename}

for ((ra=$ramin;ra<=$ramax;ra=$ra+$rastep)); do
    for ((rb=$rbmin;rb<=$rbmax;rb=$rb+$rbstep)); do
	for ((rc=$rcmin;rc<=$rcmax;rc=$rc+$rcstep)); do
	    for ((rd=$rdmin;rd<=$rdmax;rd=$rd+$rdstep)); do
		for ((re=$remin;re<=$remax;re=$re+$restep)); do
		    for ((rf=$rfmin;rf<=$rfmax;rf=$rf+$rfstep)); do
                        for ((rg=$rgmin;rg<=$rgmax;rg=$rg+$rgstep)); do
			    for ((rh=$rhmin;rh<=$rhmax;rh=$rh+$rhstep)); do
	        		echo "$ra $rb $rc $rd $re $rf $rg $rh" >> ${filename}
			    done
			done
    		    done
		done
	    done
	done
    done
done
Nlines=$(grep -c "" < ${filename})
echo "$Nlines different parameters combinations"
