file=$1
tt=`grep "scReverted( true), mispred( true)" $file -c`
tn=`grep "scReverted( true), mispred(false)" $file -c`
nt=`grep "scReverted(false), mispred( true)" $file -c`
nn=`grep "scReverted(false), mispred(false)" $file -c`
tageCorr=`expr $tt + $nn`
tageMis=`expr $nt + $tn`
echo $tageCorr correctly predicted by tage
echo $tageMis mispredicted by tage
echo $tt correctly predicted by tage but reverted by sc
echo $tn mispredicted by tage and corrected by sc