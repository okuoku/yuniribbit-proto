(library (r7c heap flonum)
  (export
    $flonum?

    $fl-nan? $fl-finite? $fl-infinite?
    $fl= $fl<= $fl>= $fl< $fl>
    $fl+ $fl- $fl* $fl/
    $fl->fx
    $fl-expt
    $fl-floor $fl-ceiling $fl-truncate $fl-round
    $fl-acos $fl-asin $fl-atan $fl-atan2
    $fl-cos  $fl-sin  $fl-tan
    $fl-exp $fl-log $fl-loge $fl-sqrt

    $fl-floor/ $fl-truncate/)
  (import (rsc-core-syntax) (rvm-primitives)))
