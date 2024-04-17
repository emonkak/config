" Fix <svg> and <math> highlighting
syntax clear htmlTagN
syntax match htmlTagN
\ '<[-a-zA-Z0-9]\+'hs=s+1
\ contained
\ contains=htmlTagName,htmlMathTagName,htmlSvgTagName,htmlSpecialTagName,@htmlTagNameCluster
syntax match htmlTagN
\ '</[-a-zA-Z0-9]\+'hs=s+2
\ contained
\ contains=htmlTagName,htmlMathTagName,htmlSvgTagName,htmlSpecialTagName,@htmlTagNameCluster
