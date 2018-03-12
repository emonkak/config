" Vim additional syntax: scheme
" Version: 0.0.1
" Copyright (C) 2012 emonkak <emonkak@gmail.com>
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}




" syntax match schemeSharpBang '^#!.*'
" syntax region schemeStringLiteral start='#`"' end='"'
" syntax match schemeBinaryLiteral '#b[01]\+'
" syntax match schemeOctalLiteral '#o[0-7]\+'
" syntax match schemeDecimalLiteral '#d\d\+'
" syntax match schemeHexadecimalLiteral '#x\x\+'
"
" highlight link schemeSharpBang Preproc
" highlight link schemeStringLiteral String
"
" highlight link schemeBinaryLiteral Number
" highlight link schemeOctalLiteral Number
" highlight link schemeDecimalLiteral Number
" highlight link schemeHexadecimalLiteral Number

if get(g:, 'lisp_rainbow')
  syntax cluster schemeListCluster contains=ALLBUT,schemeParen0,schemeParen2,schemeParen3,schemeParen4,schemeParen5,schemeParen6,schemeParen7,schemeParen8,schemeParen9,schemeParen10

  syntax region schemeParen0           matchgroup=hlLevel0 start="#\=(" end=")" contains=@schemeListCluster,schemeParen1
  syntax region schemeParen1 contained matchgroup=hlLevel1 start="#\=(" end=")" contains=@schemeListCluster,schemeParen2
  syntax region schemeParen2 contained matchgroup=hlLevel2 start="#\=(" end=")" contains=@schemeListCluster,schemeParen3
  syntax region schemeParen3 contained matchgroup=hlLevel3 start="#\=(" end=")" contains=@schemeListCluster,schemeParen4
  syntax region schemeParen4 contained matchgroup=hlLevel4 start="#\=(" end=")" contains=@schemeListCluster,schemeParen5
  syntax region schemeParen5 contained matchgroup=hlLevel5 start="#\=(" end=")" contains=@schemeListCluster,schemeParen6
  syntax region schemeParen6 contained matchgroup=hlLevel6 start="#\=(" end=")" contains=@schemeListCluster,schemeParen7
  syntax region schemeParen7 contained matchgroup=hlLevel7 start="#\=(" end=")" contains=@schemeListCluster,schemeParen8
  syntax region schemeParen8 contained matchgroup=hlLevel8 start="#\=(" end=")" contains=@schemeListCluster,schemeParen9
  syntax region schemeParen9 contained matchgroup=hlLevel9 start="#\=(" end=")" contains=@schemeListCluster,schemeParen0

  highlight default hlLevel0 ctermfg=red      guifg=red1
  highlight default hlLevel1 ctermfg=yellow   guifg=orange1
  highlight default hlLevel2 ctermfg=green    guifg=yellow1
  highlight default hlLevel3 ctermfg=cyan     guifg=greenyellow
  highlight default hlLevel4 ctermfg=magenta  guifg=green1
  highlight default hlLevel5 ctermfg=red      guifg=springgreen1
  highlight default hlLevel6 ctermfg=yellow   guifg=cyan1
  highlight default hlLevel7 ctermfg=green    guifg=slateblue1
  highlight default hlLevel8 ctermfg=cyan     guifg=magenta1
  highlight default hlLevel9 ctermfg=magenta  guifg=purple1
else
  " syntax clear schemeStruc
  " syntax region schemeStruc matchgroup=SpecialKey start="#\=(" matchgroup=SpecialKey end=")" contains=ALL
  " syntax region schemeStruc matchgroup=SpecialKey start="#\=\[" matchgroup=SpecialKey end="\]" contains=ALL
endif




" __END__
" vim: foldmethod=marker
