" Vim additional syntax: scheme
" Version: 0.0.0
" Copyright (C) 2010 emonkak <emonkak@gmail.com>
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




syntax match schemeSharpBang '^#!.*'
syntax region schemeStringLiteral start='#`"' end='"'
syntax match schemeBinaryLiteral '#b[01]\+'
syntax match schemeOctalLiteral '#o[0-7]\+'
syntax match schemeDecimalLiteral '#d\d\+'
syntax match schemeHexadecimalLiteral '#x\x\+'

highlight link schemeSharpBang Preproc
highlight link schemeStringLiteral String

highlight link schemeBinaryLiteral Number
highlight link schemeOctalLiteral Number
highlight link schemeDecimalLiteral Number
highlight link schemeHexadecimalLiteral Number




" __END__
" vim: foldmethod=marker
