" Vim syntax: int-ghci
" Version: 0.0.0
" Copyright (C) 2011 emonkak <emonkak@gmail.com>
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
" Version: 0.0.0

if exists('b:current_syntax')
  finish
endif




syntax match ghciPrompt '^ghci>'

syntax region ghciLoading oneline start='^Loading\spackage\s' end='\sdone\.$'
syntax region ghciMessage oneline start='^<interactive>' end='$'
syntax region ghciVersion oneline start='GHCi, version [0-9.]\+' end='$'

syntax region ghciString oneline start='"' end='"'

highlight default link ghciPrompt Identifier
highlight default link ghciLoading Statement
highlight default link ghciMessage Special
highlight default link ghciVersion PreProc
highlight default link ghciString Constant




let b:current_syntax = 'int-ghci'

" __END__
" vim: foldmethod=marker
