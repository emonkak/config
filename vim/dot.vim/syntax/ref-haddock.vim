" Vim syntax: haddock
" Version: 0.0.0
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

if exists('b:current_syntax')
  finish
endif



syntax keyword haddockKeyword data class module type
syntax match haddockHeadline "^\S.\+"
syntax match haddockHeadline "^\s\+Arguments$"
syntax match haddockHeadline "^\s\+Contents$"
syntax match haddockHeadline "^\s\+Description$"
syntax match haddockHeadline "^\s\+Instances$"
syntax match haddockHeadline "^\s\+Synopsis$"
syntax match haddockList "^\s\+\s\*"
syntax match haddockCode "^\s\s\S.\+"
syntax match haddockFunction "([-!#$%&\*\+/<=>\?@\\^|~:.]\+)\s\ze::"
syntax match haddockFunction "[a-z][a-zA-Z0-9_']\+\s\ze::"
syntax match haddockVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syntax match haddockConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syntax match haddockVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syntax match haddockConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"


highlight default link haddockKeyword Type
highlight default link haddockList PreProc
highlight default link haddockCode String
highlight default link haddockFunction Identifier
highlight default link haddockHeadline Type
highlight default link haddockVarSym Operator
highlight default link haddockConSym Operator




let b:current_syntax = 'haddock'

" __END__
" vim: foldmethod=marker
