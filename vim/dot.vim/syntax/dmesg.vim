" Vim syntax: dmesg
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

if exists('b:current_syntax')
  finish
endif




syntax region dmesgBracket start='\[' end='\]' oneline
syntax region dmesgString  start='"' end='"' oneline
syntax region dmesgString  start="'" end="'" oneline
syntax match dmesgTime '^\[.\{-}\]' nextgroup=dmesgIdentifier skipwhite
syntax match dmesgIdentifier '[ 0-9A-Za-z#/_.:-]\{-}:\(\s\|$\)' contained
syntax match dmesgType '\<\w\+='


highlight link dmesgBracket Special
highlight link dmesgString String
highlight link dmesgTime PreProc
highlight link dmesgIdentifier Identifier
highlight link dmesgType Type




let b:current_syntax = 'dmesg'

" __END__
" vim: foldmethod=marker
