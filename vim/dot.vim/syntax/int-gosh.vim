" Vim syntax: int-gosh
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



for file in split(globpath(&runtimepath, 'syntax/scheme.vim'), "\n")
  source `=file`
endfor

syntax match goshPrompt '^gosh>'
syntax match goshPrompt '^\.\.\.\.\.'
syntax match goshError '^\*\*\*\sERROR:\s.\+' contains=goshMaker
syntax match goshMaker '^\*\*\*\s' conceal contained
syntax match goshStackTrace '^Stack Trace:'
syntax match goshBorder '^_\+$'

highlight default link goshPrompt Identifier
highlight default link goshError Error
highlight default link goshStackTrace Special
highlight default link goshBorder SpecialKey




let b:current_syntax = 'int-gosh'

" __END__
" vim: foldmethod=marker
