" Vim additional ftplugin: scheme
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

setlocal commentstring=;%s

setlocal lispwords-=and
setlocal lispwords-=or
setlocal lispwords-=if
setlocal lispwords-=cond
setlocal lispwords-=case
setlocal lispwords-=else

setlocal lispwords+=call-with-input-file
setlocal lispwords+=call-with-output-file
setlocal lispwords+=call-with-input-string
setlocal lispwords+=call-with-output-string
setlocal lispwords+=call-with-builder
setlocal lispwords+=call-with-iterator
setlocal lispwords+=call-with-string-io
setlocal lispwords+=call-with-current-continuation
setlocal lispwords+=call-with-iterators
setlocal lispwords+=call-with-values

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal commentstring< lispwords<'

" __END__
" vim: foldmethod=marker
