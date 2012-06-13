" quickrun outputter: clipboard
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
" Variables  "{{{1

let s:outputter = {}




" Interface  "{{{1
function! s:outputter.output(data, session)
  if has('win32unix')
    call writefile(split(a:data, "\x0A", 1), '/dev/clipboard', 'b')
  elseif executable('pbcopy')
    call system('pbcopy', a:data)
  elseif executable('xsel')
    call system('xsel -b', a:data)
  elseif executable('xclip')
    call system('xclip -o', a:data)
  elseif has('clipboard')
    let @+ = a:data
  else
    echoerr 'Yanking into the clipboard is not supported.'
  endif
endfunction




function! quickrun#outputter#clipboard#new()  "{{{2
  return deepcopy(s:outputter)
endfunction




" __END__  "{{{1
" vim: foldmethod=marker
