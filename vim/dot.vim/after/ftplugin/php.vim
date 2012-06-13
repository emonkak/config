" Vim additional ftplugin: php
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

setlocal foldmethod=expr foldexpr=PHPFold(v:lnum)

function! PHPFold(lnum)
  let current = getline(a:lnum)

  if current =~# '\(}\|[])];\)$'
    let level = indent(a:lnum) / &l:shiftwidth
    return level > 1 ? '=' : '<' . (level + 1)
  elseif current =~# '^\s*\('
                 \ . '\(\(private\|protected\|public\|static\|var\)\s\)*\$\w\+\s*=\s*\(array(\|[\)$'
                 \ . '\|\(\(final\|private\|protected\|public\|static\)\s\)*function'
                 \ . '\|\(abstract\s\)\?class'
                 \ . '\|interface'
                 \ . '\|trait'
                 \ . '\)'
    let level = indent(a:lnum) / &l:shiftwidth
    return level > 1 ? '=' : '>' . (level + 1)
  else
    return '='
  endif
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin = ''
else
  let b:undo_ftplugin = ' | '
endif

let b:undo_ftplugin .= 'setlocal foldmethod< foldexpr<'

" __END__
" vim: foldmethod=marker
