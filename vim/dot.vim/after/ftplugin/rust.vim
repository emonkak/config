" Vim additional ftplugin: rust
" Version: 0.0.0
" Copyright (C) 2021 emonkak <emonkak@gmail.com>
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

setlocal foldmethod=expr foldexpr=RustFold(v:lnum)

function! RustFold(lnum)
  let current = getline(a:lnum)

  if current =~# '^\s*}\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level <= 2 ? '<' . level : '='
  endif

  if current =~# '^\s*\('
  \            . 'macro_rules!\|'
  \            . 'impl\|'
  \            . '\(pub\(\s*(\s*crate\s*)\)\?\s\+\)\?\(async\s\+\)\?\(unsafe\s\+\)\?\(extern\s\+"C"\s\+\)\?\(enum\|\(const\s\+\)\?fn\|mod\|struct\|trait\|union\)'
  \            . '\)'
  \  && current !~# '[;}]\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level <= 2 ? level : '='
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= ''

" __END__
" vim: foldmethod=marker
