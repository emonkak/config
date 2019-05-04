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

function! PHPFold(lnum)  "{{{
  let current = getline(a:lnum)

  if current =~# '\s*}$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 3 ? '=' : '<' . level
  endif

  if current =~# '^\s*\('
             \ . '\(\(final\|private\|protected\|public\|static\)\s\)*function'
             \ . '\|\(\(abstract\|final\)\s\)*class'
             \ . '\|interface'
             \ . '\|namespace'
             \ . '\|trait'
             \ . '\)[^;]*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 3 ? '=' : level
  endif

  return '='
endfunction "}}}

command! -range -nargs=0 -buffer PhpInsertAccessors
\ <line1>,<line2>call s:insert_accessors()
command! -range -nargs=0 -buffer PhpInsertGetters
\ <line1>,<line2>call s:insert_getters()
command! -range -nargs=0 -buffer PhpInsertSetters
\ <line1>,<line2>call s:insert_setters()

function! s:insert_accessors() range  " {{{
  let properties = s:collect_properties(a:firstline, a:lastline)

  call cursor(a:lastline, 1)

  call s:render_getters(properties)
  call s:render_setters(properties)

  call cursor(a:firstline, 1)
endfunction  "}}}

function! s:insert_getters() range  " {{{
  let properties = s:collect_properties(a:firstline, a:lastline)

  call cursor(a:lastline, 1)

  call s:render_getters(properties)

  call cursor(a:firstline, 1)
endfunction  "}}}

function! s:insert_setters() range  " {{{
  let properties = s:collect_properties(a:firstline, a:lastline)

  call cursor(a:lastline, 1)

  call s:render_setters(properties)

  call cursor(a:firstline, 1)
endfunction  "}}}

function! s:render_getters(properties)  "{{{
  for property in a:properties
    let [_, name, type] = property
    silent put =''
    silent put =s:create_getter('public', name, type)
  endfor
endfunction  "}}}

function! s:render_setters(properties)  "{{{
  for property in a:properties
    let [visibility, name, type] = property
    silent put =''
    silent put =s:create_setter(visibility, name, type)
  endfor
endfunction  "}}}

function! s:create_getter(visibility, name, type)  " {{{
  return join([
  \    '    ' . a:visibility . ' function get' . s:camelize(a:name) . '(): ' . a:type,
  \    '    {',
  \    '        return $this->' . a:name . ';',
  \    '    }'
  \ ], "\n")
endfunction  "}}}

function! s:create_setter(visibility, name, type)  " {{{
  return join([
  \   '    ' . a:visibility . ' function set' . s:camelize(a:name) . '(' . a:type . ' $' . a:name . '): void',
  \   '    {',
  \   '        $this->' . a:name . ' = ' . '$' . a:name . ';',
  \   '    }'
  \ ], "\n")
endfunction  "}}}

function! s:collect_properties(first, last)  " {{{
  let properties = []
  let types = []

  for line_num in range(a:first, a:last)
    let line = getline(line_num)
    let matches = matchlist(line, '@var\s\+\(\S\+\)')
    if len(matches) > 0
      call add(types, matches[1])
    endif

    let matches = matchlist(line, '\(private\|public\|protected\)\s\+\$\(\w\+\)')
    if len(matches) > 0
      let type = len(types) > 0 ? remove(types, 0) : 'mixed'
      call add(properties, [matches[1], matches[2], type])
    endif
  endfor

  return properties
endfunction  "}}}

function! s:camelize(word)  " {{{
    return substitute(a:word,
    \                 '\%([_-]\|^\)\([a-z]\)\([a-z]\+\)',
    \                 '\=toupper(submatch(1)) . tolower(submatch(2))',
    \                 'gi')
endfunction  "}}}

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal foldmethod< foldexpr<'

" __END__
" vim: foldmethod=marker
