" Vim colorscheme: basic256
" Version: 0.0.0
" Copyright (C) 2010-2012 emonkak <emonkak@gmail.com>
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
" Init  "{{{1

highlight clear
set background=dark

let g:colors_name = expand('<sfile>:t:r')




" Variables  "{{{1

let s:foreground_color = '#e8eaeb'
let s:background_color = '#21272b'
let s:basic_colors = [
\   '#363f45',
\   '#f7554d',
\   '#10a184',
\   '#c97a12',
\   '#1c95e6',
\   '#f74d90',
\   '#9d77fc',
\   '#869096',
\   '#657078',
\   '#ffb6b3',
\   '#38e0bf',
\   '#f7ba6a',
\   '#8dd0fc',
\   '#fcb3d0',
\   '#cfbdff',
\   '#c5c6c7',
\ ]

function! s:xterm_256colors() "{{{
  let _ = []
  let [r, g, b] = [0, 0, 0]

  for i in range(240, 25, -1)
    let rgb = [r, g, b]

    for j in range(3)
      if rgb[j] != 0
        let rgb[j] = (rgb[j] * 40) + 55
      endif
    endfor

    let b += 1
    if b > 5
      let b = 0
      let g += 1
    endif
    if g > 5
      let g = 0
      let r += 1
    endif

    call insert(_, '#' . join(map(rgb, 'printf("%02x", v:val)'), ''))
  endfor

  for i in range(24, 1, -1)
    let rgb = []

    for j in range(3)
      call add(rgb, 8 + (24 - i) * 10)
    endfor

    call add(_, '#' . join(map(rgb, 'printf("%02x", v:val)'), ''))
  endfor

  return _
endfunction  "}}}

let s:color_table = s:basic_colors + s:xterm_256colors()




" Utilities  "{{{1
function! s:attributes(expr) "{{{2
  if &term ==# 'win32'
    let ATTRS = {
    \   'r': 'reverse',
    \   's': 'standout',
    \ }
  else
    let ATTRS = {
    \   'b': 'bold',
    \   'c': 'undercurl',
    \   'i': 'italic',
    \   'r': 'reverse',
    \   's': 'standout',
    \   'u': 'underline',
    \ }
  endif
  let attrs = []
  for key in split(a:expr, '.\zs')
    if has_key(ATTRS, key)
      call insert(attrs, ATTRS[key])
    endif
  endfor
  return empty(attrs) ? 'NONE' : join(attrs, ',')
endfunction




function! s:color(color)  "{{{2
  if &term ==# 'win32'
    let TABLE = [0, 4, 2, 6, 1, 5, 3, 7]
    return TABLE[a:color % len(TABLE)]
  else
    return a:color % &t_Co
  endif
endfunction




function! s:gui_color(color)  "{{{2
  return s:color_table[a:color % len(s:color_table)]
endfunction




function! s:highlight(name, config)  "{{{2
  let args = []
  let reversed_p = 0

  if has_key(a:config, 'attr')
    let reversed_p = &term ==# 'win32' && a:config['attr'] =~# 'r'
    call insert(args, 'cterm=' . s:attributes(a:config['attr']))
    call insert(args, 'gui=' . s:attributes(a:config['attr']))
  endif
  if has_key(a:config, 'fg')
    call insert(args, (reversed_p ? 'ctermbg=' : 'ctermfg=') . s:color(a:config['fg']))
    call insert(args, (reversed_p ? 'guibg=' : 'guifg=') . s:gui_color(a:config['fg']))
  endif
  if has_key(a:config, 'bg')
    call insert(args, (reversed_p ? 'ctermfg=' : 'ctermbg=') . s:color(a:config['bg']))
    call insert(args, (reversed_p ? 'guifg=' : 'guibg=') . s:gui_color(a:config['bg']))
  endif
  if has_key(a:config, 'sp')
    call insert(args, 'guisp=' . s:gui_color(a:config['sp']))
  endif

  execute 'highlight' a:name 'NONE' join(args)
endfunction




" Highlights  "{{{1
" Basic  "{{{2

if has('gui_running')
  execute 'highlight Normal' 'guifg='.s:foreground_color 'guibg='.s:background_color
else
  call s:highlight('Normal', {})
endif

call s:highlight('ColorColumn'  , {'bg': 8})
call s:highlight('Conceal'      , {'fg': 8})
call s:highlight('Cursor'       , {'bg': 2})
call s:highlight('CursorColumn' , {'bg': 0})
call s:highlight('CursorIM'     , {'bg': 14})
call s:highlight('CursorLine'   , {'bg': 0})
call s:highlight('DiffAdd'      , {'bg': 4})
call s:highlight('DiffChange'   , {'bg': 5})
call s:highlight('DiffDelete'   , {'fg': 8})
call s:highlight('Directory'    , {'fg': 14})
call s:highlight('DiffText'     , {'bg': 5})
call s:highlight('ErrorMsg'     , {'bg': 1})
call s:highlight('VertSplit'    , {'attr': 'r', 'fg': 8})
call s:highlight('Folded'       , {'attr': 'i', 'fg': 6})
call s:highlight('FoldColumn'   , {'fg': 6})
call s:highlight('SignColumn'   , {'fg': 14})
call s:highlight('IncSearch'    , {'attr': 'r'})
call s:highlight('LineNr'       , {'fg': 8})
call s:highlight('CursorLineNr' , {'bg': 0})
call s:highlight('MatchParen'   , {'attr': 'b', 'fg': 0, 'bg': 14})
call s:highlight('ModeMsg'      , {'bg': 4})
call s:highlight('MoreMsg'      , {'bg': 2})
call s:highlight('NonText'      , {'fg': 8})
call s:highlight('Pmenu'        , {'bg': 0})
call s:highlight('PmenuSbar'    , {})
call s:highlight('PmenuSel'     , {'attr': 'r', 'fg': 11})
call s:highlight('PmenuThumb'   , {'bg': 11})
call s:highlight('Question'     , {'fg': 10})
call s:highlight('Search'       , {'attr': 'r', 'fg': 11})
call s:highlight('SpecialKey'   , {'fg': 8})
if has('gui_running')
  call s:highlight('SpellBad'   , {'attr': 'c', 'sp': 1})
  call s:highlight('SpellCap'   , {'attr': 'c', 'sp': 4})
  call s:highlight('SpellRare'  , {'attr': 'c', 'sp': 5})
  call s:highlight('SpellLocal' , {'attr': 'c', 'sp': 6})
else
  call s:highlight('SpellBad'   , {'bg': 1})
  call s:highlight('SpellCap'   , {'bg': 4})
  call s:highlight('SpellRare'  , {'bg': 5})
  call s:highlight('SpellLocal' , {'bg': 6})
endif
call s:highlight('StatusLine'   , {'attr': 'b', 'bg': 0})
call s:highlight('StatusLineNC' , {'bg': 8})
call s:highlight('TabLine'      , {'bg': 0})
call s:highlight('TabLineFill'  , {'bg': 0})
call s:highlight('TabLineSel'   , {'attr': 'bu', 'bg': 0})
call s:highlight('Title'        , {'fg': 14})
call s:highlight('Visual'       , {'bg': 4})
call s:highlight('VisualNOS'    , {'attr': 'r'})
call s:highlight('WarningMsg'   , {'fg': 11})
call s:highlight('WildMenu'     , {'attr': 'br', 'fg': 11})
call s:highlight('lCursor'      , {'bg': 14})




" Syntax  "{{{2

call s:highlight('Comment'      , {'attr': 'i', 'fg': 7})
call s:highlight('Constant'     , {'fg': 13})
call s:highlight('Identifier'   , {'fg': 14})
call s:highlight('Statement'    , {'fg': 11})
call s:highlight('PreProc'      , {'fg': 12})
call s:highlight('Type'         , {'fg': 10})
call s:highlight('Special'      , {'fg': 9})
call s:highlight('Underlined'   , {'attr': 'u', 'fg': 12})
call s:highlight('Ignore'       , {'fg': 0})
call s:highlight('Error'        , {'bg': 1})
call s:highlight('Todo'         , {'attr': 'u', 'fg': 11})

call s:highlight('LspError'               , {'fg': 9})
call s:highlight('LspErrorText'           , {'fg': 9})
call s:highlight('LspErrorHighlight'      , {'attr': 'c'})
call s:highlight('LspWarning'             , {'fg': 9})
call s:highlight('LspWarningText'         , {'fg': 9})
call s:highlight('LspWarningHighlight'    , {'attr': 'c'})
call s:highlight('LspInformation'         , {'fg': 12})
call s:highlight('LspInformationText'     , {'fg': 12})
call s:highlight('LspInformationHighlight', {'attr': 'c'})
call s:highlight('LspHint'                , {'fg': 12})
call s:highlight('LspHintText'            , {'fg': 12})
call s:highlight('LspHintHighlight'       , {'attr': 'c'})





" __END__  "{{{1
" vim: foldmethod=marker
