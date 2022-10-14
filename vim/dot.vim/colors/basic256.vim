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

let s:gui_fg_color = '#e8eaeb'
let s:gui_bg_color = '#21272b'
let s:gui_colors = [
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




" Utilities  "{{{1
function! s:highlight_config(config) abort  "{{{2
  for name in keys(a:config)
    call s:highlight(name, a:config[name])
  endfor
endfunction




function! s:highlight(name, definition) abort  "{{{2
  let args = []

  let fg_no = has_key(a:definition, 'fg') ? s:color_no(a:definition['fg']) : -1
  let bg_no = has_key(a:definition, 'bg') ? s:color_no(a:definition['bg']) : -1
  let sp_no = has_key(a:definition, 'sp') ? s:color_no(a:definition['sp']) : -1

  if has_key(a:definition, 'attr')
    if &term ==# 'win32' && a:definition['attr'] =~# 'r'
      let tmp = fg_no;
      let fg_no = bg_no;
      let bg_no = tmp;
    endif
    call insert(args, 'cterm=' . s:attributes(a:definition['attr']))
    call insert(args, 'gui=' . s:attributes(a:definition['attr']))
  endif

  if fg_no >= 0
    call insert(args, 'ctermfg=' . s:term_color(fg_no))
    call insert(args, 'guifg=' . s:gui_color(fg_no))
  elseif has_key(a:definition, 'fg')
    call insert(args, 'guifg=' . a:definition['fg'])
  endi

  if bg_no >= 0
    call insert(args, 'ctermbg=' . s:term_color(bg_no))
    call insert(args, 'guibg=' . s:gui_color(bg_no))
  elseif has_key(a:definition, 'bg')
    call insert(args, 'guibg=' . a:definition['bg'])
  endif

  if sp_no >= 0
    call insert(args, 'guisp=' . s:gui_color(sp_no))
  endif

  execute 'highlight' a:name 'NONE' join(args)
endfunction




function! s:attributes(input) abort  "{{{2
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
  for key in split(a:input, '.\zs')
    if has_key(ATTRS, key)
      call insert(attrs, ATTRS[key])
    endif
  endfor
  return empty(attrs) ? 'NONE' : join(attrs, ',')
endfunction




function! s:color_no(color_name) abort  "{{{2
  if type(a:color_name) == type(0)
    return a:color_name
  endif
  let COLOR_TABLE = {
  \   'black': 0,
  \   'red': 1,
  \   'green': 2,
  \   'yellow': 3,
  \   'blue': 4,
  \   'magenta': 5,
  \   'cyan': 6,
  \   'white': 7,
  \   'bright_black': 8,
  \   'bright_red': 9,
  \   'bright_green': 10,
  \   'bright_yellow': 11,
  \   'bright_blue': 12,
  \   'bright_magenta': 13,
  \   'bright_cyan': 14,
  \   'bright_white': 15,
  \ }
  return get(COLOR_TABLE, a:color_name, -1)
endfunction




function! s:term_color(color_no) abort  "{{{2
  if &term ==# 'win32'
    let INDEX_TABLE = [0, 4, 2, 6, 1, 5, 3, 7]
    return INDEX_TABLE[a:color_no % len(INDEX_TABLE)]
  else
    return a:color_no % &t_Co
  endif
endfunction




function! s:gui_color(color_no) abort  "{{{2
  if a:color_no >= 16 && len(s:gui_colors) < 256
    let s:gui_colors += s:xterm_256colors()
  endif
  return s:gui_colors[a:color_no % len(s:gui_colors)]
endfunction




function! s:xterm_256colors()  abort "{{{2
  let colors = []
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

    call insert(colors, '#' . join(map(rgb, 'printf("%02x", v:val)'), ''))
  endfor

  for i in range(24, 1, -1)
    let rgb = []

    for j in range(3)
      call add(rgb, 8 + (24 - i) * 10)
    endfor

    call add(colors, '#' . join(map(rgb, 'printf("%02x", v:val)'), ''))
  endfor

  return colors
endfunction




" Highlights  "{{{1

call s:highlight_config({
\   'Normal':                  {'fg': s:gui_fg_color, 'bg': s:gui_bg_color },
\   'ColorColumn':             {'bg': 'black'},
\   'Conceal':                 {'fg': 'bright_black'},
\   'Cursor':                  {'bg': 'green'},
\   'CursorColumn':            {'bg': 'black'},
\   'CursorIM':                {'bg': 'bright_cyan'},
\   'CursorLine':              {'bg': 'black'},
\   'DiffAdd':                 {'bg': 'blue'},
\   'DiffChange':              {'bg': 'magenta'},
\   'DiffDelete':              {'fg': 'bright_black'},
\   'Directory':               {'fg': 'bright_cyan'},
\   'DiffText':                {'bg': 'magenta'},
\   'ErrorMsg':                {'bg': 'red'},
\   'VertSplit':               {'attr': 'r', 'fg': 'bright_black'},
\   'Folded':                  {'attr': 'i', 'fg': 'cyan'},
\   'FoldColumn':              {'fg': 'cyan'},
\   'SignColumn':              {'fg': 'bright_cyan'},
\   'IncSearch':               {'attr': 'r'},
\   'LineNr':                  {'fg': 'bright_black'},
\   'CursorLineNr':            {'bg': 'black'},
\   'MatchParen':              {'attr': 'b', 'fg': 'black', 'bg': 'bright_cyan'},
\   'ModeMsg':                 {'bg': 'blue'},
\   'MoreMsg':                 {'bg': 'green'},
\   'NonText':                 {'fg': 'bright_black'},
\   'Pmenu':                   {'bg': 'black'},
\   'PmenuSbar':               {'bg': 'bright_black'},
\   'PmenuSel':                {'attr': 'r', 'fg': 'bright_yellow'},
\   'PmenuThumb':              {'bg': 'bright_yellow'},
\   'Question':                {'fg': 'bright_green'},
\   'Search':                  {'attr': 'r', 'fg': 'bright_yellow'},
\   'SpecialKey':              {'fg': 'bright_black'},
\
\   'SpellBad':                has('gui_running') ? {'attr': 'c', 'sp': 'red'} : {'bg': 'red'},
\   'SpellCap':                has('gui_running') ? {'attr': 'c', 'sp': 'blue'} : {'bg': 'blue'},
\   'SpellRare':               has('gui_running') ? {'attr': 'c', 'sp': 'magenta'} : {'bg': 'magenta'},
\   'SpellLocal':              has('gui_running') ? {'attr': 'c', 'sp': 'cyan'} : {'bg': 'cyan'},
\
\   'StatusLine':              {'attr': 'b', 'bg': 'black'},
\   'StatusLineNC':            {'bg': 'bright_black'},
\   'StatusLineTerm':          {'attr': 'br', 'fg': 'bright_green'},
\   'StatusLineTermNC':        {'attr': 'r', 'fg': 'green'},
\   'TabLine':                 {'bg': 'black'},
\   'TabLineFill':             {'bg': 'black'},
\   'TabLineSel':              {'attr': 'bu', 'bg': 'black'},
\   'ToolbarLine':             {'bg': 'black'},
\   'ToolbarButton':           {'attr': 'b', 'bg': 'bright_black'},
\   'Title':                   {'fg': 'bright_cyan'},
\   'Visual':                  {'bg': 'blue'},
\   'VisualNOS':               {'attr': 'r'},
\   'WarningMsg':              {'fg': 'bright_yellow'},
\   'WildMenu':                {'attr': 'br', 'fg': 'bright_yellow'},
\   'lCursor':                 {'bg': 'bright_cyan'},
\
\   'Comment':                 {'attr': 'i', 'fg': 'white'},
\   'Constant':                {'fg': 'bright_magenta'},
\   'Identifier':              {'fg': 'bright_cyan'},
\   'Statement':               {'fg': 'bright_yellow'},
\   'PreProc':                 {'fg': 'bright_blue'},
\   'Type':                    {'fg': 'bright_green'},
\   'Special':                 {'fg': 'bright_red'},
\   'Underlined':              {'attr': 'u', 'fg': 'bright_blue'},
\   'Ignore':                  {'fg': 'black'},
\   'Error':                   {'bg': 'red'},
\   'Todo':                    {'attr': 'u', 'fg': 'bright_yellow'},
\
\   'LspError':                {'fg': 'bright_red'},
\   'LspErrorText':            {'fg': 'bright_red'},
\   'LspErrorHighlight':       {'attr': 'c'},
\   'LspWarning':              {'fg': 'bright_red'},
\   'LspWarningText':          {'fg': 'bright_red'},
\   'LspWarningHighlight':     {'attr': 'c'},
\   'LspInformation':          {'fg': 'bright_blue'},
\   'LspInformationText':      {'fg': 'bright_blue'},
\   'LspInformationHighlight': {'attr': 'c'},
\   'LspHint':                 {'fg': 'bright_blue'},
\   'LspHintText':             {'fg': 'bright_blue'},
\   'LspHintHighlight':        {'attr': 'c'},
\ })




" __END__  "{{{1
" vim: foldmethod=marker
