" Vim colorscheme: basic256
" Version: 0.0.0
" Copyright (C) 2010-2011 emonkak <emonkak@gmail.com>
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

if exists('syntax_on')
  syntax reset
endif

let g:colors_name = 'basic256'




" Variables  "{{{1

let s:gui_colors = [
\   '#292929', '#c41700', '#679b00', '#c49300',
\   '#074080', '#9b0059', '#007676', '#818181',
\   '#474747', '#ff614c', '#b9f73e', '#ffd24c',
\   '#4581c4', '#dd429a', '#37bbbb', '#ffffff',
\ ] + [
\   '#000000', '#00005f', '#000087', '#0000af',
\   '#0000d7', '#0000ff', '#005f00', '#005f5f',
\   '#005f87', '#005faf', '#005fd7', '#005fff',
\   '#008700', '#00875f', '#008787', '#0087af',
\   '#0087d7', '#0087ff', '#00af00', '#00af5f',
\   '#00af87', '#00afaf', '#00afd7', '#00afff',
\   '#00d700', '#00d75f', '#00d787', '#00d7af',
\   '#00d7d7', '#00d7ff', '#00ff00', '#00ff5f',
\   '#00ff87', '#00ffaf', '#00ffd7', '#00ffff',
\   '#5f0000', '#5f005f', '#5f0087', '#5f00af',
\   '#5f00d7', '#5f00ff', '#5f5f00', '#5f5f5f',
\   '#5f5f87', '#5f5faf', '#5f5fd7', '#5f5fff',
\   '#5f8700', '#5f875f', '#5f8787', '#5f87af',
\   '#5f87d7', '#5f87ff', '#5faf00', '#5faf5f',
\   '#5faf87', '#5fafaf', '#5fafd7', '#5fafff',
\   '#5fd700', '#5fd75f', '#5fd787', '#5fd7af',
\   '#5fd7d7', '#5fd7ff', '#5fff00', '#5fff5f',
\   '#5fff87', '#5fffaf', '#5fffd7', '#5fffff',
\   '#870000', '#87005f', '#870087', '#8700af',
\   '#8700d7', '#8700ff', '#875f00', '#875f5f',
\   '#875f87', '#875faf', '#875fd7', '#875fff',
\   '#878700', '#87875f', '#878787', '#8787af',
\   '#8787d7', '#8787ff', '#87af00', '#87af5f',
\   '#87af87', '#87afaf', '#87afd7', '#87afff',
\   '#87d700', '#87d75f', '#87d787', '#87d7af',
\   '#87d7d7', '#87d7ff', '#87ff00', '#87ff5f',
\   '#87ff87', '#87ffaf', '#87ffd7', '#87ffff',
\   '#af0000', '#af005f', '#af0087', '#af00af',
\   '#af00d7', '#af00ff', '#af5f00', '#af5f5f',
\   '#af5f87', '#af5faf', '#af5fd7', '#af5fff',
\   '#af8700', '#af875f', '#af8787', '#af87af',
\   '#af87d7', '#af87ff', '#afaf00', '#afaf5f',
\   '#afaf87', '#afafaf', '#afafd7', '#afafff',
\   '#afd700', '#afd75f', '#afd787', '#afd7af',
\   '#afd7d7', '#afd7ff', '#afff00', '#afff5f',
\   '#afff87', '#afffaf', '#afffd7', '#afffff',
\   '#d70000', '#d7005f', '#d70087', '#d700af',
\   '#d700d7', '#d700ff', '#d75f00', '#d75f5f',
\   '#d75f87', '#d75faf', '#d75fd7', '#d75fff',
\   '#d78700', '#d7875f', '#d78787', '#d787af',
\   '#d787d7', '#d787ff', '#d7af00', '#d7af5f',
\   '#d7af87', '#d7afaf', '#d7afd7', '#d7afff',
\   '#d7d700', '#d7d75f', '#d7d787', '#d7d7af',
\   '#d7d7d7', '#d7d7ff', '#d7ff00', '#d7ff5f',
\   '#d7ff87', '#d7ffaf', '#d7ffd7', '#d7ffff',
\   '#ff0000', '#ff005f', '#ff0087', '#ff00af',
\   '#ff00d7', '#ff00ff', '#ff5f00', '#ff5f5f',
\   '#ff5f87', '#ff5faf', '#ff5fd7', '#ff5fff',
\   '#ff8700', '#ff875f', '#ff8787', '#ff87af',
\   '#ff87d7', '#ff87ff', '#ffaf00', '#ffaf5f',
\   '#ffaf87', '#ffafaf', '#ffafd7', '#ffafff',
\   '#ffd700', '#ffd75f', '#ffd787', '#ffd7af',
\   '#ffd7d7', '#ffd7ff', '#ffff00', '#ffff5f',
\   '#ffff87', '#ffffaf', '#ffffd7', '#ffffff',
\   '#080808', '#121212', '#1c1c1c', '#262626',
\   '#303030', '#3a3a3a', '#444444', '#4e4e4e',
\   '#585858', '#626262', '#6c6c6c', '#767676',
\   '#808080', '#8a8a8a', '#949494', '#9e9e9e',
\   '#a8a8a8', '#b2b2b2', '#bcbcbc', '#c6c6c6',
\   '#d0d0d0', '#dadada', '#e4e4e4', '#eeeeee',
\ ]




" Utilities  "{{{1
function! s:attributes(expr)  "{{{2
  let _ = &term ==# 'win32' ? {
  \ 'r': 'reverse',
  \ 's': 'standout',
  \ } : {
  \ 'b': 'bold',
  \ 'c': 'undercurl',
  \ 'i': 'italic',
  \ 'r': 'reverse',
  \ 's': 'standout',
  \ 'u': 'underline',
  \ }
  let attrs = []
  for key in split(a:expr, '.\zs')
    if has_key(_, key)
      call insert(attrs, _[key])
    endif
  endfor
  return empty(attrs) ? 'NONE' : join(attrs, ',')
endfunction




function! s:color(color)  "{{{2
  if type(a:color) == type('')
    return a:color
  elseif has('gui_running')
    return s:gui_colors[a:color % len(s:gui_colors)]
  elseif &term ==# 'win32'
    let _ = [0, 4, 2, 6, 1, 5, 3, 7, 8, 12, 10, 14, 9, 13, 11, 15]
    return _[a:color % len(_)]
  else
    return a:color % &t_Co
  endif
endfunction




function! s:highlight(name, settings)  "{{{2
  let _ = []
  let type = has('gui_running') ? 'gui' : 'cterm'
  let reversed_p = 0

  if has_key(a:settings, 'attr')
    if &term ==# 'win32' && a:settings['attr'] =~# 'r'
      let reversed_p = 1
    endif
    call insert(_, ['', s:attributes(a:settings['attr'])])
  endif
  if has_key(a:settings, 'fg')
    call insert(_, [reversed_p ? 'bg' : 'fg', s:color(a:settings['fg'])])
  endif
  if has_key(a:settings, 'bg')
    call insert(_, [reversed_p ? 'fg' : 'bg', s:color(a:settings['bg'])])
  endif
  if has_key(a:settings, 'sp') && has('gui_running')
    call insert(_, ['sp', s:color(a:settings['sp'])])
  endif

  execute 'highlight' a:name 'NONE' join(map(_, 'type . join(v:val, "=")'))
endfunction




" Highlighting  "{{{1
" Basic  "{{{2

if has('gui_running')
  call s:highlight('Normal'     , {'fg': '#e2e2e2', 'bg': '#171717'})
  call s:highlight('Cursor'     , {'fg': 'bg', 'bg': 'fg'})
  call s:highlight('CursorIM'   , {'fg': 'bg', 'bg': 'fg'})
  call s:highlight('lCursor'    , {'fg': 'bg', 'bg': 'fg'})
else
  call s:highlight('Normal'     , {})
endif

call s:highlight('SpecialKey'   , {'fg': 8})
call s:highlight('NonText'      , {'fg': 12})
call s:highlight('Directory'    , {'fg': 14})
call s:highlight('MatchParen'   , {'attr': 'b', 'fg': 0, 'bg': 14})
call s:highlight('LineNr'       , {'fg': 8})
call s:highlight('Question'     , {'fg': 10})
call s:highlight('VertSplit'    , {'attr': 'r'})
call s:highlight('Title'        , {'fg': 14})
call s:highlight('Visual'       , {'bg': 4})
call s:highlight('VisualNOS'    , {'attr': 'r'})
call s:highlight('WildMenu'     , {'attr': 'br', 'fg': 11})

call s:highlight('ErrorMsg'     , {'bg': 1})
call s:highlight('MoreMsg'      , {'bg': 2})
call s:highlight('ModeMsg'      , {'bg': 4})
call s:highlight('WarningMsg'   , {'fg': 11})

call s:highlight('IncSearch'    , {'attr': 'r'})
call s:highlight('Search'       , {'attr': 'r', 'fg': 11})

call s:highlight('StatusLine'   , {'attr': 'rb', 'fg': 15})
call s:highlight('StatusLineNC' , {'attr': 'r'})

call s:highlight('Folded'       , {'fg': 6})
call s:highlight('FoldColumn'   , {'fg': 6})
call s:highlight('SignColumn'   , {'fg': 14})
call s:highlight('Conceal'      , {'bg': 8})

call s:highlight('DiffAdd'      , {'bg': 4})
call s:highlight('DiffChange'   , {'bg': 5})
call s:highlight('DiffDelete'   , {'fg': 8})
call s:highlight('DiffText'     , {'bg': 5})

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

call s:highlight('Pmenu'        , {'attr': 'u'})
call s:highlight('PmenuSel'     , {'attr': 'r', 'fg': 11})
call s:highlight('PmenuSbar'    , {})
call s:highlight('PmenuThumb'   , {'bg': 11})

call s:highlight('TabLine'      , {'bg': 8})
call s:highlight('TabLineSel'   , {'attr': 'bu', 'fg': 15, 'bg': 8})
call s:highlight('TabLineFill'  , {'bg': 8})

call s:highlight('CursorColumn' , {'bg': 0})
call s:highlight('CursorLine'   , {'bg': 0})
call s:highlight('ColorColumn'  , {'bg': 8})




" Syntax  "{{{2

call s:highlight('Comment'      , {'fg': 14})
call s:highlight('Constant'     , {'fg': 13})
call s:highlight('Special'      , {'fg': 9})
call s:highlight('Identifier'   , {'attr': 'b', 'fg': 14})
call s:highlight('Statement'    , {'fg': 11})
call s:highlight('PreProc'      , {'fg': 12})
call s:highlight('Type'         , {'fg': 10})
call s:highlight('Underlined'   , {'attr': 'u', 'fg': 12})
call s:highlight('Ignore'       , {'fg': 0})
call s:highlight('Error'        , {'bg': 1})
call s:highlight('Todo'         , {'attr': 'u', 'fg': 11})

highlight link String         Constant
highlight link Character      Constant
highlight link Number         Constant
highlight link Boolean        Constant
highlight link Float          Constant
highlight link Function       Identifier
highlight link Conditional    Statement
highlight link Repeat         Statement
highlight link Label          Statement
highlight link Operator       Statement
highlight link Keyword        Statement
highlight link Exception      Statement
highlight link Include        PreProc
highlight link Define         PreProc
highlight link Macro          PreProc
highlight link PreCondit      PreProc
highlight link StorageClass   Type
highlight link Structure      Type
highlight link Typedef        Type
highlight link Tag            Special
highlight link SpecialChar    Special
highlight link Delimiter      Special
highlight link SpecialComment Special
highlight link Debug          Special




" __END__  "{{{1
" vim: foldmethod=marker
