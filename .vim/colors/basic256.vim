" Vim colorscheme: basic256
" Version: 0.0.0
" Copyright (C) 2010 emonkak <emonkak@gmail.com>
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
\  '#000000', '#990000', '#009900', '#999900',
\  '#000099', '#990099', '#009999', '#999999',
\  '#555555', '#cc6633', '#00cc00', '#cccc00',
\  '#3366cc', '#cc00cc', '#00cccc', '#ffffff',
\ ]
let s:gui_colors += [
\  '#000000', '#00005f', '#000087', '#0000af',
\  '#0000d7', '#0000ff', '#005f00', '#005f5f',
\  '#005f87', '#005faf', '#005fd7', '#005fff',
\  '#008700', '#00875f', '#008787', '#0087af',
\  '#0087d7', '#0087ff', '#00af00', '#00af5f',
\  '#00af87', '#00afaf', '#00afd7', '#00afff',
\  '#00d700', '#00d75f', '#00d787', '#00d7af',
\  '#00d7d7', '#00d7ff', '#00ff00', '#00ff5f',
\  '#00ff87', '#00ffaf', '#00ffd7', '#00ffff',
\  '#5f0000', '#5f005f', '#5f0087', '#5f00af',
\  '#5f00d7', '#5f00ff', '#5f5f00', '#5f5f5f',
\  '#5f5f87', '#5f5faf', '#5f5fd7', '#5f5fff',
\  '#5f8700', '#5f875f', '#5f8787', '#5f87af',
\  '#5f87d7', '#5f87ff', '#5faf00', '#5faf5f',
\  '#5faf87', '#5fafaf', '#5fafd7', '#5fafff',
\  '#5fd700', '#5fd75f', '#5fd787', '#5fd7af',
\  '#5fd7d7', '#5fd7ff', '#5fff00', '#5fff5f',
\  '#5fff87', '#5fffaf', '#5fffd7', '#5fffff',
\  '#870000', '#87005f', '#870087', '#8700af',
\  '#8700d7', '#8700ff', '#875f00', '#875f5f',
\  '#875f87', '#875faf', '#875fd7', '#875fff',
\  '#878700', '#87875f', '#878787', '#8787af',
\  '#8787d7', '#8787ff', '#87af00', '#87af5f',
\  '#87af87', '#87afaf', '#87afd7', '#87afff',
\  '#87d700', '#87d75f', '#87d787', '#87d7af',
\  '#87d7d7', '#87d7ff', '#87ff00', '#87ff5f',
\  '#87ff87', '#87ffaf', '#87ffd7', '#87ffff',
\  '#af0000', '#af005f', '#af0087', '#af00af',
\  '#af00d7', '#af00ff', '#af5f00', '#af5f5f',
\  '#af5f87', '#af5faf', '#af5fd7', '#af5fff',
\  '#af8700', '#af875f', '#af8787', '#af87af',
\  '#af87d7', '#af87ff', '#afaf00', '#afaf5f',
\  '#afaf87', '#afafaf', '#afafd7', '#afafff',
\  '#afd700', '#afd75f', '#afd787', '#afd7af',
\  '#afd7d7', '#afd7ff', '#afff00', '#afff5f',
\  '#afff87', '#afffaf', '#afffd7', '#afffff',
\  '#d70000', '#d7005f', '#d70087', '#d700af',
\  '#d700d7', '#d700ff', '#d75f00', '#d75f5f',
\  '#d75f87', '#d75faf', '#d75fd7', '#d75fff',
\  '#d78700', '#d7875f', '#d78787', '#d787af',
\  '#d787d7', '#d787ff', '#d7af00', '#d7af5f',
\  '#d7af87', '#d7afaf', '#d7afd7', '#d7afff',
\  '#d7d700', '#d7d75f', '#d7d787', '#d7d7af',
\  '#d7d7d7', '#d7d7ff', '#d7ff00', '#d7ff5f',
\  '#d7ff87', '#d7ffaf', '#d7ffd7', '#d7ffff',
\  '#ff0000', '#ff005f', '#ff0087', '#ff00af',
\  '#ff00d7', '#ff00ff', '#ff5f00', '#ff5f5f',
\  '#ff5f87', '#ff5faf', '#ff5fd7', '#ff5fff',
\  '#ff8700', '#ff875f', '#ff8787', '#ff87af',
\  '#ff87d7', '#ff87ff', '#ffaf00', '#ffaf5f',
\  '#ffaf87', '#ffafaf', '#ffafd7', '#ffafff',
\  '#ffd700', '#ffd75f', '#ffd787', '#ffd7af',
\  '#ffd7d7', '#ffd7ff', '#ffff00', '#ffff5f',
\  '#ffff87', '#ffffaf', '#ffffd7', '#ffffff',
\  '#080808', '#121212', '#1c1c1c', '#262626',
\  '#303030', '#3a3a3a', '#444444', '#4e4e4e',
\  '#585858', '#626262', '#6c6c6c', '#767676',
\  '#808080', '#8a8a8a', '#949494', '#9e9e9e',
\  '#a8a8a8', '#b2b2b2', '#bcbcbc', '#c6c6c6',
\  '#d0d0d0', '#dadada', '#e4e4e4', '#eeeeee',
\ ]




" Utilities  "{{{1
function! s:attributes(attr)  "{{{2
  let _ = {
  \ 'b': 'bold',
  \ 'c': 'undercurl',
  \ 'i': 'italic',
  \ 'r': 'reverse',
  \ 's': 'standout',
  \ 'u': 'underline',
  \ }
  return a:attr == '' ? 'NONE' :
  \      join(map(split(a:attr, '.\zs'), 'get(_, v:val, "")'), ',')
endfunction




function! s:highlight(name, attr, ...)  "{{{2
  let _ = ['fg=', 'bg=', 'sp=']
  if has('gui_running')
    let args = a:000[:2]
    let type = 'gui'
    call map(args,
    \       'type . remove(_, 0) . (v:val > -1 ? s:gui_colors[v:val % 255] : "NONE")')
  else
    let args = a:000[:1]
    let type = 'cterm'
    call map(args,
    \       'type . remove(_, 0) . (v:val > -1 ? v:val % &t_Co : "NONE")')
  endif
  execute 'highlight' a:name
  \       type . '=' . s:attributes(a:attr)
  \       join(args)
endfunction




" Highlighting  "{{{1
" Basic  "{{{2

if has('gui_running')
  highlight Normal   guifg=#cccccc guibg=#111111
  highlight Cursor   guifg=#000000 guibg=#009900
  highlight CursorIM guifg=#000000 guibg=#009999
  highlight lCursor  guifg=#000000 guibg=#009999
endif

call s:highlight('SpecialKey'   , ''  ,  8, -1)
call s:highlight('NonText'      , ''  ,  4, -1)
call s:highlight('Directory'    , ''  , 14, -1)
call s:highlight('MatchParen'   , 'b' , 14, -1)
call s:highlight('LineNr'       , ''  ,  8, -1)
call s:highlight('Question'     , ''  , 10, -1)
call s:highlight('VertSplit'    , ''  ,  8, -1)
call s:highlight('Title'        , ''  , 14, -1)
call s:highlight('Visual'       , ''  , -1,  4)
call s:highlight('VisualNOS'    , 'r' , -1, -1)
call s:highlight('WildMenu'     , 'br', 11,  0)

call s:highlight('ErrorMsg'     , ''  , -1,  1)
call s:highlight('MoreMsg'      , ''  , -1,  2)
call s:highlight('ModeMsg'      , ''  , -1,  4)
call s:highlight('WarningMsg'   , ''  , 11, -1)

call s:highlight('IncSearch'    , 'r' , -1, -1)
call s:highlight('Search'       , ''  ,  0, 11)

call s:highlight('StatusLine'   , 'br', -1,  0)
call s:highlight('StatusLineNC' , ''  ,  0,  7)

call s:highlight('Folded'       , ''  ,  6, -1)
call s:highlight('FoldColumn'   , ''  ,  6, -1)
call s:highlight('SignColumn'   , ''  , 14, -1)
call s:highlight('Conceal'      , ''  , -1,  8)

call s:highlight('DiffAdd'      , ''  , -1,  4)
call s:highlight('DiffChange'   , ''  , -1,  5)
call s:highlight('DiffDelete'   , ''  ,  8, -1)
call s:highlight('DiffText'     , ''  , -1,  5)

if has('gui_running')
  call s:highlight('SpellBad'   , 'c' , -1, -1,  1)
  call s:highlight('SpellCap'   , 'c' , -1, -1,  4)
  call s:highlight('SpellRare'  , 'c' , -1, -1,  5)
  call s:highlight('SpellLocal' , 'c' , -1, -1,  6)
else
  call s:highlight('SpellBad'   , ''  , -1,  1)
  call s:highlight('SpellCap'   , ''  , -1,  4)
  call s:highlight('SpellRare'  , ''  , -1,  5)
  call s:highlight('SpellLocal' , ''  , -1,  6)
endif

call s:highlight('Pmenu'        , 'u' , -1, -1)
call s:highlight('PmenuSel'     , ''  ,  0, 11)
call s:highlight('PmenuSbar'    , ''  , -1, -1)
call s:highlight('PmenuThumb'   , ''  , -1, 11)

call s:highlight('TabLine'      , 'r' , -1,  0)
call s:highlight('TabLineSel'   , 'b' , -1, -1)
call s:highlight('TabLineFill'  , 'r' , -1,  0)

call s:highlight('CursorColumn' , ''  , -1,  0)
call s:highlight('CursorLine'   , 'u' , -1, -1)
call s:highlight('ColorColumn'  , ''  , -1,  8)




" Syntax  "{{{2

call s:highlight('Comment'      , ''  ,  14, -1)
call s:highlight('Constant'     , ''  ,  13, -1)
call s:highlight('Special'      , ''  ,   9, -1)
call s:highlight('Identifier'   , 'b' ,  14, -1)
call s:highlight('Statement'    , ''  ,  11, -1)
call s:highlight('PreProc'      , ''  ,  12, -1)
call s:highlight('Type'         , ''  ,  10, -1)
call s:highlight('Underlined'   , 'u' ,  12, -1)
call s:highlight('Ignore'       , ''  ,   0, -1)
call s:highlight('Error'        , ''  ,  -1,  1)
call s:highlight('Todo'         , ''  ,   0, 11)

hi link String         Constant
hi link Character      Constant
hi link Number         Constant
hi link Boolean        Constant
hi link Float          Constant
hi link Function       Identifier
hi link Conditional    Statement
hi link Repeat         Statement
hi link Label          Statement
hi link Operator       Statement
hi link Keyword        Statement
hi link Exception      Statement
hi link Include        PreProc
hi link Define         PreProc
hi link Macro          PreProc
hi link PreCondit      PreProc
hi link StorageClass   Type
hi link Structure      Type
hi link Typedef        Type
hi link Tag            Special
hi link SpecialChar    Special
hi link Delimiter      Special
hi link SpecialComment Special
hi link Debug          Special




" __END__  "{{{1
" vim: foldmethod=marker
