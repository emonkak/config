" Vim colorscheme: basic
" Copyright (C) 2010 emonkak <http://github.com/emonkak/>
" Absolute  "{{{1

highlight clear
set background=dark

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "basic"




" Variables  "{{{1

let s:gui_running = has('gui_running')
let s:gui_ansi_colors = [
\ '#000000', '#990000', '#009900', '#999900',
\ '#000099', '#990099', '#009999', '#999999',
\ '#555555', '#cc6633', '#00cc00', '#cccc00',
\ '#3366cc', '#cc00cc', '#00cccc', '#ffffff',
\ ]




" Utilities  "{{{1
function! s:highlight_attributes(attributes)  "{{{2
  let _ = {
  \ 'b': 'bold',
  \ 'i': 'italic',
  \ 'r': 'reverse',
  \ 'u': 'underline',
  \ 'c': 'undercurl',
  \ }
  return join(map(split(a:attributes, '.\zs'), 'get(_, v:val, "")'), ',')
endfunction




function! s:highlight(name, fg, bg, ...)  "{{{2
  if s:gui_running
    let prefix = 'gui'
    let fg = a:fg > -1 ? get(s:gui_ansi_colors, a:fg, 'NONE') : 'NONE'
    let bg = a:bg > -1 ? get(s:gui_ansi_colors, a:bg, 'NONE') : 'NONE'
  else
    let prefix = 'cterm'
    let fg = a:fg > -1 ? a:fg : 'NONE'
    let bg = a:bg > -1 ? a:bg : 'NONE'
  endif
  let attr = exists('a:1') && len(a:1) ? s:highlight_attributes(a:1) : 'NONE'
  execute 'highlight' a:name
  \        prefix.'fg='.fg
  \        prefix.'bg='.bg
  \        prefix.'='.attr
endfunction




" General colors  "{{{1

if s:gui_running
  highlight Normal   guifg=#cccccc guibg=#222222
  highlight Cursor   guifg=#000000 guibg=#009900
  highlight CursorIM guifg=#000000 guibg=#009999
  highlight lCursor  guifg=#000000 guibg=#009999
endif

call s:highlight('SpecialKey'   ,  8, -1)
call s:highlight('NonText'      ,  4, -1)
call s:highlight('Directory'    , 14, -1)
call s:highlight('MatchParen'   , -1,  6)
call s:highlight('LineNr'       ,  8, -1)
call s:highlight('Question'     , 10, -1)
call s:highlight('VertSplit'    ,  8, -1)
call s:highlight('Title'        , 14, -1)
call s:highlight('Visual'       , -1,  4)
call s:highlight('VisualNOS'    , -1, -1, 'r')
call s:highlight('WildMenu'     ,  0, 11)

call s:highlight('ErrorMsg'     , -1,  1)
call s:highlight('MoreMsg'      , -1,  2)
call s:highlight('ModeMsg'      , -1,  4)
call s:highlight('WarningMsg'   , 11, -1)

call s:highlight('IncSearch'    , -1, -1, 'r')
call s:highlight('Search'       ,  0, 11)

call s:highlight('StatusLine'   , -1,  0, 'r')
call s:highlight('StatusLineNC' ,  7,  0, 'r')

call s:highlight('Folded'       ,  6, -1)
call s:highlight('FoldColumn'   ,  6, -1)
call s:highlight('SignColumn'   , 14, -1)

call s:highlight('DiffAdd'      , -1,  4)
call s:highlight('DiffChange'   , -1,  5)
call s:highlight('DiffDelete'   ,  8, -1)
call s:highlight('DiffText'     , -1,  5)

call s:highlight('SpellBad'     , -1,  1)
call s:highlight('SpellCap'     , -1,  4)
call s:highlight('SpellRare'    , -1,  5)
call s:highlight('SpellLocal'   , -1,  6)

call s:highlight('Pmenu'        , -1, -1, 'u')
call s:highlight('PmenuSel'     ,  0, 11)
call s:highlight('PmenuSbar'    , -1, -1)
call s:highlight('PmenuThumb'   , -1, 11)

call s:highlight('TabLine'      , -1,  0, 'r')
call s:highlight('TabLineSel'   , 15, -1)
call s:highlight('TabLineFill'  , -1,  0, 'r')

call s:highlight('CursorColumn' , -1,  0)
call s:highlight('CursorLine'   , -1,  0)




" Syntax highlighting  "{{{1

call s:highlight('Comment'      , 14, -1)
call s:highlight('Constant'     , 13, -1)
call s:highlight('Special'      ,  9, -1)
call s:highlight('Identifier'   , 14, -1, 'b')
call s:highlight('Statement'    , 11, -1)
call s:highlight('PreProc'      , 12, -1)
call s:highlight('Type'         , 10, -1)
call s:highlight('Underlined'   , 12, -1, 'u')
call s:highlight('Ignore'       ,  0, -1)
call s:highlight('Error'        , -1,  1)
call s:highlight('Todo'         ,  0, 11)

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
