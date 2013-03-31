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

if exists('syntax_on')
  syntax reset
endif

let g:colors_name = 'basic256'




" Variables  "{{{1

let s:_ = {}
let s:_.color_table = [
\   '#292929', '#c41700', '#679b00', '#c49300',
\   '#074080', '#9b0059', '#007676', '#818181',
\   '#474747', '#ff614c', '#b9f73e', '#ffd24c',
\   '#4581c4', '#dd429a', '#37bbbb', '#ffffff',
\ ]




" Utilities  "{{{1
function! s:_.attributes(expr) dict "{{{2
  let _ = &term ==# 'win32' ? {
  \   'r': 'reverse',
  \   's': 'standout',
  \ } : {
  \   'b': 'bold',
  \   'c': 'undercurl',
  \   'i': 'italic',
  \   'r': 'reverse',
  \   's': 'standout',
  \   'u': 'underline',
  \ }
  let attrs = []
  for key in split(a:expr, '.\zs')
    if has_key(_, key)
      call insert(attrs, _[key])
    endif
  endfor
  return empty(attrs) ? 'NONE' : join(attrs, ',')
endfunction




function! s:_.color(color) dict  "{{{2
  if type(a:color) == type('')
    return a:color
  elseif has('gui_running')
    return self.color_table[a:color % len(self.color_table)]
  elseif &term ==# 'win32'
    let _ = [0, 4, 2, 6, 1, 5, 3, 7, 8, 12, 10, 14, 9, 13, 11, 15]
    return _[a:color % len(_)]
  else
    return a:color % &t_Co
  endif
endfunction




function! s:_.highlight(name, config) dict  "{{{2
  let _ = []
  let type = has('gui_running') ? 'gui' : 'cterm'
  let reversed_p = 0

  if has_key(a:config, 'attr')
    if &term ==# 'win32' && a:config['attr'] =~# 'r'
      let reversed_p = !0
    endif
    call insert(_, ['', self.attributes(a:config['attr'])])
  endif
  if has_key(a:config, 'fg')
    call insert(_, [reversed_p ? 'bg' : 'fg', self.color(a:config['fg'])])
  endif
  if has_key(a:config, 'bg')
    call insert(_, [reversed_p ? 'fg' : 'bg', self.color(a:config['bg'])])
  endif
  if has_key(a:config, 'sp') && has('gui_running')
    call insert(_, ['sp', self.color(a:config['sp'])])
  endif

  execute 'highlight' a:name 'NONE' join(map(_, 'type . join(v:val, "=")'))
endfunction




function! s:_.xterm_colors() dict "{{{2
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

    call insert(_, '#'.join(map(rgb, 'printf("%02x", v:val)'), ''))
  endfor

  for i in range(24, 1, -1)
    let rgb = []

    for j in range(3)
      call add(rgb, 8 + (24 - i) * 10)
    endfor

    call add(_, '#'.join(map(rgb, 'printf("%02x", v:val)'), ''))
  endfor

  return _
endfunction

call extend(s:_.color_table, s:_.xterm_colors())




" Highlighting  "{{{1
" Basic  "{{{2

if has('gui_running')
  call s:_.highlight('Normal'     , {'fg': '#e2e2e2', 'bg': '#171717'})
  call s:_.highlight('Cursor'     , {'bg': 2})
  call s:_.highlight('CursorIM'   , {'bg': 14})
  call s:_.highlight('lCursor'    , {'bg': 14})
else
  call s:_.highlight('Normal'     , {})
endif

call s:_.highlight('SpecialKey'   , {'fg': 8})
call s:_.highlight('NonText'      , {'fg': 12})
call s:_.highlight('Directory'    , {'fg': 14})
call s:_.highlight('MatchParen'   , {'attr': 'b', 'fg': 0, 'bg': 14})
call s:_.highlight('LineNr'       , {'fg': 8})
call s:_.highlight('CursorLineNr' , {'bg': 0, 'fg': 7})
call s:_.highlight('Question'     , {'fg': 10})
call s:_.highlight('VertSplit'    , {'attr': 'r', 'fg': 8})
call s:_.highlight('Title'        , {'fg': 14})
call s:_.highlight('Visual'       , {'bg': 4})
call s:_.highlight('VisualNOS'    , {'attr': 'r'})
call s:_.highlight('WildMenu'     , {'attr': 'br', 'fg': 11})

call s:_.highlight('ErrorMsg'     , {'bg': 1})
call s:_.highlight('MoreMsg'      , {'bg': 2})
call s:_.highlight('ModeMsg'      , {'bg': 4})
call s:_.highlight('WarningMsg'   , {'fg': 11})

call s:_.highlight('IncSearch'    , {'attr': 'r'})
call s:_.highlight('Search'       , {'attr': 'r', 'fg': 11})

call s:_.highlight('StatusLine'   , {'attr': 'b', 'bg': 8, 'fg': 15})
call s:_.highlight('StatusLineNC' , {'bg': 8})

call s:_.highlight('Folded'       , {'fg': 6})
call s:_.highlight('FoldColumn'   , {'fg': 6})
call s:_.highlight('SignColumn'   , {'fg': 14})
call s:_.highlight('Conceal'      , {'bg': 8})

call s:_.highlight('DiffAdd'      , {'bg': 4})
call s:_.highlight('DiffChange'   , {'bg': 5})
call s:_.highlight('DiffDelete'   , {'fg': 8})
call s:_.highlight('DiffText'     , {'bg': 5})

if has('gui_running')
  call s:_.highlight('SpellBad'   , {'attr': 'c', 'sp': 1})
  call s:_.highlight('SpellCap'   , {'attr': 'c', 'sp': 4})
  call s:_.highlight('SpellRare'  , {'attr': 'c', 'sp': 5})
  call s:_.highlight('SpellLocal' , {'attr': 'c', 'sp': 6})
else
  call s:_.highlight('SpellBad'   , {'bg': 1})
  call s:_.highlight('SpellCap'   , {'bg': 4})
  call s:_.highlight('SpellRare'  , {'bg': 5})
  call s:_.highlight('SpellLocal' , {'bg': 6})
endif

call s:_.highlight('Pmenu'        , {'attr': 'u'})
call s:_.highlight('PmenuSel'     , {'attr': 'r', 'fg': 11})
call s:_.highlight('PmenuSbar'    , {})
call s:_.highlight('PmenuThumb'   , {'bg': 11})

call s:_.highlight('TabLine'      , {'bg': 8})
call s:_.highlight('TabLineSel'   , {'attr': 'bu', 'fg': 15, 'bg': 8})
call s:_.highlight('TabLineFill'  , {'bg': 8})

call s:_.highlight('CursorColumn' , {'bg': 0})
call s:_.highlight('CursorLine'   , {'bg': 0})
call s:_.highlight('ColorColumn'  , {'bg': 8})




" Syntax  "{{{2

call s:_.highlight('Comment'      , {'fg': 14})
call s:_.highlight('Constant'     , {'fg': 13})
call s:_.highlight('Special'      , {'fg': 9})
call s:_.highlight('Identifier'   , {'attr': 'b', 'fg': 14})
call s:_.highlight('Statement'    , {'fg': 11})
call s:_.highlight('PreProc'      , {'fg': 12})
call s:_.highlight('Type'         , {'fg': 10})
call s:_.highlight('Underlined'   , {'attr': 'u', 'fg': 12})
call s:_.highlight('Ignore'       , {'fg': 0})
call s:_.highlight('Error'        , {'bg': 1})
call s:_.highlight('Todo'         , {'attr': 'u', 'fg': 11})

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




" Fin.  "{{{1

unlet s:_




" __END__  "{{{1
" vim: foldmethod=marker
