highlight clear

set background=dark

let g:colors_name = expand('<sfile>:t:r')

function! s:define_highlights(config) abort
  for name in keys(a:config)
    call s:highlight(name, a:config[name])
  endfor
endfunction

function! s:highlight(name, definition) abort
  let args = []

  let fg_color = has_key(a:definition, 'fg') ? s:color(a:definition['fg']) : ''
  let bg_color = has_key(a:definition, 'bg') ? s:color(a:definition['bg']) : ''
  let sp_color = has_key(a:definition, 'sp') ? s:color(a:definition['sp']) : ''

  if has_key(a:definition, 'attr')
    if &t_Co < 16 && a:definition['attr'] =~# 'r'
      let tmp = fg_color;
      let fg_color = bg_color;
      let bg_color = tmp;
    endif
    call insert(args, 'cterm=' . s:attributes(a:definition['attr']))
    call insert(args, 'gui=' . s:attributes(a:definition['attr']))
  endif

  if type(fg_color) == v:t_number
    call insert(args, 'ctermfg=' . s:term_color(fg_color))
    call insert(args, 'guifg=' . s:gui_color(fg_color))
  elseif !empty(fg_color)
    call insert(args, 'guifg=' . fg_color)
  endi

  if type(bg_color) == v:t_number
    call insert(args, 'ctermbg=' . s:term_color(bg_color))
    call insert(args, 'guibg=' . s:gui_color(bg_color))
  elseif !empty(bg_color)
    call insert(args, 'guibg=' . fg_color)
  endif

  if type(sp_color) == v:t_number
    call insert(args, 'guisp=' . s:gui_color(sp_color))
  elseif !empty(sp_color)
    call insert(args, 'guisp=' . sp_color)
  endif

  execute 'highlight' a:name 'NONE' join(args)
endfunction

function! s:attributes(input) abort
  let ATTRS = {
  \   'b': 'bold',
  \   'c': 'undercurl',
  \   'i': 'italic',
  \   'r': 'reverse',
  \   's': 'standout',
  \   'u': 'underline',
  \ }
  let attrs = []
  for key in split(a:input, '.\zs')
    if has_key(ATTRS, key)
      call insert(attrs, ATTRS[key])
    endif
  endfor
  return empty(attrs) ? 'NONE' : join(attrs, ',')
endfunction

function! s:color(color) abort
  if type(a:color) == v:t_number
    return a:color
  else
    let COLOR_TABLE = {
    \   'black': 0,
    \   'darkred': 1,
    \   'darkgreen': 2,
    \   'darkyellow': 3,
    \   'brown': 3,
    \   'darkblue': 4,
    \   'darkmagenta': 5,
    \   'darkcyan': 6,
    \   'lightgray': 7,
    \   'lightgrey': 7,
    \   'gray': 7,
    \   'grey': 7,
    \   'darkgray': 8,
    \   'darkgrey': 8,
    \   'red': 9,
    \   'lightred': 9,
    \   'green': 10,
    \   'lightgreen': 10,
    \   'yellow': 11,
    \   'lightyellow': 11,
    \   'blue': 12,
    \   'lightblue': 12,
    \   'magenta': 13,
    \   'lightmagenta': 13,
    \   'cyan': 14,
    \   'lightcyan': 14,
    \   'white': 15,
    \ }
    return get(COLOR_TABLE, tolower(a:color), a:color)
  endif
endfunction

function! s:term_color(color) abort
  if &t_Co < 16
    let INDEX_TABLE = [0, 4, 2, 6, 1, 5, 3, 7]
    return INDEX_TABLE[a:color % len(INDEX_TABLE)]
  else
    return a:color % &t_Co
  endif
endfunction

function! s:gui_color(color) abort
  return s:gui_colors[a:color % len(s:gui_colors)]
endfunction

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

call s:define_highlights({
\   'ColorColumn':             {'bg': 'black'},
\   'Comment':                 {'attr': 'i', 'fg': 'gray'},
\   'Conceal':                 {'fg': 'darkgray'},
\   'Constant':                {'fg': 'lightmagenta'},
\   'Cursor':                  {'bg': 'darkgreen'},
\   'CursorColumn':            {'bg': 'black'},
\   'CursorIM':                {'bg': 'lightcyan'},
\   'CursorLine':              {'bg': 'black'},
\   'CursorLineNr':            {'bg': 'black'},
\   'DiffAdd':                 {'bg': 'darkblue'},
\   'DiffChange':              {'bg': 'darkmagenta'},
\   'DiffDelete':              {'fg': 'darkgray'},
\   'DiffText':                {'bg': 'darkmagenta'},
\   'Directory':               {'fg': 'lightcyan'},
\   'Error':                   {'bg': 'darkred'},
\   'ErrorMsg':                {'bg': 'darkred'},
\   'FoldColumn':              {'fg': 'darkcyan'},
\   'Folded':                  {'attr': 'i', 'fg': 'darkcyan'},
\   'Identifier':              {'fg': 'lightcyan'},
\   'Ignore':                  {'fg': 'black'},
\   'IncSearch':               {'attr': 'r'},
\   'LineNr':                  {'fg': 'darkgray'},
\   'LspError':                {'fg': 'lightred'},
\   'LspErrorHighlight':       {'attr': 'c'},
\   'LspErrorText':            {'fg': 'lightred'},
\   'LspHint':                 {'fg': 'lightblue'},
\   'LspHintHighlight':        {'attr': 'c'},
\   'LspHintText':             {'fg': 'lightblue'},
\   'LspInformation':          {'fg': 'lightblue'},
\   'LspInformationHighlight': {'attr': 'c'},
\   'LspInformationText':      {'fg': 'lightblue'},
\   'LspWarning':              {'fg': 'lightred'},
\   'LspWarningHighlight':     {'attr': 'c'},
\   'LspWarningText':          {'fg': 'lightred'},
\   'MatchParen':              {'attr': 'b', 'fg': 'black', 'bg': 'lightcyan'},
\   'ModeMsg':                 {'bg': 'darkblue'},
\   'NormalFloat':             {},
\   'MoreMsg':                 {'bg': 'darkgreen'},
\   'NonText':                 {'fg': 'darkgray'},
\   'Normal':                  {'fg': s:gui_fg_color, 'bg': s:gui_bg_color },
\   'Pmenu':                   {'bg': 'black'},
\   'PmenuSbar':               {'bg': 'darkgray'},
\   'PmenuSel':                {'attr': 'r', 'fg': 'lightyellow'},
\   'PmenuThumb':              {'bg': 'lightyellow'},
\   'PreProc':                 {'fg': 'lightblue'},
\   'Question':                {'fg': 'lightgreen'},
\   'Search':                  {'attr': 'r', 'fg': 'lightyellow'},
\   'SignColumn':              {'fg': 'lightcyan'},
\   'Special':                 {'fg': 'lightred'},
\   'SpecialKey':              {'fg': 'darkgray'},
\   'SpellBad':                has('gui_running') ? {'attr': 'c', 'sp': 'darkred'} : {'bg': 'red'},
\   'SpellCap':                has('gui_running') ? {'attr': 'c', 'sp': 'darkblue'} : {'bg': 'blue'},
\   'SpellLocal':              has('gui_running') ? {'attr': 'c', 'sp': 'darkcyan'} : {'bg': 'cyan'},
\   'SpellRare':               has('gui_running') ? {'attr': 'c', 'sp': 'darkmagenta'} : {'bg': 'magenta'},
\   'Statement':               {'fg': 'lightyellow'},
\   'StatusLine':              {'attr': 'b', 'bg': 'black'},
\   'StatusLineNC':            {'bg': 'darkgray'},
\   'StatusLineTerm':          {'attr': 'br', 'fg': 'lightgreen'},
\   'StatusLineTermNC':        {'attr': 'r', 'fg': 'darkgreen'},
\   'TabLine':                 {'bg': 'black'},
\   'TabLineFill':             {'bg': 'black'},
\   'TabLineSel':              {'attr': 'bu', 'bg': 'black'},
\   'Title':                   {'fg': 'lightcyan'},
\   'Todo':                    {'attr': 'u', 'fg': 'lightyellow'},
\   'ToolbarButton':           {'attr': 'b', 'bg': 'darkgray'},
\   'ToolbarLine':             {'bg': 'black'},
\   'Type':                    {'fg': 'lightgreen'},
\   'Underlined':              {'attr': 'u', 'fg': 'lightblue'},
\   'VertSplit':               {'attr': 'r', 'fg': 'darkgray'},
\   'Visual':                  {'bg': 'darkblue'},
\   'VisualNOS':               {'attr': 'r'},
\   'WarningMsg':              {'fg': 'lightyellow'},
\   'WildMenu':                {'attr': 'br', 'fg': 'lightyellow'},
\   'WinSeparator':            {'bg': 'black'},
\   'lCursor':                 {'bg': 'lightcyan'},
\ })
