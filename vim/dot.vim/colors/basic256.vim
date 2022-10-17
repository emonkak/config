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

  let fg = has_key(a:definition, 'fg') ? s:color(a:definition['fg']) : -1
  let bg = has_key(a:definition, 'bg') ? s:color(a:definition['bg']) : -1
  let sp = has_key(a:definition, 'sp') ? s:color(a:definition['sp']) : -1

  if has_key(a:definition, 'attr')
    if &term ==# 'win32' && a:definition['attr'] =~# 'r'
      let tmp = fg;
      let fg = bg;
      let bg = tmp;
    endif
    call insert(args, 'cterm=' . s:attributes(a:definition['attr']))
    call insert(args, 'gui=' . s:attributes(a:definition['attr']))
  endif

  if fg >= 0
    call insert(args, 'ctermfg=' . s:term_color(fg))
    call insert(args, 'guifg=' . s:gui_color(fg))
  elseif has_key(a:definition, 'fg')
    call insert(args, 'guifg=' . a:definition['fg'])
  endi

  if bg >= 0
    call insert(args, 'ctermbg=' . s:term_color(bg))
    call insert(args, 'guibg=' . s:gui_color(bg))
  elseif has_key(a:definition, 'bg')
    call insert(args, 'guibg=' . a:definition['bg'])
  endif

  if sp >= 0
    call insert(args, 'guisp=' . s:gui_color(sp))
  endif

  execute 'highlight' a:name 'NONE' join(args)
endfunction

function! s:attributes(input) abort
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

function! s:color(color) abort
  if type(a:color) == type(0)
    return a:color
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
  return get(COLOR_TABLE, a:color, -1)
endfunction

function! s:term_color(color) abort
  if &term ==# 'win32'
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
