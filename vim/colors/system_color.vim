highlight clear

set background=dark

let g:colors_name = expand('<sfile>:t:r')

let s:ATTR_TABLE = {
\   'b': 'bold',
\   'c': 'undercurl',
\   'i': 'italic',
\   'r': 'reverse',
\   's': 'standout',
\   'u': 'underline',
\ }

let s:NR16_COLOR_TABLE = {
\   'black': 0,
\   'darkred': 1,
\   'darkgreen': 2,
\   'darkyellow': 3,
\   'darkblue': 4,
\   'darkmagenta': 5,
\   'darkcyan': 6,
\   'lightgray': 7,
\   'darkgray': 8,
\   'lightred': 9,
\   'lightgreen': 10,
\   'lightyellow': 11,
\   'lightblue': 12,
\   'lightmagenta': 13,
\   'lightcyan': 14,
\   'white': 15,
\ }

let s:NR8_COLOR_TABLE = {
\   'black': 0,
\   'darkred': 4,
\   'darkgreen': 2,
\   'darkyellow': 6,
\   'darkblue': 1,
\   'darkmagenta': 5,
\   'darkcyan': 3,
\   'lightgray': 7,
\   'darkgray': 0,
\   'lightred': 4,
\   'lightgreen': 2,
\   'lightyellow': 6,
\   'lightblue': 1,
\   'lightmagenta': 5,
\   'lightcyan': 3,
\   'white': 7,
\ }

if !exists('g:system_color_gui_color_table')
  let g:system_color_gui_color_table = {
  \   'foreground': 'White',
  \   'background': 'Black',
  \   'black': 'Black',
  \   'darkred': 'DarkRed',
  \   'darkgreen': 'DarkGreen',
  \   'darkyellow': 'DarkYellow',
  \   'darkblue': 'DarkBlue',
  \   'darkmagenta': 'DarkMagenta',
  \   'darkcyan': 'DarkCyan',
  \   'lightgray': 'Gray',
  \   'darkgray': 'DarkGray',
  \   'lightred': 'Red',
  \   'lightgreen': 'Green',
  \   'lightyellow': 'Yellow',
  \   'lightblue': 'Blue',
  \   'lightmagenta': 'Magenta',
  \   'lightcyan': 'Cyan',
  \   'white': 'White',
  \ }
endif

function! s:highlight_all(definitions) abort
  for definition in a:definitions
    call s:highlight(definition)
  endfor
endfunction

function! s:highlight(definition) abort
  let args = []

  if has_key(a:definition, 'attr')
    let attrs = s:attributes(a:definition['attr'])
    call insert(args, 'cterm=' . attrs)
    call insert(args, 'gui=' . attrs)
  else
    call insert(args, 'cterm=NONE')
    call insert(args, 'gui=NONE')
  endif

  if has_key(a:definition, 'fg')
    call insert(args, 'ctermfg=' . s:term_color(a:definition['fg']))
    call insert(args, 'guifg=' . s:gui_color(a:definition['fg']))
  endi

  if has_key(a:definition, 'bg')
    call insert(args, 'ctermbg=' . s:term_color(a:definition['bg']))
    call insert(args, 'guibg=' . s:gui_color(a:definition['bg']))
  endif

  if has_key(a:definition, 'sp')
    call insert(args, 'guisp=' . s:gui_color(a:definition['sp']))
  endif

  execute 'highlight' a:definition.group 'NONE' join(args)
endfunction

function! s:attributes(tags) abort
  let attrs = []
  for tag in split(a:tags, '.\zs')
    if has_key(s:ATTR_TABLE, tag)
      call insert(attrs, s:ATTR_TABLE[tag])
    endif
  endfor
  return empty(attrs) ? 'NONE' : join(attrs, ',')
endfunction

function! s:term_color(color) abort
  if &t_Co < 16
    return get(s:NR8_COLOR_TABLE, a:color, 'NONE')
  else
    return get(s:NR16_COLOR_TABLE, a:color, 'NONE')
  endif
endfunction

function! s:gui_color(color) abort
  return get(g:system_color_gui_color_table, a:color, 'NONE')
endfunction

call s:highlight_all([
\   { 'group': 'ColorColumn', 'bg': 'black' },
\   { 'group': 'Comment', 'attr': 'i', 'fg': 'lightgray' },
\   { 'group': 'Conceal', 'fg': 'darkgray' },
\   { 'group': 'Constant', 'fg': 'lightmagenta' },
\   { 'group': 'Cursor', 'bg': 'darkgreen' },
\   { 'group': 'CursorColumn', 'bg': 'black' },
\   { 'group': 'CursorIM', 'bg': 'lightcyan' },
\   { 'group': 'CursorLine', 'bg': 'black' },
\   { 'group': 'CursorLineNr', 'bg': 'black' },
\   { 'group': 'DiffAdd', 'bg': 'darkblue' },
\   { 'group': 'DiffChange', 'bg': 'darkmagenta' },
\   { 'group': 'DiffDelete', 'fg': 'darkgray' },
\   { 'group': 'DiffText', 'bg': 'darkmagenta' },
\   { 'group': 'Directory', 'fg': 'lightcyan' },
\   { 'group': 'Error', 'bg': 'darkred' },
\   { 'group': 'ErrorMsg', 'bg': 'darkred' },
\   { 'group': 'FoldColumn', 'fg': 'darkcyan' },
\   { 'group': 'Folded', 'attr': 'i', 'fg': 'darkcyan' },
\   { 'group': 'Identifier', 'fg': 'lightcyan' },
\   { 'group': 'Ignore', 'fg': 'black' },
\   { 'group': 'IncSearch', 'attr': 'r' },
\   { 'group': 'LineNr', 'fg': 'darkgray' },
\   { 'group': 'LspError', 'fg': 'lightred' },
\   { 'group': 'LspErrorHighlight', 'attr': 'c' },
\   { 'group': 'LspErrorText', 'fg': 'lightred' },
\   { 'group': 'LspHint', 'fg': 'lightblue' },
\   { 'group': 'LspHintHighlight', 'attr': 'c' },
\   { 'group': 'LspHintText', 'fg': 'lightblue' },
\   { 'group': 'LspInformation', 'fg': 'lightblue' },
\   { 'group': 'LspInformationHighlight', 'attr': 'c' },
\   { 'group': 'LspInformationText', 'fg': 'lightblue' },
\   { 'group': 'LspWarning', 'fg': 'lightred' },
\   { 'group': 'LspWarningHighlight', 'attr': 'c' },
\   { 'group': 'LspWarningText', 'fg': 'lightred' },
\   { 'group': 'MatchParen', 'attr': 'b', 'fg': 'black', 'bg': 'lightcyan' },
\   { 'group': 'ModeMsg', 'bg': 'darkblue' },
\   { 'group': 'NormalFloat' },
\   { 'group': 'MoreMsg', 'bg': 'darkgreen' },
\   { 'group': 'NonText', 'fg': 'darkgray' },
\   { 'group': 'Pmenu', 'bg': 'black' },
\   { 'group': 'PmenuSbar', 'bg': 'darkgray' },
\   { 'group': 'PmenuSel', 'attr': 'r', 'fg': 'lightyellow' },
\   { 'group': 'PmenuThumb', 'bg': 'lightyellow' },
\   { 'group': 'PreProc', 'fg': 'lightblue' },
\   { 'group': 'Question', 'fg': 'lightgreen' },
\   { 'group': 'Search', 'attr': 'r', 'fg': 'lightyellow' },
\   { 'group': 'SignColumn', 'fg': 'lightcyan' },
\   { 'group': 'Special', 'fg': 'lightred' },
\   { 'group': 'SpecialKey', 'fg': 'darkgray' },
\   { 'group': 'Statement', 'fg': 'lightyellow' },
\   { 'group': 'StatusLine', 'attr': 'b', 'bg': 'black' },
\   { 'group': 'StatusLineNC', 'bg': 'darkgray' },
\   { 'group': 'StatusLineTerm', 'attr': 'br', 'fg': 'lightgreen' },
\   { 'group': 'StatusLineTermNC', 'attr': 'r', 'fg': 'darkgreen' },
\   { 'group': 'TabLine', 'bg': 'black' },
\   { 'group': 'TabLineFill', 'bg': 'black' },
\   { 'group': 'TabLineSel', 'attr': 'bu', 'bg': 'black' },
\   { 'group': 'Title', 'fg': 'lightcyan' },
\   { 'group': 'Todo', 'attr': 'u', 'fg': 'lightyellow' },
\   { 'group': 'ToolbarButton', 'attr': 'b', 'bg': 'darkgray' },
\   { 'group': 'ToolbarLine', 'bg': 'black' },
\   { 'group': 'Type', 'fg': 'lightgreen' },
\   { 'group': 'Underlined', 'attr': 'u', 'fg': 'lightblue' },
\   { 'group': 'VertSplit', 'fg': 'darkgray' },
\   { 'group': 'Visual', 'bg': 'darkblue' },
\   { 'group': 'VisualNOS', 'attr': 'r' },
\   { 'group': 'WarningMsg', 'fg': 'lightyellow' },
\   { 'group': 'WildMenu', 'attr': 'br', 'fg': 'lightyellow' },
\   { 'group': 'WinSeparator', 'fg': 'darkgray' },
\   { 'group': 'lCursor', 'bg': 'lightcyan' },
\ ] + (has('gui_running') ? [
\   { 'group': 'Normal', 'fg': 'foreground', 'bg': 'background' },
\   { 'group': 'SpellBad', 'attr': 'c', 'sp': 'darkred' },
\   { 'group': 'SpellCap', 'attr': 'c', 'sp': 'darkblue' },
\   { 'group': 'SpellLocal', 'attr': 'c', 'sp': 'darkcyan' },
\   { 'group': 'SpellRare', 'attr': 'c', 'sp': 'darkmagenta' },
\ ] : [
\   { 'group': 'SpellBad', 'bg': 'darkred' },
\   { 'group': 'SpellCap', 'bg': 'darkblue' },
\   { 'group': 'SpellLocal', 'bg': 'darkcyan' },
\   { 'group': 'SpellRare', 'bg': 'darkmagenta' },
\ ]))
