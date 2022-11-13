highlight clear

set background=dark

let g:colors_name = expand('<sfile>:t:r')

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

function! s:h(group, definition) abort
  let args = []
  let gui_colors = g:system_color_gui_color_table
  let term_colors = &t_Co < 16 ? s:NR8_COLOR_TABLE : s:NR16_COLOR_TABLE

  if has_key(a:definition, 'attr')
    let attr = join(a:definition.attr, ',')
    call insert(args, 'cterm=' . attr)
    call insert(args, 'gui=' . attr)
  else
    call insert(args, 'cterm=NONE')
    call insert(args, 'gui=NONE')
  endif

  if has_key(a:definition, 'fg')
    call insert(args, 'ctermfg=' . get(term_colors, a:definition.fg, 'NONE'))
    call insert(args, 'guifg=' . get(gui_colors, a:definition.fg, 'NONE'))
  endi

  if has_key(a:definition, 'bg')
    call insert(args, 'ctermbg=' . get(term_colors, a:definition.bg, 'NONE'))
    call insert(args, 'guibg=' . get(gui_colors, a:definition.bg, 'NONE'))
  endif

  if has_key(a:definition, 'sp')
    call insert(args, 'guisp=' . get(gui_colors, a:definition.sp, 'NONE'))
  endif

  execute 'highlight' a:group 'NONE' join(args)
endfunction

call s:h('ColorColumn', { 'bg': 'black' })
call s:h('Comment', { 'attr': ['italic'], 'fg': 'lightgray' })
call s:h('Conceal', { 'fg': 'darkgray' })
call s:h('Constant', { 'fg': 'lightmagenta' })
call s:h('Cursor', { 'bg': 'darkgreen' })
call s:h('CursorColumn', { 'bg': 'black' })
call s:h('CursorIM', { 'bg': 'lightcyan' })
call s:h('CursorLine', { 'bg': 'black' })
call s:h('CursorLineNr', { 'bg': 'black' })
call s:h('DiffAdd', { 'bg': 'darkblue' })
call s:h('DiffChange', { 'bg': 'darkmagenta' })
call s:h('DiffDelete', { 'fg': 'darkgray' })
call s:h('DiffText', { 'bg': 'darkmagenta' })
call s:h('Directory', { 'fg': 'lightcyan' })
call s:h('Error', { 'bg': 'darkred' })
call s:h('ErrorMsg', { 'bg': 'darkred' })
call s:h('FoldColumn', { 'fg': 'darkcyan' })
call s:h('Folded', { 'attr': ['italic'], 'fg': 'darkcyan' })
call s:h('Identifier', { 'fg': 'lightcyan' })
call s:h('Ignore', { 'fg': 'black' })
call s:h('IncSearch', { 'attr': ['reverse'] })
call s:h('LineNr', { 'fg': 'darkgray' })
call s:h('LspError', { 'fg': 'lightred' })
call s:h('LspErrorHighlight', { 'attr': ['undercurl'] })
call s:h('LspErrorText', { 'fg': 'lightred' })
call s:h('LspHint', { 'fg': 'lightblue' })
call s:h('LspHintHighlight', { 'attr': ['undercurl'] })
call s:h('LspHintText', { 'fg': 'lightblue' })
call s:h('LspInformation', { 'fg': 'lightblue' })
call s:h('LspInformationHighlight', { 'attr': ['undercurl'] })
call s:h('LspInformationText', { 'fg': 'lightblue' })
call s:h('LspWarning', { 'fg': 'lightred' })
call s:h('LspWarningHighlight', { 'attr': ['undercurl'] })
call s:h('LspWarningText', { 'fg': 'lightred' })
call s:h('MatchParen', { 'attr': ['bold', 'reverse'] })
call s:h('ModeMsg', { 'bg': 'darkblue' })
call s:h('NormalFloat', {})
call s:h('MoreMsg', { 'bg': 'darkgreen' })
call s:h('NonText', { 'fg': 'darkgray' })
call s:h('Whitespace', { 'fg': 'black' })
call s:h('Pmenu', { 'bg': 'black' })
call s:h('PmenuSbar', { 'bg': 'darkgray' })
call s:h('PmenuSel', { 'attr': ['reverse'], 'fg': 'lightyellow' })
call s:h('PmenuThumb', { 'bg': 'lightyellow' })
call s:h('PreProc', { 'fg': 'lightblue' })
call s:h('Question', { 'fg': 'lightgreen' })
call s:h('Search', { 'attr': ['reverse'], 'fg': 'lightyellow' })
call s:h('SignColumn', { 'fg': 'lightcyan' })
call s:h('Special', { 'fg': 'lightred' })
call s:h('SpecialKey', { 'fg': 'darkgray' })
call s:h('Statement', { 'fg': 'lightyellow' })
call s:h('StatusLine', { 'attr': ['bold'], 'bg': 'black' })
call s:h('StatusLineNC', { 'bg': 'darkgray' })
call s:h('StatusLineTerm', { 'attr': ['bold', 'reverse'], 'fg': 'lightgreen' })
call s:h('StatusLineTermNC', { 'attr': ['reverse'], 'fg': 'darkgreen' })
call s:h('TabLine', { 'bg': 'black' })
call s:h('TabLineFill', { 'bg': 'black' })
call s:h('TabLineSel', { 'attr': ['bold', 'underline'], 'bg': 'black' })
call s:h('Title', { 'fg': 'lightcyan' })
call s:h('Todo', { 'attr': ['underline'], 'fg': 'lightyellow' })
call s:h('ToolbarButton', { 'attr': ['bold'], 'bg': 'darkgray' })
call s:h('ToolbarLine', { 'bg': 'black' })
call s:h('Type', { 'fg': 'lightgreen' })
call s:h('Underlined', { 'attr': ['underline'], 'fg': 'lightblue' })
call s:h('VertSplit', { 'fg': 'darkgray' })
call s:h('Visual', { 'bg': 'darkblue' })
call s:h('VisualNOS', { 'attr': ['reverse'] })
call s:h('WarningMsg', { 'fg': 'lightyellow' })
call s:h('WildMenu', { 'attr': ['bold', 'reverse'], 'fg': 'lightyellow' })
call s:h('WinSeparator', { 'fg': 'darkgray' })
call s:h('lCursor', { 'bg': 'lightcyan' })

if has('gui_running')
  call s:h('Normal', { 'fg': 'foreground', 'bg': 'background' })
  call s:h('SpellBad', { 'attr': ['undercurl'], 'sp': 'darkred' })
  call s:h('SpellCap', { 'attr': ['undercurl'], 'sp': 'darkblue' })
  call s:h('SpellLocal', { 'attr': ['undercurl'], 'sp': 'darkcyan' })
  call s:h('SpellRare', { 'attr': ['undercurl'], 'sp': 'darkmagenta' })
else
  call s:h('SpellBad', { 'bg': 'darkred' })
  call s:h('SpellCap', { 'bg': 'darkblue' })
  call s:h('SpellLocal', { 'bg': 'darkcyan' })
  call s:h('SpellRare', { 'bg': 'darkmagenta' })
endif
