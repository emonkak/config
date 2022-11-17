highlight clear

set background=dark

let g:colors_name = expand('<sfile>:t:r')

let s:NR16_COLOR_TABLE = {
\   'black': 0,
\   'red': 1,
\   'green': 2,
\   'yellow': 3,
\   'blue': 4,
\   'magenta': 5,
\   'cyan': 6,
\   'white': 7,
\   'bright-black': 8,
\   'bright-red': 9,
\   'bright-green': 10,
\   'bright-yellow': 11,
\   'bright-blue': 12,
\   'bright-magenta': 13,
\   'bright-cyan': 14,
\   'bright-white': 15,
\ }

let s:NR8_COLOR_TABLE = {
\   'black': 0,
\   'red': 4,
\   'green': 2,
\   'yellow': 6,
\   'blue': 1,
\   'magenta': 5,
\   'cyan': 3,
\   'white': 7,
\   'bright-black': 0,
\   'bright-red': 4,
\   'bright-green': 2,
\   'bright-yellow': 6,
\   'bright-blue': 1,
\   'bright-magenta': 5,
\   'bright-cyan': 3,
\   'bright-white': 7,
\ }

if !exists('g:ansi_color_gui_color_table')
  let g:ansi_color_gui_color_table = {
  \   'foreground': 'White',
  \   'background': 'Black',
  \   'black': 'Black',
  \   'red': 'DarkRed',
  \   'green': 'DarkGreen',
  \   'yellow': 'DarkYellow',
  \   'blue': 'DarkBlue',
  \   'magenta': 'DarkMagenta',
  \   'cyan': 'DarkCyan',
  \   'white': 'Gray',
  \   'bright-black': 'DarkGray',
  \   'bright-red': 'Red',
  \   'bright-green': 'Green',
  \   'bright-yellow': 'Yellow',
  \   'bright-blue': 'Blue',
  \   'bright-magenta': 'Magenta',
  \   'bright-cyan': 'Cyan',
  \   'bright-white': 'White',
  \ }
endif

function! s:h(group, definition) abort
  let args = []
  let gui_colors = g:ansi_color_gui_color_table
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
call s:h('Comment', { 'attr': ['italic'], 'fg': 'white' })
call s:h('Conceal', { 'fg': 'bright-black' })
call s:h('Constant', { 'fg': 'bright-magenta' })
call s:h('Cursor', { 'bg': 'green' })
call s:h('CursorColumn', { 'bg': 'black' })
call s:h('CursorIM', { 'bg': 'bright-cyan' })
call s:h('CursorLine', { 'bg': 'black' })
call s:h('CursorLineNr', { 'bg': 'black' })
call s:h('DiffAdd', { 'bg': 'blue' })
call s:h('DiffChange', { 'bg': 'magenta' })
call s:h('DiffDelete', { 'fg': 'bright-black' })
call s:h('DiffText', { 'bg': 'magenta' })
call s:h('Directory', { 'fg': 'bright-cyan' })
call s:h('Error', { 'bg': 'red' })
call s:h('ErrorMsg', { 'bg': 'red' })
call s:h('FoldColumn', { 'fg': 'cyan' })
call s:h('Folded', { 'attr': ['italic'], 'fg': 'cyan' })
call s:h('Identifier', { 'fg': 'bright-cyan' })
call s:h('Ignore', { 'fg': 'black' })
call s:h('IncSearch', { 'attr': ['reverse'] })
call s:h('LineNr', { 'fg': 'bright-black' })
call s:h('LspError', { 'fg': 'bright-red' })
call s:h('LspErrorHighlight', { 'attr': ['undercurl'] })
call s:h('LspErrorText', { 'fg': 'bright-red' })
call s:h('LspHint', { 'fg': 'bright-blue' })
call s:h('LspHintHighlight', { 'attr': ['undercurl'] })
call s:h('LspHintText', { 'fg': 'bright-blue' })
call s:h('LspInformation', { 'fg': 'bright-blue' })
call s:h('LspInformationHighlight', { 'attr': ['undercurl'] })
call s:h('LspInformationText', { 'fg': 'bright-blue' })
call s:h('LspWarning', { 'fg': 'bright-red' })
call s:h('LspWarningHighlight', { 'attr': ['undercurl'] })
call s:h('LspWarningText', { 'fg': 'bright-red' })
call s:h('MatchParen', { 'attr': ['bold', 'reverse'] })
call s:h('ModeMsg', { 'bg': 'blue' })
call s:h('NormalFloat', {})
call s:h('MoreMsg', { 'bg': 'green' })
call s:h('NonText', { 'fg': 'bright-black' })
call s:h('Whitespace', { 'fg': 'black' })
call s:h('Pmenu', { 'bg': 'black' })
call s:h('PmenuSbar', { 'bg': 'bright-black' })
call s:h('PmenuSel', { 'attr': ['reverse'], 'fg': 'bright-yellow' })
call s:h('PmenuThumb', { 'bg': 'bright-yellow' })
call s:h('PreProc', { 'fg': 'bright-blue' })
call s:h('Question', { 'fg': 'bright-green' })
call s:h('Search', { 'attr': ['reverse'], 'fg': 'bright-yellow' })
call s:h('SignColumn', { 'fg': 'bright-cyan' })
call s:h('Special', { 'fg': 'bright-red' })
call s:h('SpecialKey', { 'fg': 'bright-black' })
call s:h('Statement', { 'fg': 'bright-yellow' })
call s:h('StatusLine', { 'attr': ['bold'], 'bg': 'black' })
call s:h('StatusLineNC', { 'bg': 'bright-black' })
call s:h('StatusLineTerm', { 'attr': ['bold', 'reverse'], 'fg': 'bright-green' })
call s:h('StatusLineTermNC', { 'attr': ['reverse'], 'fg': 'green' })
call s:h('TabLine', { 'bg': 'black' })
call s:h('TabLineFill', { 'bg': 'black' })
call s:h('TabLineSel', { 'attr': ['bold', 'underline'], 'bg': 'black' })
call s:h('Title', { 'fg': 'bright-cyan' })
call s:h('Todo', { 'attr': ['underline'], 'fg': 'bright-yellow' })
call s:h('ToolbarButton', { 'attr': ['bold'], 'bg': 'bright-black' })
call s:h('ToolbarLine', { 'bg': 'black' })
call s:h('Type', { 'fg': 'bright-green' })
call s:h('Underlined', { 'attr': ['underline'], 'fg': 'bright-blue' })
call s:h('VertSplit', { 'fg': 'bright-black' })
call s:h('Visual', { 'bg': 'blue' })
call s:h('VisualNOS', { 'attr': ['reverse'] })
call s:h('WarningMsg', { 'fg': 'bright-yellow' })
call s:h('WildMenu', { 'attr': ['bold', 'reverse'], 'fg': 'bright-yellow' })
call s:h('WinSeparator', { 'fg': 'bright-black' })
call s:h('lCursor', { 'bg': 'bright-cyan' })

if has('gui_running')
  call s:h('Normal', { 'fg': 'foreground', 'bg': 'background' })
  call s:h('SpellBad', { 'attr': ['undercurl'], 'sp': 'red' })
  call s:h('SpellCap', { 'attr': ['undercurl'], 'sp': 'blue' })
  call s:h('SpellLocal', { 'attr': ['undercurl'], 'sp': 'cyan' })
  call s:h('SpellRare', { 'attr': ['undercurl'], 'sp': 'magenta' })
else
  call s:h('SpellBad', { 'bg': 'red' })
  call s:h('SpellCap', { 'bg': 'blue' })
  call s:h('SpellLocal', { 'bg': 'cyan' })
  call s:h('SpellRare', { 'bg': 'magenta' })
endif
