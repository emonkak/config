highlight clear

set background=dark

let g:colors_name = expand('<sfile>:t:r')

if !exists('g:ansi_colors')
  let g:ansi_colors = {
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

if &t_Co < 16
  let s:TERM_COLOR_NUMBERS = {
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
else
  let s:TERM_COLOR_NUMBERS = {
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
endif

function! s:h(group, definition) abort
  let args = []

  let attr = has_key(a:definition, 'attr')
  \        ? a:definition.attr
  \        : 'NONE'
  call insert(args, 'cterm=' . attr)
  call insert(args, 'gui=' . attr)

  if has_key(a:definition, 'fg')
    call insert(args, 'ctermfg=' . get(s:TERM_COLOR_NUMBERS, a:definition.fg, 'NONE'))
    call insert(args, 'guifg=' . get(g:ansi_colors, a:definition.fg, 'NONE'))
  endi

  if has_key(a:definition, 'bg')
    call insert(args, 'ctermbg=' . get(s:TERM_COLOR_NUMBERS, a:definition.bg, 'NONE'))
    call insert(args, 'guibg=' . get(g:ansi_colors, a:definition.bg, 'NONE'))
  endif

  if has_key(a:definition, 'sp')
    call insert(args, 'guisp=' . get(g:ansi_colors, a:definition.sp, 'NONE'))
  endif

  execute 'highlight' a:group 'NONE' join(args)
endfunction

call s:h('ColorColumn', { 'bg': 'black' })
call s:h('Comment', { 'attr': 'italic', 'fg': 'white' })
call s:h('Conceal', { 'fg': 'bright-black' })
call s:h('Constant', { 'fg': 'bright-magenta' })
call s:h('Cursor', { 'bg': 'green' })
call s:h('CursorColumn', { 'bg': 'black' })
call s:h('CursorIM', { 'bg': 'bright-cyan' })
call s:h('CursorLine', { 'bg': 'black' })
call s:h('CursorLineNr', { 'bg': 'black' })
call s:h('DiagnosticDeprecated', { 'fg': 'white' })
call s:h('DiagnosticError', { 'fg': 'bright-red' })
call s:h('DiagnosticHint', { 'fg': 'white' })
call s:h('DiagnosticInfo', { 'fg': 'bright-blue' })
call s:h('DiagnosticOk', { 'fg': 'bright-green' })
call s:h('DiagnosticUnderlineError', { 'attr': 'underline', 'sp': 'bright-red' })
call s:h('DiagnosticUnderlineHint', { 'attr': 'underline', 'sp': 'bright-green' })
call s:h('DiagnosticUnderlineInfo', { 'attr': 'underline', 'sp': 'bright-blue' })
call s:h('DiagnosticUnderlineOk', { 'attr': 'underline', 'sp': 'bright-yellow' })
call s:h('DiagnosticUnderlineWarn', { 'attr': 'underline', 'sp': 'white' })
call s:h('DiagnosticWarn', { 'fg': 'bright-yellow' })
call s:h('DiffAdd', { 'bg': 'blue' })
call s:h('DiffChange', { 'bg': 'magenta' })
call s:h('DiffDelete', { 'fg': 'bright-black' })
call s:h('DiffText', { 'bg': 'magenta' })
call s:h('Directory', { 'fg': 'bright-cyan' })
call s:h('Error', { 'bg': 'red' })
call s:h('ErrorMsg', { 'bg': 'red' })
call s:h('FoldColumn', { 'fg': 'cyan' })
call s:h('Folded', { 'attr': 'italic', 'fg': 'cyan' })
call s:h('Identifier', { 'fg': 'bright-cyan' })
call s:h('Ignore', { 'fg': 'bright-black' })
call s:h('IncSearch', { 'attr': 'reverse' })
call s:h('LineNr', { 'fg': 'bright-black' })
call s:h('MatchParen', { 'attr': 'bold,reverse' })
call s:h('ModeMsg', { 'bg': 'blue' })
call s:h('MoreMsg', { 'bg': 'green' })
call s:h('NonText', { 'fg': 'bright-black' })
call s:h('NormalFloat', {})
call s:h('Pmenu', { 'bg': 'black' })
call s:h('PmenuSbar', { 'bg': 'bright-black' })
call s:h('PmenuSel', { 'attr': 'reverse', 'fg': 'bright-yellow' })
call s:h('PmenuThumb', { 'bg': 'bright-yellow' })
call s:h('PreProc', { 'fg': 'bright-blue' })
call s:h('Question', { 'fg': 'bright-green' })
call s:h('Search', { 'attr': 'reverse', 'fg': 'bright-yellow' })
call s:h('SignColumn', { 'fg': 'bright-cyan' })
call s:h('Special', { 'fg': 'bright-red' })
call s:h('SpecialKey', { 'fg': 'bright-black' })
call s:h('Statement', { 'fg': 'bright-yellow' })
call s:h('StatusLine', { 'attr': 'bold', 'bg': 'black' })
call s:h('StatusLineNC', { 'bg': 'bright-black' })
call s:h('StatusLineTerm', { 'attr': 'bold', 'bg': 'black' })
call s:h('StatusLineTermNC', { 'bg': 'bright-black' })
call s:h('TabLine', { 'bg': 'black' })
call s:h('TabLineFill', { 'bg': 'black' })
call s:h('TabLineSel', { 'attr': 'bold,underline', 'bg': 'black' })
call s:h('Title', { 'fg': 'bright-cyan' })
call s:h('Todo', { 'attr': 'underline', 'fg': 'bright-yellow' })
call s:h('ToolbarButton', { 'attr': 'bold', 'bg': 'bright-black' })
call s:h('ToolbarLine', { 'bg': 'black' })
call s:h('Type', { 'fg': 'bright-green' })
call s:h('Underlined', { 'attr': 'underline', 'fg': 'bright-blue' })
call s:h('VertSplit', { 'fg': 'bright-black' })
call s:h('Visual', { 'bg': 'blue' })
call s:h('VisualNOS', { 'attr': 'reverse' })
call s:h('WarningMsg', { 'fg': 'bright-yellow' })
call s:h('Whitespace', { 'fg': 'black' })
call s:h('WildMenu', { 'attr': 'bold,reverse', 'fg': 'bright-yellow' })
call s:h('WinSeparator', { 'fg': 'bright-black' })
call s:h('lCursor', { 'bg': 'cyan' })

if has('gui_running')
  call s:h('Normal', { 'fg': 'foreground', 'bg': 'background' })
  call s:h('Terminal', { 'fg': 'foreground', 'bg': 'background' })
  call s:h('SpellBad', { 'attr': 'undercurl', 'sp': 'red' })
  call s:h('SpellCap', { 'attr': 'undercurl', 'sp': 'blue' })
  call s:h('SpellLocal', { 'attr': 'undercurl', 'sp': 'cyan' })
  call s:h('SpellRare', { 'attr': 'undercurl', 'sp': 'magenta' })
else
  call s:h('SpellBad', { 'bg': 'red' })
  call s:h('SpellCap', { 'bg': 'blue' })
  call s:h('SpellLocal', { 'bg': 'cyan' })
  call s:h('SpellRare', { 'bg': 'magenta' })
endif

if has('nvim')
  let g:terminal_color_1 = g:ansi_colors['black']
  let g:terminal_color_2 = g:ansi_colors['red']
  let g:terminal_color_3 = g:ansi_colors['green']
  let g:terminal_color_4 = g:ansi_colors['yellow']
  let g:terminal_color_5 = g:ansi_colors['blue']
  let g:terminal_color_6 = g:ansi_colors['magenta']
  let g:terminal_color_7 = g:ansi_colors['cyan']
  let g:terminal_color_8 = g:ansi_colors['white']
  let g:terminal_color_9 = g:ansi_colors['bright-black']
  let g:terminal_color_10 = g:ansi_colors['bright-red']
  let g:terminal_color_11 = g:ansi_colors['bright-green']
  let g:terminal_color_12 = g:ansi_colors['bright-yellow']
  let g:terminal_color_13 = g:ansi_colors['bright-blue']
  let g:terminal_color_14 = g:ansi_colors['bright-magenta']
  let g:terminal_color_15 = g:ansi_colors['bright-cyan']
  let g:terminal_color_16 = g:ansi_colors['bright-white']
else
  let g:terminal_ansi_colors = [
  \   g:ansi_colors['black'],
  \   g:ansi_colors['red'],
  \   g:ansi_colors['green'],
  \   g:ansi_colors['yellow'],
  \   g:ansi_colors['blue'],
  \   g:ansi_colors['magenta'],
  \   g:ansi_colors['cyan'],
  \   g:ansi_colors['white'],
  \   g:ansi_colors['bright-black'],
  \   g:ansi_colors['bright-red'],
  \   g:ansi_colors['bright-green'],
  \   g:ansi_colors['bright-yellow'],
  \   g:ansi_colors['bright-blue'],
  \   g:ansi_colors['bright-magenta'],
  \   g:ansi_colors['bright-cyan'],
  \   g:ansi_colors['bright-white'],
  \ ]
endif
