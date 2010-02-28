hi clear
set background=dark

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "basic"

hi Normal       cterm=NONE         ctermfg=NONE ctermbg=NONE
hi SpecialKey   cterm=NONE         ctermfg=8    ctermbg=NONE
hi NonText      cterm=NONE         ctermfg=4    ctermbg=NONE
hi LineNr       cterm=NONE         ctermfg=8    ctermbg=NONE
hi Directory    cterm=NONE         ctermfg=14   ctermbg=NONE
hi Question     cterm=NONE         ctermfg=10   ctermbg=NONE
hi Title        cterm=NONE         ctermfg=14   ctermbg=NONE
hi Visual       cterm=reverse      ctermfg=NONE ctermbg=NONE
hi VertSplit    cterm=NONE         ctermfg=8    ctermbg=NONE
hi WildMenu     cterm=NONE         ctermfg=15   ctermbg=4

" fold
hi Folded       cterm=NONE         ctermfg=6    ctermbg=NONE
hi FoldColumn   cterm=NONE         ctermfg=14   ctermbg=NONE

" search
hi Search       cterm=NONE         ctermfg=NONE ctermbg=4
hi MatchParen   cterm=NONE         ctermfg=NONE ctermbg=6
hi IncSearch    cterm=reverse      ctermfg=NONE ctermbg=NONE

" status line
hi StatusLine   cterm=reverse      ctermfg=NONE ctermbg=0
hi StatusLineNC cterm=reverse      ctermfg=7    ctermbg=0

" tab line
hi TabLine      cterm=reverse      ctermfg=NONE ctermbg=0
hi TabLineSel   cterm=NONE         ctermfg=15   ctermbg=NONE
hi TabLineFill  cterm=reverse      ctermfg=NONE ctermbg=0

" message
hi MoreMsg      cterm=NONE         ctermfg=NONE ctermbg=2
hi ModeMsg      cterm=NONE         ctermfg=NONE ctermbg=4
hi ErrorMsg     cterm=NONE         ctermfg=NONE ctermbg=1
hi WarningMsg   cterm=NONE         ctermfg=11   ctermbg=NONE

" diff
hi DiffAdd      cterm=NONE         ctermfg=NONE ctermbg=4
hi DiffChange   cterm=NONE         ctermfg=NONE ctermbg=5
hi DiffDelete   cterm=NONE         ctermfg=8    ctermbg=NONE
hi DiffText     cterm=NONE         ctermfg=NONE ctermbg=5

" spell check
hi SpellBad     cterm=NONE         ctermfg=NONE ctermbg=1
hi SpellCap     cterm=NONE         ctermfg=NONE ctermbg=4
hi SpellRare    cterm=NONE         ctermfg=NONE ctermbg=5
hi SpellLocal   cterm=underline    ctermfg=NONE ctermbg=6

" complete menu
hi Pmenu        cterm=underline    ctermfg=NONE ctermbg=NONE
hi PmenuSel     cterm=NONE         ctermfg=0    ctermbg=11
hi PmenuSbar    cterm=NONE         ctermfg=NONE ctermbg=NONE
hi PmenuThumb   cterm=NONE         ctermfg=NONE ctermbg=11

" syntax
hi Comment      cterm=NONE         ctermfg=14   ctermbg=NONE
hi Constant     cterm=NONE         ctermfg=13   ctermbg=NONE
hi Special      cterm=NONE         ctermfg=9    ctermbg=NONE
hi Identifier   cterm=bold         ctermfg=14   ctermbg=NONE
hi Statement    cterm=NONE         ctermfg=11   ctermbg=NONE
hi PreProc      cterm=NONE         ctermfg=12   ctermbg=NONE
hi Type         cterm=NONE         ctermfg=10   ctermbg=NONE
hi Underlined   cterm=underline    ctermfg=12   ctermbg=NONE
hi Ignore       cterm=NONE         ctermfg=0    ctermbg=NONE
hi Error        cterm=NONE         ctermfg=NONE ctermbg=1
hi Todo         cterm=NONE         ctermfg=0    ctermbg=3

" link
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
