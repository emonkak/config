" Vim syntax: vim
" Version: 0.0.0

syntax clear vimOperParen
syntax region vimOperParen
\ matchgroup=vimParenSep start="(" end=")"
\ contains=@vimOperGroup
syntax region vimOperParen
\ oneline matchgroup=vimSep start="{" end="}"
\ contains=@vimOperGroup
\ nextgroup=vimVar,vimFuncVar

syntax clear vimUserAttrbCmpltFunc
syntax match vimUserAttrbCmpltFunc
\ contained ",\%([sS]:\|<[sS][iI][dD]>\)\=\%(\w*\%(#\w*\)\+\|\w*\)"hs=s+1
\ nextgroup=vimUserCmdError
