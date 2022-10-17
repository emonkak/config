if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=cabal\ build
CompilerSet errorformat=%-Z\ %#,
                       \%W%f:%l:%c:\ Warning:\ %m,
                       \%E%f:%l:%c:\ %m,
                       \%E%>%f:%l:%c:,
                       \%+C\ \ %#%m,
                       \%W%>%f:%l:%c:,
                       \%+C\ \ %#%tarning:\ %m
let g:current_compiler = 'cabal'
