if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=tsc\ --build
CompilerSet errorformat=%f\ %#(%l\\,%c):\ %trror\ TS%n:\ %m,
		       \%trror\ TS%n:\ %m,
		       \%-G%.%#

let g:current_compiler = 'tsc'
