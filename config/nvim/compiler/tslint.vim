if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=./node_modules/.bin/tslint\ \"%\"
CompilerSet errorformat=
  \%EERROR:\ %f[%l\\,\ %c]:\ %m
  \%WWARNING:\ %f[%l\\,\ %c]:\ %m,
  \%E%f[%l\\,\ %c]:\ %m

let g:current_compiler = 'tslint'
