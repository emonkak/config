if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=ant\ $*
CompilerSet errorformat=
    \%E\ %#[%.%#]\ %f(%l):\ col:\ %c\ Error:\ %m,
    \%W\ %#[%.%#]\ %f(%l):\ col:\ %c\ Warning:\ %m,
    \%E\ %#[%.%#]\ %f:\ Error:\ %m,
    \%W\ %#[%.%#]\ %f:\ Warning:\ %m,
    \%-G%.%#

let g:current_compiler = 'mxmlc'
