if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=javac\ -Xlint:unchecked\ -Xlint:deprecation\ \"%\"
CompilerSet errorformat=%E%f:%l:\ %m,
                       \%C%\S%\+:\ %.%#\ %m,
                       \%Z%p^,%C%.%#

let g:current_compiler = 'javac'
