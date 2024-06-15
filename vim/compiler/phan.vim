if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=composer\ -v\ exec\ --\ phan\ -j\ 4
CompilerSet errorformat=%f:%l\ %m

let g:current_compiler = 'phan'
