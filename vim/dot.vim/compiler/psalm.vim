if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=composer\ -v\ exec\ --\ psalm\ --output-format=emacs
CompilerSet errorformat=%f:%l:%m

let g:current_compiler = 'psalm'
