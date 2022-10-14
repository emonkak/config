if exists('g:current_compiler')
  finish
endif

CompilerSet makeprg=composer\ -v\ exec\ --\ phpstan\ analyze\ --memory-limit\ 1G\ --level\ max\ --error-format\ raw
CompilerSet errorformat=%f\ (%o):%l:%m,%f:%l:%m

let g:current_compiler = 'phpstan'
