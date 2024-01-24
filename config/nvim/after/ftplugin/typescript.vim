setlocal expandtab
setlocal foldexpr=TypescriptFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword-=:
setlocal shiftwidth=4
setlocal softtabstop=4

if !exists('b:current_compiler')
  compiler tsc
endif

function! TypescriptFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^'
  \            . '\%(export\s\+\%(default\s\+\)\?\|declare\s\+\)\?'
  \            . '\%(\%(abstract\s\+\)\?class\|\%(async\s\+\)\?function\|interface\|module\|namespace\)'
  \            . '\>'
  \ && current !~# '[;}]\s*$'
    return '>1'
  elseif current =~# '^}\s*$'
    return '<1'
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' iskeyword<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
