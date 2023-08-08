if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal cinoptions-=(0
setlocal expandtab
setlocal foldexpr=JavascriptFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword+=$ iskeyword-=:
setlocal shiftwidth=4
setlocal softtabstop=4

function! JavascriptFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^'
  \            . '\%(export\s\+\%(default\s\+\)\?\)\?'
  \            . '\%(class\|\%(async\s\+\)\?function\)'
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
\                    . ' cinoptions<'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' iskeyword<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
