if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal cinoptions-=(0
setlocal expandtab
setlocal foldexpr=JavascriptFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword+=$
setlocal iskeyword-=58
setlocal shiftwidth=4
setlocal softtabstop=4

function! JavascriptFold(lnum) abort
  let current = getline(a:lnum)
  let level = indent(a:lnum) / shiftwidth() + 1

  if current =~# '\s*}$'
    return level > 2 ? '=' : '<' . level
  endif

  if current =~# '^\s*\%(export\s\+\%(default\s\+\)\?\)\?\%(class\|\%(async\s\+\)\?function\>\)'
  \  || (level > 1 && current =~# '^\s*\%(static\s\+\)\?\%(async\s\+\)\?\h\w*(\s*\(\h\|)\)')
    if current !~# ';\s*$'
      return level > 2 ? '=' : '>' . level
    endif
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
