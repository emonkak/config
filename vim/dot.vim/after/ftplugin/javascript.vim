setlocal cinoptions-=(0
setlocal foldexpr=JavascriptFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword+=$
setlocal iskeyword-=58

function! JavascriptFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '\s*}$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '<' . level
  endif

  if current =~# '^\s*\%(export\s\+\%(default\s\+\)\?\)\?\%(class\|\%(async\s\+\)\?function\)\s\+\h'
  \  && current !~# ';\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '>' . level
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|'
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' cinoptions<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' iskeyword<'
