setlocal cinoptions-=(0
setlocal expandtab
setlocal foldexpr=JavascriptFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword+=$
setlocal iskeyword-=58
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

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
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' iskeyword<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
