setlocal expandtab
setlocal foldexpr=GoFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

function! GoFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '\s*}$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 1 ? '=' : '<' . level
  endif

  if current =~# '^\s*\%(func\|type\s\+\h\+\s\+\%(struct\|interface\)\)\s'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 1 ? '=' : level
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|'
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
