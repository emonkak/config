setlocal expandtab
setlocal foldexpr=LuaFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=2
setlocal softtabstop=2

function! LuaFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '\s*end$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '<' . level
  endif

  if current =~# '^\s*\(local\s*\)\?function\>'
  \  && current !~# '\send\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '>' . level
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
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
