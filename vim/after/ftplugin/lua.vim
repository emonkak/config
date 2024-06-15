setlocal expandtab
setlocal foldexpr=LuaFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=2
setlocal softtabstop=2

function! LuaFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\%(local\s\+\)\?function\>' && current !~# '\<end\s*$'
    return '>1'
  elseif current =~# '^end\s*$'
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
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
