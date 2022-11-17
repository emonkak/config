setlocal expandtab
setlocal foldexpr=VimFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=2
setlocal softtabstop=2

function! VimFold(lnum) abort
  let current = getline(a:lnum)
  if current =~# '^\s*endfunction$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return '<' . level
  endif

  if current =~# '^\s*function!\?\>'
    let level = indent(a:lnum) / shiftwidth() + 1
    return '>' . level
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
