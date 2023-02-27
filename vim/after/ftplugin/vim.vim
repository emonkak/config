if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal foldexpr=VimFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=2
setlocal softtabstop=2

function! VimFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*function!\?\>'
    let level = indent(a:lnum) / shiftwidth() + 1
    return 'a1'
  elseif current =~# '^\s*endfunction\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return 's1'
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
