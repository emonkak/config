if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal commentstring=//%s
setlocal expandtab
setlocal foldexpr=ObjcFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4

function! ObjcFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*@\%(implementation\|interface\|protocol\)\>'
  \ && current !~# '@end\s*$'
    return '>1'
  elseif current =~# '^\s*@end\s*$'
    return '<1'
  elseif current =~# '^[-+]' && current !~# '}\s*$'
    return '>2'
  elseif current =~# '^}\s*$'
    return '<2'
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
