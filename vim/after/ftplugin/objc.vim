setlocal expandtab
setlocal foldexpr=ObjcFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

function! ObjcFold(lnum) abort
  let current = getline(a:lnum)
  return (current =~# '^@\(implementation\|interface\|protocol\)') ? '>1'
  \    : (current =~# '^[-+]') ? '>2'
  \    : (current =~# '^}') ? '<2'
  \    : (current =~# '^@end') ? '<1'
  \    : '='
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
\                    . ' tabstop<'
