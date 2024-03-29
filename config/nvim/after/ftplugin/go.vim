if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal foldexpr=GoFold(v:lnum)
setlocal foldmethod=expr
setlocal foldnestmax=3
setlocal shiftwidth=4
setlocal softtabstop=4

function! GoFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*\%(func\|type\s\+\h\w*\s\+\%(struct\|interface\)\)\>'
  \ && current !~# '\s*;\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    if level <= &l:foldnestmax
      return '>' . level
    endif
  elseif current =~# '\s*}\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    if level <= &l:foldnestmax
      return '<' . level
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
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' foldnestmax<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
