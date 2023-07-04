if expand('<sfile>:t:r') !=# &filetype
  finish
endif

if &buftype ==# 'help'
  finish
endif

setlocal conceallevel=0
setlocal foldexpr=HelpFold(v:lnum)
setlocal foldmethod=expr

function! HelpFold(lnum) abort
  let current = getline(a:lnum)
  if current =~# "^[A-Z.][-A-Z0-9 .,()_']*"
    let previous = getline(a:lnum - 1)
    if previous =~ '^=\+$'
      return '>1'
    elseif previous =~ '^-\+$'
      return '>2'
    endif
  elseif current == '' || current == '<'
    let next = getline(a:lnum + 1)
    if next =~ '^=\+$'
      return '<1'
    elseif next =~ '^-\+$'
      return '<2'
    endif
  endif
  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal conceallevel<'
