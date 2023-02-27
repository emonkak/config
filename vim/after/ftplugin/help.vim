if expand('<sfile>:t:r') !=# &filetype
  finish
endif

if &buftype ==# 'help'
  finish
endif

setlocal conceallevel=0

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal conceallevel<'
