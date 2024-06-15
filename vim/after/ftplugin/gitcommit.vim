if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal nofoldenable

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal nofoldenable<'
