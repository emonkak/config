if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal shiftwidth=4
setlocal softtabstop=4

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal expandtab< shiftwidth< softtabstop<'
