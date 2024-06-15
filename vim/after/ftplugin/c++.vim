if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal commentstring=//%s
setlocal expandtab
setlocal shiftwidth=4
setlocal softtabstop=4

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' commentstring<'
\                    . ' expandtab<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
