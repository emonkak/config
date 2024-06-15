if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal cinoptions=:1s,l1,g0,t0,(0,j1
setlocal expandtab
setlocal foldmethod=syntax
setlocal foldnestmax=2
setlocal shiftwidth=4
setlocal softtabstop=4

if !exists('b:current_compiler')
  compiler gradle
endif

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' cinoptions<'
\                    . ' expandtab<'
\                    . ' foldmethod<'
\                    . ' foldnestmax<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
