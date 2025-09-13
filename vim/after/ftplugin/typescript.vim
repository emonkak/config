setlocal expandtab
setlocal iskeyword-=:
setlocal iskeyword+=$
setlocal shiftwidth=2
setlocal softtabstop=2

if !exists('b:current_compiler')
  compiler tsc
endif

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' expandtab<'
\                    . ' iskeyword<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
