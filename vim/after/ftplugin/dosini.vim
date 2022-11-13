setlocal foldmethod=expr

let &l:foldexpr = '(getline(v:lnum)[0] == "[") ? ">1" : "="'

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|'
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
