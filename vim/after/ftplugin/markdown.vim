setlocal foldexpr=MarkdownFold(v:lnum)
setlocal foldmethod=expr

function! MarkdownFold(lnum) abort
  let current = getline(a:lnum);

  if current =~# '^#\{1,5}\s*\S'
    return '>' . current
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal foldexpr< foldmethod<'
