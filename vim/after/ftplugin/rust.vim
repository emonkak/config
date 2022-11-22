setlocal foldmethod=expr
setlocal foldexpr=RustFold(v:lnum)

if !exists('b:current_compiler')
  compiler cargo
endif

function! RustFold(lnum)
  let current = getline(a:lnum)

  if current =~# '^\s*}$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 3 ? '=' : '<' . level
  endif

  if current =~# '^\s*\%('
  \            . 'macro_rules!\|'
  \            . '\%(unsafe\s\+\)\?impl\|'
  \            . '\%(pub\%(\s*(\s*\%(crate\|super\)\s*)\)\?\s\+\)\?\%(const\s\+\)\?\%(unsafe\s\+\)\?\%(async\s\+\)\?\%(extern\s\+"[^"]\+"\s\+\)\?\%(enum\|fn\|mod\|struct\|trait\|union\)\>'
  \            . '\)'
  \  && current !~# '[;}]\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 3 ? '=' : '>' . level
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal foldexpr< foldmethod<'
