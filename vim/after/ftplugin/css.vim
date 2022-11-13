setlocal expandtab
setlocal foldexpr=CssFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal tabstop=2

function! CssFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*@\(layer\|media\)\>'
  \  && current !~# '[;}]\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '>' . level
  endif

  if current =~# '^\s*}\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '<' . level
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|'
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
