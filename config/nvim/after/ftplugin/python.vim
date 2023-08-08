if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal foldexpr=PythonFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4

function! PythonFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*\%(class\|def\)\>'
    let level = indent(a:lnum) / shiftwidth() + 1
    return '>' . level
  elseif current =~# '\S'
    let next_lnum = nextnonblank(a:lnum + 1)
    if a:lnum + 1 < next_lnum
      let current_indent = indent(a:lnum)
      let next_indent = indent(next_lnum)
      if current_indent > next_indent
        let level = next_indent / shiftwidth() + 1
          return '<' . level
      endif
    endif
  endif

  return '='
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
