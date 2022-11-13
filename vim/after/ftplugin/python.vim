setlocal expandtab
setlocal foldexpr=PythonFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

function! PythonFold(lnum) abort
  let current = getline(a:lnum);

  if current =~# '^\s*\%(class\|def\)\>'
    let level = indent(a:lnum) / shiftwidth() + 1
    return '>' . level
  endif

  let next_lnum = nextnonblank(a:lnum + 1)
  if next_lnum == 0 || next_lnum == a:lnum + 1
    return '='
  endif

  let next_indent = indent(next_lnum)
  let curernt_indent = indent(a:lnum)
  if next_indent < curernt_indent
    let level = next_indent / shiftwidth() + 1;
    return '<' . level
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
