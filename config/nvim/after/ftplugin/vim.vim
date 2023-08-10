if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal foldexpr=VimFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword-=#
setlocal shiftwidth=2
setlocal softtabstop=2

function! VimFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*fu\%[nction]!\?\>'
    if s:check_syntax(a:lnum, match(current, '\S') + 1, 'vimCommand')
      let level = indent(a:lnum) / shiftwidth() + 1
      return 'a1'
    endif
  elseif current =~# '^\s*endf\%[unction]\s*$'
    if s:check_syntax(a:lnum, match(current, '\S') + 1, 'vimCommand')
      let level = indent(a:lnum) / shiftwidth() + 1
      return 's1'
    endif
  endif

  return '='
endfunction

function! s:check_syntax(lnum, col, syntax_name) abort
  for syntax in synstack(a:lnum, a:col)
    let name = synIDattr(syntax, 'name')
    if name ==# a:syntax_name
      return 1
    endif
  endfor
  return 0
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
