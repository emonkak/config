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
    if s:check_syntax(a:lnum, match(current, '\S') + 1, ['vimFunction', 'vimCommand'])
      return 'a1'
    endif
  elseif current =~# '^\s*endf\%[unction]\s*$'
    if s:check_syntax(a:lnum, match(current, '\S') + 1, ['vimEndfunction', 'vimCommand'])
      return 's1'
    endif
  endif

  return '='
endfunction

function! s:check_syntax(lnum, col, expected_syntax_names) abort
  for syntax in synstack(a:lnum, a:col)
    let name = synIDattr(syntax, 'name')
    if index(a:expected_syntax_names, name) >= 0
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
