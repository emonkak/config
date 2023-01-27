if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal foldexpr=MarkdownFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=2
setlocal softtabstop=2

function! MarkdownFold(lnum) abort
  let current = getline(a:lnum)
  if current =~ '^#\{1,6}' && !s:in_syntax(a:lnum, 'markdownCode')
    return '>1'
  elseif current =~ '^>\s'
    let prev = getline(a:lnum - 1)
    if prev == ''
      return 'a1'
    endif
    let next = getline(a:lnum + 1)
    if next == ''
      return 's1'
    endif
  elseif current =~ '^\(`\{3}\|\~\{3}\)'
    let prev = getline(a:lnum - 1)
    if prev == '' && !s:in_syntax(a:lnum - 1, 'markdownCode')
      return 'a1'
    endif
    let next = getline(a:lnum + 1)
    if next == '' && !s:in_syntax(a:lnum + 1, 'markdownCode')
      return 's1'
    endif
  endif
  return '='
endfunction

function! s:in_syntax(lnum, syntax_name) abort
  for syntax in synstack(a:lnum, 1)
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

let b:undo_ftplugin .= 'setlocal expandtab< foldexpr< foldmethod< shiftwidth< softtabstop<'
