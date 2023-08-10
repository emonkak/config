if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal foldexpr=MarkdownFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4

function! MarkdownFold(lnum) abort
  let current = getline(a:lnum)
  if current =~ '^#\{1,6}' && s:check_syntax(a:lnum, 1, 'markdownHeading')
    return '>1'
  elseif current =~ '^>' && s:check_syntax(a:lnum, 1, 'markdownBlockquote')
    let prev = getline(a:lnum - 1)
    let next = getline(a:lnum + 1)
    if prev == '' && next == ''
      return '='
    endif
    if prev == ''
      return 'a1'
    endif
    if next == ''
      return 's1'
    endif
  elseif current =~ '^\(`\{3,}\|\~\{3,}\)'
    let prev = getline(a:lnum - 1)
    if prev == '' && !s:check_syntax(a:lnum - 1, 1, 'markdownFencedCode')
      return 'a1'
    endif
    let next = getline(a:lnum + 1)
    if next == '' && !s:check_syntax(a:lnum + 1, 1, 'markdownFencedCode')
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

let b:undo_ftplugin .= 'setlocal expandtab< foldexpr< foldmethod< shiftwidth< softtabstop<'
