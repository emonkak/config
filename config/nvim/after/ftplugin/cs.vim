if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal commentstring=//%s
setlocal expandtab
setlocal foldexpr=CsFold(v:lnum)
setlocal foldmethod=expr
setlocal foldnestmax=3
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal textwidth=100

inoreabbrev <buffer> ///  /// <summary><CR><CR></summary><Up>

function! CsFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*\%('
  \            . '\%(\%(abstract\|partial\|public\|private\|protected\|protected\|internal\|static\)\s\+\)*\%(class\|enum\|interface\)'
  \            . '\|namespace'
  \            . '\)\>'
  \ && current !~# '[;}]\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    if level <= &l:foldnestmax
      return '>' . level
    endif
  elseif current =~# '^\s*}\s*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    if level <= &l:foldnestmax
      return '<' . level
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
\                    . ' commentstring<'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' foldnestmax<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' textwidth<'
\                    . ' | execute "iunabbrev" "<buffer>" "///"'
