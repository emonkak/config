setlocal expandtab
setlocal foldexpr=TypescriptFold(v:lnum)
setlocal foldmethod=expr
setlocal iskeyword-=:
setlocal makeprg=./node_modules/.bin/tsc
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

function! TypescriptFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '^\s*'
             \ . '\%(\%(export\s\+\%(default\s\+\)\?\)\?\%(\%(abstract\s\+\)\?class\|\%(async\s\+\)\?function\|interface\|module\)'
             \ . '\|\%(\%(static\|private\|protected\|public\|async\|get\|set\)\s\+\)*\*\?\%(\h\+\|\[.\{-}\]\)\s*\%(<.\+>\)\?\s*('
             \ . '\|\%(\%(static\|private\|protected\|public\)\s\+\)\?\h\+\s*=\s*\(async\s*\)\?('
             \ . '\|\%(declare\s\+\)\%(module\|namespace\|interface\|class\)'
             \ . '\)\>'
             \ . '[^;]*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '>' . level
  endif

  if current =~# '^\s*}$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 2 ? '=' : '<' . level
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
\                    . ' iskeyword<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
