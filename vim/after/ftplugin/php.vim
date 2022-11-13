setlocal commentstring=//%s
setlocal expandtab
setlocal foldexpr=PHPFold(v:lnum)
setlocal foldmethod=expr
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

if !exists('b:current_compiler')
  compiler psalm
endif

inoreabbrev <buffer> /** /**<Space>*/<Left><Left><Left>

command! -range -nargs=0 -buffer PHPInsertAccessors
\ <line1>,<line2>call s:define_accessors()
command! -range -nargs=0 -buffer PHPInsertGetters
\ <line1>,<line2>call s:define_getters()
command! -range -nargs=0 -buffer PHPInsertSetters
\ <line1>,<line2>call s:define_setters()

function! PHPFold(lnum) abort
  let current = getline(a:lnum)

  if current =~# '\s*}$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 3 ? '=' : '<' . level
  endif

  if current =~# '^\s*\('
             \ . '\(\(final\|private\|protected\|public\|static\)\s\)*function'
             \ . '\|\(\(abstract\|final\)\s\)*class'
             \ . '\|interface'
             \ . '\|namespace'
             \ . '\|trait'
             \ . '\)\>[^;]*$'
    let level = indent(a:lnum) / shiftwidth() + 1
    return level > 3 ? '=' : level
  endif

  return '='
endfunction

function! s:define_accessors() abort range
  let properties = s:collect_properties(a:firstline, a:lastline)

  call cursor(a:lastline, 1)

  call s:render_getters(properties)
  call s:render_setters(properties)

  call cursor(a:firstline, 1)
endfunction

function! s:define_getters() abort range
  let properties = s:collect_properties(a:firstline, a:lastline)

  call cursor(a:lastline, 1)

  call s:render_getters(properties)

  call cursor(a:firstline, 1)
endfunction

function! s:define_setters() abort range
  let properties = s:collect_properties(a:firstline, a:lastline)

  call cursor(a:lastline, 1)

  call s:render_setters(properties)

  call cursor(a:firstline, 1)
endfunction

function! s:render_getters(properties) abort
  for property in a:properties
    let [_, name, type] = property
    silent put =''
    silent put =s:create_getter('public', name, type)
  endfor
endfunction

function! s:render_setters(properties) abort
  for property in a:properties
    let [visibility, name, type] = property
    silent put =''
    silent put =s:create_setter(visibility, name, type)
  endfor
endfunction

function! s:create_getter(visibility, name, type) abort
  let type_definition = a:type ==# 'mixed' ? '' : ': ' . a:type
  return join([
  \    '    ' . a:visibility . ' function get' . s:camelize(a:name) . '()' . type_definition,
  \    '    {',
  \    '        return $this->' . a:name . ';',
  \    '    }'
  \ ], "\n")
endfunction

function! s:create_setter(visibility, name, type) abort
  let type_definition = a:type ==# 'mixed' ? '' : a:type . ' '
  return join([
  \   '    ' . a:visibility . ' function set' . s:camelize(a:name) . '(' . type_definition . '$' . s:lowerCamelize(a:name) . '): void',
  \   '    {',
  \   '        $this->' . a:name . ' = ' . '$' . s:lowerCamelize(a:name) . ';',
  \   '    }'
  \ ], "\n")
endfunction

function! s:collect_properties(first, last) abort
  let properties = []
  let types = []

  for line_num in range(a:first, a:last)
    let line = getline(line_num)
    let matches = matchlist(line, '@var\s\+\(\S\+\)')
    if len(matches) > 0
      call add(types, matches[1])
    endif

    let matches = matchlist(line, '\(private\|public\|protected\)\s\+\%(\(?\?\h\+\)\s\+\)\?\$\(\h\+\)')
    if len(matches) > 0
      let alternate_type = len(types) > 0 ? remove(types, 0) : 'mixed'
      let type = matches[2] != '' ? matches[2] : alternate_type
      call add(properties, [matches[1], matches[3], type])
    endif
  endfor

  return properties
endfunction

function! s:camelize(string) abort
    return substitute(a:string,
    \                 '\%([_-]\|^\)\([a-z]\)\([a-z]\+\)',
    \                 '\=toupper(submatch(1)) . tolower(submatch(2))',
    \                 'gi')
endfunction

function! s:lowerCamelize(string) abort
    return substitute(s:camelize(a:string),
    \                 '^[a-z]',
    \                 '\=tolower(submatch(0))',
    \                 '')
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|'
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' commentstring<'
\                    . ' expandtab<'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' tabstop<'
\                    . '|iunabbrev <buffer> /**'
\                    . '|delcommand PHPInsertAccessors'
\                    . '|delcommand PHPInsertGetters'
\                    . '|delcommand PHPInsertSetters'
