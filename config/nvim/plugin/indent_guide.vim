if exists('g:loaded_indent_guide')
  finish
endif

function! s:update_listchars() abort
  let settings = filter(split(&listchars, '\\\@<!,'),
  \                     'v:val !~# "^leadmultispace:"')
  if &l:expandtab
    let separator = strdisplaywidth("\u2502") == 1 ? "\u2502" : '|'
    call add(settings, 'leadmultispace:'
    \                  . separator
    \                  . repeat(' ', &l:shiftwidth - 1))
  else
    call add(settings, 'leadmultispace: ')
  endif
  try
    let &l:listchars = join(settings, ',')
    return 1
  catch /^Vim\%((\a\+)\)\=:E474:/
    return 0
  endtry
endfunction

if s:update_listchars()
  augroup plugin-indent-guide
    autocmd!
    autocmd BufEnter *  call s:update_listchars()
    autocmd OptionSet expandtab,listchars,shiftwidth
    \ call s:update_listchars()
  augroup END
endif

let g:loaded_indent_guide = 1
