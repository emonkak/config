setlocal foldmethod=expr

let &l:foldexpr = '(getline(v:lnum)[0] == "[") ? ">1" : "="'

function! s:jump_section_n(pattern) abort
  let pattern = a:pattern[1:]
  let forward_p = a:pattern[0] == '/'
  let flags = forward_p ? 'W' : 'Wb'

  mark '
  let i = 0
  while i < v:count1
    if search(pattern, flags) == 0
      if forward_p
        normal! G
      else
        normal! gg
      endif
      break
    endif
    let i = i + 1
  endwhile
endfunction

function! s:jump_section_v(motion) abort
  execute 'normal!' "gv\<Esc>"
  execute 'normal' v:count1 . a:motion
  let line = line('.')
  let col = col('.')

  normal! gv
  call cursor(line, col)
endfunction

function! s:jump_section_o(motion) abort
  execute 'normal' v:count1 . a:motion
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= '|'
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' foldexpr<'
\                    . ' foldmethod<'
