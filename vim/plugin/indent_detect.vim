if exists('g:loaded_indent_detect')
  finish
endif

function! s:on_FileType() abort
  let undo_options = indent_detect#configure_options()
  if undo_options != ''
    let b:undo_ftplugin =
    \   (exists('b:undo_ftplugin') ? b:undo_ftplugin . ' | ' : '')
    \   . undo_options
  endif
endfunction

augroup plugin-indent-detect
  autocmd!
  autocmd FileType *  call s:on_FileType()
augroup END

let g:loaded_indent_detect = 1
