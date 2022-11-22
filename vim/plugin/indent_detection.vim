if exists('g:loaded_indent_detection')
  finish
endif

function! s:on_FileType() abort
  let undo_options = indent_detection#configure_options()
  if undo_options != ''
    let b:undo_ftplugin =
    \   (exists('b:undo_ftplugin') ? b:undo_ftplugin . ' | ' : '')
    \   . undo_options
  endif
endfunction

augroup plugin-indent-detection
  autocmd!
  autocmd FileType *  call s:on_FileType()
augroup END

let g:loaded_indent_detection = 1
