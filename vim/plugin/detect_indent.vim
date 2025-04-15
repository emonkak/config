if exists('g:loaded_detect_indent')
  finish
endif

function! s:on_FileType() abort
  let undo_options = detect_indent#configure_options()
  if undo_options != ''
    let b:undo_ftplugin =
    \   (exists('b:undo_ftplugin') ? b:undo_ftplugin . ' | ' : '')
    \   . undo_options
  endif
endfunction

augroup plugin-detect-indent
  autocmd!
  autocmd FileType *  call s:on_FileType()
augroup END

let g:loaded_detect_indent = 1
