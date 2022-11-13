if exists('g:loaded_occurrences_highlight')
  finish
endif

function! s:update_highlight()
  let symbol = search('\%#\k', 'cn') > 0 ? expand('<cword>') : ''
  if exists('w:occurrences_highlight_symbol')
  \  && w:occurrences_highlight_symbol ==# symbol
    return
  endif

  call s:clear_highlight()

  if symbol != ''
    let pattern = '\V\<' . escape(symbol, '\\') . '\>'
    let w:occurrences_highlight_symbol_id = matchadd('OccurrencesHighlightSymbol', pattern)
  endif

  let w:occurrences_highlight_symbol = symbol
endfunction

function! s:clear_highlight()
  if exists('w:occurrences_highlight_symbol_id')
    call matchdelete(w:occurrences_highlight_symbol_id)
    unlet w:occurrences_highlight_symbol_id
  endif
  unlet! w:occurrences_highlight_symbol
endfunction

augroup plugin-occurrences-highlight
  autocmd!
  autocmd BufLeave *  call s:clear_highlight()
  autocmd CursorMoved *  call s:update_highlight()
  autocmd InsertEnter *  call s:clear_highlight()
  autocmd WinEnter *  call s:update_highlight()
  autocmd WinLeave *  call s:clear_highlight()
augroup END

highlight default link OccurrencesHighlightSymbol  CursorLine

let g:loaded_occurrences_highlight = 1
