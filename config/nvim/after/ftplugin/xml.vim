if expand('<sfile>:t:r') !=# &filetype
  finish
endif

setlocal expandtab
setlocal shiftwidth=2
setlocal softtabstop=2

" To deal with tag-name-including-hyphens.
setlocal iskeyword+=-

" Insert the end tag after the cursor.
" Before: <code{|}
" After:  <code>{|}</code>
inoremap <buffer> <lt><lt>  ><lt>/<C-x><C-o><C-r>=
                           \<SID>keys_to_stop_insert_mode_completion()
                           \<CR><C-o>F<lt>

" Wrap the cursor with the tag.
" Before: <code{|}
" After:  <code>
"           {|}
"         </code>
inoremap <buffer> >>  ><CR>X<CR><LT>/<C-x><C-o><C-d><C-r>=
                     \<SID>keys_to_stop_insert_mode_completion()
                     \<CR><C-o><Up><BS>

function! s:keys_to_stop_insert_mode_completion() abort
  if pumvisible()
    return "\<C-e>"
  else
    return "\<Space>\<BS>"
  endif
endfunction

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif

let b:undo_ftplugin .= 'setlocal'
\                    . ' expandtab<'
\                    . ' iskeyword<'
\                    . ' shiftwidth<'
\                    . ' softtabstop<'
\                    . ' | silent! execute "iunmap <buffer> <lt><lt>"'
\                    . ' | silent! execute "iunmap <buffer> >>"'
