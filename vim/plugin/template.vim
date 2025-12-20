if exists('g:loaded_template')
  finish
endif

let g:template_dir = get(g:, 'template_dir', 'templates/')

command! -bang -bar -complete=customlist,s:cmd_TemplateLoad_complete -nargs=1
\ TemplateLoad
\ call s:cmd_TemplateLoad(<q-args>, <bang>0)

function! s:cmd_TemplateLoad(name, banged)
  if !a:banged && (line('$') != 1 || col('$') != 1)
    echo 'This buffer is not empty. Template will not be loaded.'
    return
  endif

  let templates = split(globpath(&runtimepath, g:template_dir . a:name), "\n")
  if len(templates) < 1
    echo 'Template file is not found:' string(a:name)
    return
  endif

  " Load the template file.
  if a:banged
    % delete _
  endif
  silent keepalt 1 read `=templates[0]`
  0 delete _

  doautocmd <nomodeline> User PluginTemplateLoad
endfunction

function! s:cmd_TemplateLoad_complete(arglead, cmdline, cursorpos)
  let pattern = g:template_dir . a:arglead . '*'
  return map(split(globpath(&runtimepath, pattern), "\n"),
  \          'fnamemodify(v:val, ":t")')
endfunction

augroup plugin-template
  autocmd!
  autocmd BufNewFile *  call s:on_BufNewFile()
augroup END

function! s:on_BufNewFile()
  doautocmd <nomodeline> User PluginTemplateDetect

  if &l:filetype != '' && line('$') == 1 && col('$') == 1
    silent execute 'TemplateLoad' &l:filetype
  endif
endfunction

let g:loaded_template = 1
