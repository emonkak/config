if exists('g:loaded_skeleton')
  finish
endif

let g:skeleton_dir = get(g:, 'skeleton_dir', 'skeletons/')

command! -bang -bar -complete=customlist,s:cmd_SkeletonLoad_complete -nargs=1
\ SkeletonLoad
\ call s:cmd_SkeletonLoad(<q-args>, <bang>0)

function! s:cmd_SkeletonLoad(name, banged)
  if !a:banged && (line('$') != 1 || col('$') != 1)
    echo 'This buffer is not empty. Skeleton will not be loaded.'
    return
  endif

  let skeletons = split(globpath(&runtimepath, g:skeleton_dir . a:name), "\n")
  if len(skeletons) < 1
    echo 'Skeleton file is not found:' string(a:name)
    return
  endif

  " Load the skeleton file.
  if a:banged
    % delete _
  endif
  silent keepalt 1 read `=skeletons[0]`
  0 delete _

  doautocmd <nomodeline> User PluginSkeletonLoad
endfunction

function! s:cmd_SkeletonLoad_complete(arglead, cmdline, cursorpos)
  let pattern = g:skeleton_dir . a:arglead . '*'
  return map(split(globpath(&runtimepath, pattern), "\n"),
  \          'fnamemodify(v:val, ":t")')
endfunction

augroup plugin-skeleton
  autocmd!
  autocmd BufNewFile *  call s:on_BufNewFile()
augroup END

function! s:on_BufNewFile()
  doautocmd <nomodeline> User PluginSkeletonDetect

  if &l:filetype != '' && line('$') == 1 && col('$') == 1
    silent execute 'SkeletonLoad' &l:filetype
  endif
endfunction

let g:loaded_skeleton = 1
