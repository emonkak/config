" My .vimrc
" Basic  "{{{1
" Absolute  "{{{2

function! s:SID_PREFIX()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfunction




" Encoding  "{{{2

set encoding=utf-8

if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'

  " Does iconv support JIS X 0213 ?
  if iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif

  " Make fileencodings
  let &fileencodings = s:enc_jis
  let &fileencodings .= ',' . s:enc_euc
  let &fileencodings .= ',' . 'utf-8'
  let &fileencodings .= ',' . 'cp932'
  let &fileencodings .= ',' . 'ucs-bom'

  unlet s:enc_euc
  unlet s:enc_jis
endif




" Options  "{{{2

if has('vim_starting')
  if has('win32') || has('win64')
    language C
    set fileformat=unix
    set runtimepath=~/.vim,$VIMRUNTIME,~/.vim/after
    set termencoding=cp932
  endif
endif

if has('gui_running')
  set guicursor=a:blinkon0
  if has('gui_gtk2')
    set guifont=Consolas\ 10.5
    set linespace=3
  elseif has('gui_win32')
    set guifont=Consolas:h10.5
  endif
  set guioptions=AcgM
endif

if (1 < &t_Co || has('gui')) && has('syntax')
  syntax enable
  if !exists('g:colors_name')
    colorscheme basic256
  endif
endif

filetype plugin indent on


if has('kaoriya') && has('gui_running')
  set ambiwidth=auto
else
  set ambiwidth=double
endif
set backspace=indent,eol,start
set nobackup
if has('clientserver')
  set clipboard=autoselectml,exclude:cons\|linux
endif
set completeopt=menuone,longest
set confirm
set cursorline
set diffopt=filler,vertical
set directory=~/tmp,/tmp
set fileformats=unix,dos,mac
set hidden
set history=1000
if has('multi_byte_ime') || has('xim')
  set iminsert=0
  set imsearch=0
endif
set keywordprg=:help
set nrformats=hex
set nowritebackup
if exists('+shellslash')
  set shellslash
endif
set spelllang=en_us
set updatetime=1000
set virtualedit=block

set cmdheight=1
if has('conceal')
  set concealcursor=nc
  set conceallevel=2
endif
set display=lastline
set noequalalways
set foldmethod=marker
set nohlsearch
set laststatus=2
set linebreak
set list
let &listchars = "tab:\u00bb ,extends:<,trail:-"
set pumheight=20
set ruler
set showcmd
set showtabline=2
set splitbelow
set splitright
set ttimeoutlen=50
set wildmenu
set nowrapscan

set autoindent
set cinoptions=:0,l1,g0,t0,(0,W1s
set formatoptions=roqnlmM1
set formatlistpat&
let &formatlistpat .= '\|^\s*[*+-]\s*'
set ignorecase
set incsearch
set shiftround
set smartcase
set smartindent

set title
set titlestring=Vim:\ %f\ %h%r%m
if exists('$TMUX')
  let &t_fs = "\<C-g>"
  let &t_ts = "\<Esc>]2;"
endif

let &statusline = ''
let &statusline .= '%<%f %h%m%r%w'
let &statusline .= '   %{eskk#statusline("[%s]")}'
let &statusline .= '%='
let &statusline .= '[%{&l:fileencoding == "" ? &encoding : &l:fileencoding}'
let &statusline .= '%{&l:fileformat[0] == &fileformats[0] ? "" : "," . &l:fileformat}]'
let &statusline .= '   %-14.(%l,%c%V%) %P'

function! s:my_tabline()  "{{{
  let s = ''

  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let curbufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears

    let no = (i <= 10 ? i - 1 : '#')  " display 0-origin tabpagenr.
    let mod = getbufvar(curbufnr, '&modified') ? '+' : ' '
    let title = gettabvar(i, 'title')
    let title = len(title) ? title : fnamemodify(bufname(curbufnr),':t')
    let title = len(title) ? title : '[No Name]'

    let s .= '%' . i . 'T'
    let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
    let s .= no
    let s .= mod
    let s .= title
    let s .= '%#TabLineFill#'
    let s .= '  '
  endfor

  let s .= '%#TabLineFill#%T'
  let s .= '%=%#TabLine#'
  let s .= exists('t:cwd') ? '| %<' . fnamemodify(t:cwd, ':p:~:h') : ''
  return s
endfunction "}}}
let &tabline = '%!' . s:SID_PREFIX() . 'my_tabline()'


let g:mapleader = ','
let g:maplocalleader = '.'




" Misc.  "{{{2

" Use this group for any autocmd defined in this file.
augroup MyAutoCmd
  autocmd!
augroup END

call altercmd#load()
call arpeggio#load()
call rtputil#bundle()




" Syntax  {{{1
" Stuffs  "{{{2

let s:FALSE = 0
let s:TRUE = !s:FALSE




" BufferCleanr - delete unnecessary buffer  "{{{2

command! -bang -nargs=0 BufferCleaner  call s:cmd_BufferCleaner(<bang>0)
function! s:cmd_BufferCleaner(banged_p)
  let _ = range(1, bufnr('$'))
  call filter(_, 'bufexists(v:val) &&
  \               buflisted(v:val) &&
  \               (bufname(v:val) == "" || !filereadable(bufname(v:val))) &&
  \               (a:banged_p || !getbufvar(v:val, "&modified"))')
  for bufnr in _
    silent execute bufnr 'bdelete'.(a:banged_p ? '!' : '')
  endfor
  echo len(_) 'buffer deleted'
endfunction




" CD - wrapper of :cd to keep cwd for each tabpage  "{{{2

command! -complete=dir -nargs=* CD
\   if <q-args> == '' && expand('%') != ''
\ |   cd %:p:h
\ | else
\ |   cd <args>
\ | endif
\ | let t:cwd = getcwd()

AlterCommand cd  CD

autocmd MyAutoCmd TabEnter *
\   if exists('t:cwd')
\ |   if isdirectory(t:cwd)
\ |     cd `=t:cwd`
\ |   else
\ |     unlet t:cwd
\ |   endif
\ | endif

autocmd MyAutoCmd BufEnter,BufReadPost ?*
\   if !exists('t:cwd') && buflisted(bufnr('%')) && filereadable(expand('%'))
\ |   cd %:p:h
\ |   let t:cwd = getcwd()
\ | endif




" Hecho, Hechon, Hechomsg - various :echo with highlight specification  "{{{2

command! -bar -nargs=+ Hecho  call s:cmd_Hecho('echo', [<f-args>])
command! -bar -nargs=+ Hechon  call s:cmd_Hecho('echon', [<f-args>])
command! -bar -nargs=+ Hechomsg  call s:cmd_Hecho('echomsg', [<f-args>])
function! s:cmd_Hecho(echo_command, args)
  let highlight_name = a:args[0]
  let messages = a:args[1:]

  execute 'echohl' highlight_name
  execute a:echo_command join(messages)
  echohl None
endfunction




" HelpTagsAll  "{{{2

command! -bang -nargs=0 HelpTagsAll  call rtputil#helptags(<bang>0)




" Rename - rename file and buffer  "{{{2

command! -complete=file -nargs=1 Rename  call s:cmd_Rename(<q-args>)
function! s:cmd_Rename(name)
  let current = expand('%')
  if &l:readonly || !&l:modifiable || !filewritable(current)
    Hecho ErrorMsg 'This file cannot be changes'
  elseif filereadable(a:name)
    Hecho ErrorMsg 'Renamed file already exists'
  else
    file `=a:name`
    call delete(current)
    write
    redraw
    echo 'Renamed:' current '->' a:name
  endif
endfunction




" Sequence - sequence number substitutions  "{{{2
"
" :Sequence {pattern} [format] [first] [step]
"
"   Example: Print the number in front of each line:
"   Sequence ^ '%03d ' 1

command! -range -nargs=+ Sequence
\ <line1>,<line2>call s:cmd_Sequence(<q-args>)
function! s:cmd_Sequence(args) range
  let args = vimproc#parser#split_args(a:args)
  let incrementor = {
  \   'format': get(args, 1, '%d'),
  \   'current': get(args, 2, 0),
  \   'step': get(args, 3, 1)
  \ }

  function incrementor.call() dict
    let next = printf(self.format, self.current)
    let self.current += self.step
    return next
  endfunction

  execute printf('%d,%ds/%s/\=incrementor.call()/g',
  \              a:firstline,
  \              a:lastline,
  \              escape(args[0], '/'))
endfunction




" Source - wrapper of :source with echo  "{{{2

command! -bar -complete=file -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>

AlterCommand so[urce]  Source




" SuspendWithAutomticCD  "{{{2

command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCD()
function! s:cmd_SuspendWithAutomticCD()
  let shell = split(&shell, '/')[-1]
  if exists('$TMUX')
    let windows = split(vimproc#system('tmux list-windows'), '\n')
    call map(windows, 'split(v:val, "^\\d\\+\\zs:\\s")')
    call filter(windows, 'matchstr(v:val[1], "\\S\\+") ==# shell')
    let select_command = empty(windows)
    \                  ? 'new-window'
    \                  : 'select-window -t ' . windows[0][0]
    silent execute '!tmux'
    \              select_command '\;'
    \              'send-keys C-u " cd \"'.getcwd().'\"" C-m'
    redraw!
  elseif exists('$WINDOW')
    silent execute '!screen -X eval'
    \              '''select '.shell.''''
    \              '''stuff "\025 cd \\"'.getcwd().'\\" \015"'''
    redraw!
  else
    suspend
  endif
endfunction




" TabpageTitle - name the current tabpage  "{{{2

command! -bar -nargs=* TabpageTitle
\   if <q-args> == ''
\ |   let t:title = input("Set tabpage's title to: ", get(t:, 'title', ''))
\ | else
\ |   let t:title = <q-args>
\ | endif
\ | redraw!




" Utf8 and others - :edit with specified 'fileencoding'  "{{{2

command! -bang -bar -complete=file -nargs=? Cp932
\ edit<bang> ++enc=cp932 <args>
command! -bang -bar -complete=file -nargs=? Eucjp
\ edit<bang> ++enc=euc-jp <args>
command! -bang -bar -complete=file -nargs=? Iso2022jp
\ edit<bang> ++enc=iso-2022-jp <args>
command! -bang -bar -complete=file -nargs=? Utf8
\ edit<bang> ++enc=utf-8 <args>
command! -bang -bar -complete=file -nargs=? Utf16be
\ edit<bang> ++enc=utf-16be <args>
command! -bang -bar -complete=file -nargs=? Utf16
\ edit<bang> ++enc=utf-16le <args>
command! -bang -bar -complete=file -nargs=? Utf32be
\ edit<bang> ++enc=utf-32be <args>
command! -bang -bar -complete=file -nargs=? Utf32
\ edit<bang> ++enc=utf-32le <args>

command! -bang -bar -complete=file -nargs=? Jis  Iso2022jp<bang> <args>
command! -bang -bar -complete=file -nargs=? Sjis  Cp932<bang> <args>
command! -bang -bar -complete=file -nargs=? Unicode  Utf16<bang> <args>




" Utilities  "{{{1
" :grep wrappers  "{{{2

command! -bar -complete=file -nargs=+ Grep  call s:grep('grep', [<f-args>])
command! -bar -complete=file -nargs=+ Lgrep  call s:grep('lgrep', [<f-args>])
function! s:grep(command, args)
  let target = len(a:args) > 1 ? join(a:args[:-2]) : '**/*'
  let grepprg = &l:grepprg == '' ? &grepprg : &l:grepprg

  if grepprg ==# 'internal'
    execute a:command '/'.escape(a:args[-1], '/ ').'/j' target
  elseif has('win32') || has('win64')
    try
      let original_errorformat = &l:errorformat
      let &l:errorformat = &l:grepformat
      let result = vimproc#system(grepprg . " " . a:args[-1] . " " . target)
      if a:command ==# 'grep'
        cgetexpr result
      else  " lgrep
        lgetexpr result
      endif
    finally
      let &l:errorformat = original_errorformat
    endtry
  else
    execute a:command.'!' shellescape(a:args[-1]) target
  endif

  if a:command ==# 'grep'
    cwindow
  else  " lgrep
    lwindow
  endif
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  Lgrep




" :make wrappers  "{{{2

command! -bar -complete=file -nargs=* Make  call s:make('make', [<f-args>])
command! -bar -complete=file -nargs=* Lmake  call s:make('lmake', [<f-args>])
function! s:make(command, args)
  let original_winnr = winnr()
  try
    execute a:command.'!' join(a:args)
  catch
    Hecho ErrorMsg v:exception
  endtry

  if a:command ==# 'make'
    cwindow
  else  " lmake
    lwindow
  endif
  execute original_winnr 'wincmd w'
endfunction

AlterCommand mak[e]  Make
AlterCommand lmak[e]  Lmake




" :setlocal wrappers  "{{{2

command! -nargs=? -complete=filetype SetFileType
\ setlocal filetype=<args> | silent! SkeletonLoad <args>
command! -nargs=? -complete=customlist,s:complete_fileencoding SetFileEncoding
\ setlocal fileencoding=<args>
command! -nargs=? -complete=customlist,s:complete_fileformats SetFileFormat
\ setlocal fileformat=<args>

function! s:complete_fileencoding(arglead, cmdline, cursorpos)
  " {encoding: 0} see :help encoding-values.  "{{{
  let _ = {
  \  'ansi': 0,
  \  'big5': 0,
  \  'chinese': 0,
  \  'cp1250': 0,
  \  'cp1251': 0,
  \  'cp1253': 0,
  \  'cp1254': 0,
  \  'cp1255': 0,
  \  'cp1256': 0,
  \  'cp1257': 0,
  \  'cp1258': 0,
  \  'cp437': 0,
  \  'cp737': 0,
  \  'cp775': 0,
  \  'cp850': 0,
  \  'cp852': 0,
  \  'cp855': 0,
  \  'cp857': 0,
  \  'cp860': 0,
  \  'cp861': 0,
  \  'cp862': 0,
  \  'cp863': 0,
  \  'cp865': 0,
  \  'cp866': 0,
  \  'cp869': 0,
  \  'cp874': 0,
  \  'cp932': 0,
  \  'cp936': 0,
  \  'cp949': 0,
  \  'cp950': 0,
  \  'default': 0,
  \  'euc-cn': 0,
  \  'euc-jp': 0,
  \  'euc-kr': 0,
  \  'euc-tw': 0,
  \  'iso-8859-10': 0,
  \  'iso-8859-11': 0,
  \  'iso-8859-12': 0,
  \  'iso-8859-13': 0,
  \  'iso-8859-14': 0,
  \  'iso-8859-15': 0,
  \  'iso-8859-16': 0,
  \  'iso-8859-1': 0,
  \  'iso-8859-2': 0,
  \  'iso-8859-3': 0,
  \  'iso-8859-4': 0,
  \  'iso-8859-5': 0,
  \  'iso-8859-6': 0,
  \  'iso-8859-7': 0,
  \  'iso-8859-8': 0,
  \  'iso-8859-9': 0,
  \  'japan': 0,
  \  'koi8-r': 0,
  \  'koi8-u': 0,
  \  'korea': 0,
  \  'latin1': 0,
  \  'macroman': 0,
  \  'prc': 0,
  \  'sjis': 0,
  \  'taiwan': 0,
  \  'ucs-2': 0,
  \  'ucs-2be': 0,
  \  'ucs-2le': 0,
  \  'ucs-4': 0,
  \  'ucs-4be': 0,
  \  'ucs-4le': 0,
  \  'ucs2be': 0,
  \  'unicode': 0,
  \  'utf-16': 0,
  \  'utf-16le': 0,
  \  'utf-32': 0,
  \  'utf-32le': 0,
  \  'utf-8': 0,
  \  'utf8': 0,
  \ }  " }}}
  for encoding in split(&fileencodings, ',')
    let _[encoding] = 0
  endfor
  return sort(filter(keys(_), 's:prefix_of_p(a:arglead, v:val)'))
endfunction
function! s:complete_fileformats(arglead, cmdline, cursorpos)
  return sort(filter(split(&fileformats, ','), 's:prefix_of_p(a:arglead, v:val)'))
endfunction




" Jump sections  "{{{2

" for normal mode.  a:pattern is '/regexp' or '?regexp'.
function! s:jump_section_n(pattern)
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


" for visual mode.  a:motion is '[[', '[]', ']]' or ']['.
function! s:jump_section_v(motion)
  execute 'normal!' "gv\<Esc>"
  execute 'normal' v:count1 . a:motion
  let line = line('.')
  let col = col('.')

  normal! gv
  call cursor(line, col)
endfunction


" for operator-pending mode.  a:motion is '[[', '[]', ']]' or ']['.
function! s:jump_section_o(motion)
  execute 'normal' v:count1 . a:motion
endfunction




" Toggle options  "{{{2

function! s:toggle_grepprg(global_p)
  let VALUES = ['grep -nHE', 'git grep -n']
  let grepprg = &l:grepprg == '' ? &grepprg : &l:grepprg
  let i = (index(VALUES, grepprg) + 1) % len(VALUES)

  if a:global_p
    let &grepprg = VALUES[i]
    set grepprg?
  else
    let &l:grepprg = VALUES[i]
    setlocal grepprg?
  endif
endfunction
if has('vim_starting')
  silent call s:toggle_grepprg(s:TRUE)
endif


function! s:toggle_option(option_name)
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
endfunction


function! s:toggle_colorcolumn()
  if exists('b:textwidth')
    let &l:textwidth = b:textwidth
    unlet b:textwidth
    setlocal colorcolumn& colorcolumn?
  else
    let b:textwidth = &l:textwidth
    if b:textwidth == 0
      set textwidth=80
    endif
    setlocal colorcolumn=+1 colorcolumn?
  endif
endfunction




" Vertical with  "{{{2

let s:vertical_p = '(winwidth(0) * 2 > winheight(0) * 8)'

function! s:vertical_with(command, args)
  execute eval(s:vertical_p) ? 'vertical' : ''
  \       a:command
  \       join(a:args)
endfunction

command! -bar -complete=file -nargs=* Split
\ call s:vertical_with('split', [<f-args>])
command! -bar -complete=file -nargs=* SplitTop
\ call s:vertical_with('topleft', ['split', <f-args>])
command! -bar -complete=file -nargs=* SplitBottom
\ call s:vertical_with('botright', ['split', <f-args>])
command! -bar -complete=help -nargs=* Help
\ call s:vertical_with('help', [<f-args>])
command! -bar -complete=file -nargs=* New
\ call s:vertical_with('new', [<f-args>])

command! -bar -complete=file -nargs=* SplitLeft  SplitTop <args>
command! -bar -complete=file -nargs=* SplitRight  SplitBottom <args>

AlterCommand sp[lit]  Split
AlterCommand h[elp]  Help
AlterCommand new  New




function! s:all_combinations(xs)  "{{{2
  let cs = []

  for r in range(1, len(a:xs))
    call extend(cs, s:combinations(a:xs, r))
  endfor

  return cs
endfunction




function! s:close_temporary_windows()  "{{{2
  let _ = range(1, winnr('$'))
  let pattern = '^nofile\|quickfix\|help'
  call filter(_, '!buflisted(winbufnr(v:val)) &&
  \               getbufvar(winbufnr(v:val), "&buftype") =~# pattern')

  let current_winnr = winnr()
  if len(_) == winnr('$')
    call filter(_, 'current_winnr != v:val')
  endif

  for winnr in _
    execute winnr 'wincmd w'
    wincmd c
  endfor
  execute (current_winnr - len(filter(_, 'v:val < current_winnr')))
  \       'wincmd w'
endfunction




function! s:combinations(pool, r)  "{{{2
  let n = len(a:pool)
  if n < a:r || a:r <= 0
    return []
  endif

  let result = []

  let indices = range(a:r)
  call add(result, join(map(copy(indices), 'a:pool[v:val]'), ''))

  while s:TRUE
    let broken_p = s:FALSE
    for i in reverse(range(a:r))
      if indices[i] != i + n - a:r
        let broken_p = s:TRUE
        break
      endif
    endfor
    if !broken_p
      break
    endif

    let indices[i] += 1
    for j in range(i + 1, a:r - 1)
      let indices[j] = indices[j-1] + 1
    endfor
    call add(result, join(map(copy(indices), 'a:pool[v:val]'), ''))
  endwhile

  return result
endfunction




function! s:keys_to_complete()  "{{{2
  if &l:completefunc != ''
    return "\<C-x>\<C-u>"
  elseif &l:omnifunc != ''
    return "\<C-x>\<C-o>"
  else
    return "\<C-n>"
  endif
endfunction




function! s:move_window_into_tabpage(target_tabpagenr)  "{{{2
  " Move the current window into a:target_tabpagenr.
  if a:target_tabpagenr <= 0  " ignore invalid number.
    return
  endif
  let original_tabnr = tabpagenr()
  let target_bufnr = bufnr('')
  let window_view = winsaveview()

  if a:target_tabpagenr > tabpagenr('$')
    tabnew
    tabmove  " Move new tabpage at the last.
    execute target_bufnr 'buffer'
    let target_tabpagenr = tabpagenr()
  else
    execute a:target_tabpagenr 'tabnext'
    let target_tabpagenr = a:target_tabpagenr
    if winnr('$') > 1 || bufname(winbufnr(0)) != ''
      SplitTop
    endif
    execute target_bufnr 'buffer'
  endif
  call winrestview(window_view)

  execute original_tabnr 'tabnext'
  if winnr('$') > 1
    close
  else
    let target_tabpagenr -= tabpagenr() < target_tabpagenr
    tabclose
  endif

  execute target_tabpagenr 'tabnext'
endfunction




function! s:operator_search(motion_wiseness)  "{{{2
  let visual_commnad =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  let search_command = v:searchforward ? 'n' : 'N'
  let region = join(map(s:region("'[", "']", visual_commnad),
  \                     'escape(v:val, "\\/")'),
  \                 '\n')
  let @/ = '\V' . region
  call histadd('/', '\V' . region)
  silent execute 'normal!' search_command
endfunction




function! s:operator_translate(motion_wiseness)  "{{{2
  let visual_commnad =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  let query = join(s:region("'[", "']", visual_commnad), "\n")

  let api = 'http://translate.google.com/translate_a/t'
  let response = http#get(api, {
  \   'client': 'o',
  \   'hl': 'en',
  \   'sl': 'en',
  \   'tl': 'ja',
  \   'text': query
  \ }, {'User-Agent': 'Mozilla/5.0'})
  if response.header[0] ==# 'HTTP/1.1 200 OK'
    let result = json#decode(response.content)
    echo join(map(result.sentences, 'v:val.trans'))
  else
    echoerr response.header[0]
  end
endfunction




function! s:operator_yank_clipboard(motion_wiseness)  "{{{2
  let visual_commnad =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_commnad.'`]"+y'
endfunction




function! s:prefix_of_p(x, y)  "{{{2
  return a:x ==# strpart(a:y, 0, len(a:x))
endfunction




function! s:region(expr1, expr2, visual_commnad)  "{{{2
  let [lnum1, col1] = getpos(a:expr1)[1:2]
  let [lnum2, col2] = getpos(a:expr2)[1:2]
  let region = getline(lnum1, lnum2)

  if a:visual_commnad ==# "v"  " char
    if lnum1 == lnum2  " single line
      let region[0] = s:strpart(region[-1], col1 - 1, col2 - (col1 - 1))
    else  " multi line
      let region[0] = s:strpart(region[0], col1 - 1)
      let region[-1] = s:strpart(region[-1], 0, col2)
    endif
  elseif a:visual_commnad ==# "V"  " line
    let region += ['']
  else  " block
    call map(region, 's:strpart(v:val, col1 - 1, col2 - (col1 - 1))')
  endif

  return region
endfunction




function! s:set_short_indent(...)  "{{{2
  let _ = a:0 ? a:1 : 2
  let [&l:expandtab, &l:softtabstop, &l:shiftwidth] = [1, _, _]
endfunction




function! s:strpart(src, start, ...)  "{{{2
  let str = strpart(a:src, a:start)
  if a:0 > 0
    let i = byteidx(strpart(str, a:1 - 1), 1) - 1
    return i == -1 ? str : strpart(str, 0, a:1 + i)
  else
    return str
  endif
endfunction




" Mappings  "{{{1
" Terminal-GUI interoperability  "{{{2

" <M-{x}> => <Esc>x
function! s:emulate_meta_esc_behavior_in_terminal()
  " [key, acceptable-modifiers-except-meta]  "{{{
  let keys = [
  \   ['!', ''],
  \   ['"', ''],
  \   ['#', ''],
  \   ['$', ''],
  \   ['%', ''],
  \   ['&', ''],
  \   ['''', ''],
  \   ['(', ''],
  \   [')', ''],
  \   ['*', ''],
  \   ['+', ''],
  \   [',', ''],
  \   ['-', ''],
  \   ['.', ''],
  \   ['0', ''],
  \   ['1', ''],
  \   ['2', ''],
  \   ['3', ''],
  \   ['4', ''],
  \   ['5', ''],
  \   ['6', ''],
  \   ['7', ''],
  \   ['8', ''],
  \   ['9', ''],
  \   [':', ''],
  \   [';', ''],
  \   ['<BS>', 'CS'],
  \   ['<Bar>', ''],
  \   ['<Bslash>', 'C'],
  \   ['<Del>', 'CS'],
  \   ['<Down>', 'CS'],
  \   ['<End>', 'CS'],
  \   ['<Esc>', 'CS'],
  \   ['<F10>', 'CS'],
  \   ['<F11>', 'CS'],
  \   ['<F12>', 'CS'],
  \   ['<F1>', 'CS'],
  \   ['<F2>', 'CS'],
  \   ['<F3>', 'CS'],
  \   ['<F4>', 'CS'],
  \   ['<F5>', 'CS'],
  \   ['<F6>', 'CS'],
  \   ['<F7>', 'CS'],
  \   ['<F9>', 'CS'],
  \   ['<F9>', 'CS'],
  \   ['<Home>', 'CS'],
  \   ['<LT>', ''],
  \   ['<Left>', 'CS'],
  \   ['<PageDown>', 'CS'],
  \   ['<PageUp>', 'CS'],
  \   ['<Return>', 'CS'],
  \   ['<Right>', 'CS'],
  \   ['<Space>', 'CS'],
  \   ['<Tab>', 'CS'],
  \   ['<Up>', 'CS'],
  \   ['=', ''],
  \   ['>', ''],
  \   ['@', 'C'],
  \   ['A', ''],
  \   ['B', ''],
  \   ['C', ''],
  \   ['D', ''],
  \   ['E', ''],
  \   ['F', ''],
  \   ['G', ''],
  \   ['H', ''],
  \   ['I', ''],
  \   ['J', ''],
  \   ['K', ''],
  \   ['L', ''],
  \   ['M', ''],
  \   ['N', ''],
  \   ['O', ''],
  \   ['P', ''],
  \   ['Q', ''],
  \   ['R', ''],
  \   ['S', ''],
  \   ['T', ''],
  \   ['U', ''],
  \   ['V', ''],
  \   ['W', ''],
  \   ['X', ''],
  \   ['Y', ''],
  \   ['Z', ''],
  \   ['[', 'C'],
  \   [']', 'C'],
  \   ['^', 'C'],
  \   ['_', 'C'],
  \   ['`', ''],
  \   ['a', 'C'],
  \   ['b', 'C'],
  \   ['c', 'C'],
  \   ['d', 'C'],
  \   ['e', 'C'],
  \   ['f', 'C'],
  \   ['g', 'C'],
  \   ['h', 'C'],
  \   ['i', 'C'],
  \   ['j', 'C'],
  \   ['k', 'C'],
  \   ['l', 'C'],
  \   ['m', 'C'],
  \   ['n', 'C'],
  \   ['o', 'C'],
  \   ['p', 'C'],
  \   ['q', 'C'],
  \   ['r', 'C'],
  \   ['s', 'C'],
  \   ['t', 'C'],
  \   ['u', 'C'],
  \   ['v', 'C'],
  \   ['w', 'C'],
  \   ['x', 'C'],
  \   ['y', 'C'],
  \   ['z', 'C'],
  \   ['{', ''],
  \   ['}', ''],
  \   ['~', ''],
  \ ]
  "}}}

  for [key, modifiers] in keys
    let k = matchstr(key, '^<\zs.*\ze>$\|.*')

    for map in ['map', 'map!']
      execute map '<M-'.k.'>'  '<Esc>'.key
      for m in s:modifier_combinations(modifiers)
        execute map '<M-'.m.k.'>'  '<Esc><'.m.k.'>'
      endfor
    endfor
  endfor
endfunction

function! s:modifier_combinations(modifiers)
  let prefixes = map(range(len(a:modifiers)), 'a:modifiers[v:val] . "-"')
  return s:all_combinations(prefixes)
endfunction

if has('gui_running')
  " NUL
  map <C-Space>  <C-@>
  map! <C-Space>  <C-@>

  noremap! <S-Insert>  <C-r>*

  call s:emulate_meta_esc_behavior_in_terminal()
endif




" Tag jumping  "{{{2
" Fallback  "{{{3

" the prefix key.
nnoremap <Plug>(arpeggio-default:t)  <Nop>


" Basic  "{{{3

nnoremap tt  <C-]>
vnoremap tt  <C-]>
nnoremap <silent> tj  :tag<CR>
nnoremap <silent> tk  :pop<CR>
nnoremap <silent> tl  :<C-u>tags<CR>
nnoremap <silent> tn  :tnext<CR>
nnoremap <silent> tp  :tprevious<CR>
nnoremap <silent> tP  :<C-u>tfirst<CR>
nnoremap <silent> tN  :<C-u>tlast<CR>

" additions, like Web browsers
nnoremap <expr> <CR>  &l:filetype ==# 'qf' ? "\<CR>" : "\<C-]>"
vnoremap <expr> <CR>  &l:filetype ==# 'qf' ? "\<CR>" : "\<C-]>"

" addition, interactive use.
nnoremap t<Space>  :<C-u>tag<Space>


" With the preview window  "{{{3

nnoremap t't  <C-w>}
vnoremap t't  <C-w>}
nnoremap <silent> t'n  :ptnext<CR>
nnoremap <silent> t'p  :ptprevious<CR>
nnoremap <silent> t'P  :<C-u>ptfirst<CR>
nnoremap <silent> t'N  :<C-u>ptlast<CR>
nnoremap <silent> t'c  :<C-u>pclose<CR>


" With :split  "{{{3

nnoremap <silent> tst  :<c-u>call <SID>vertical_with('wincmd', [']'])<CR>
vnoremap <silent> tst  :<c-u>call <SID>vertical_with('wincmd', [']'])<CR>




" QuickFix  "{{{2
" Fallback  "{{{3

" The prefix key.
noremap q  <Nop>

" Alternative key for the original action.
nnoremap Q  q


" For quickfix list  "{{{3

nnoremap <silent> qj  :cnext<CR>
nnoremap <silent> qk  :cprevious<CR>
nnoremap <silent> qr  :<C-u>crewind<CR>
nnoremap <silent> qK  :<C-u>cfirst<CR>
nnoremap <silent> qJ  :<C-u>clast<CR>
nnoremap <silent> qfj  :<C-u>cnfile<CR>
nnoremap <silent> qfk  :<C-u>cpfile<CR>
nnoremap <silent> ql  :<C-u>clist<CR>
nnoremap <silent> qq  :<C-u>cc<CR>
nnoremap <silent> qo  :<C-u>copen<CR>
nnoremap <silent> qc  :<C-u>cclose<CR>
nnoremap <silent> qp  :<C-u>colder<CR>
nnoremap <silent> qn  :<C-u>cnewer<CR>
nnoremap <silent> qm  :<C-u>Make<CR>
nnoremap qM  :<C-u>Make<Space>
nnoremap q<Space>  :<C-u>Make<Space>
nnoremap qg  :<C-u>Grep<Space>


" For location list (mnemonic: Quickfix list for the current Window)  "{{{3

nnoremap <silent> qwj  :lnext<CR>
nnoremap <silent> qwk  :lprevious<CR>
nnoremap <silent> qwr  :<C-u>lrewind<CR>
nnoremap <silent> qwK  :<C-u>lfirst<CR>
nnoremap <silent> qwJ  :<C-u>llast<CR>
nnoremap <silent> qwfj  :<C-u>lnfile<CR>
nnoremap <silent> qwfk  :<C-u>lpfile<CR>
nnoremap <silent> qwl  :<C-u>llist<CR>
nnoremap <silent> qwq  :<C-u>ll<CR>
nnoremap <silent> qwo  :<C-u>lopen<CR>
nnoremap <silent> qwc  :<C-u>close<CR>
nnoremap <silent> qwp  :<C-u>lolder<CR>
nnoremap <silent> qwn  :<C-u>lnewer<CR>
nnoremap <silent> qwm  :<C-u>Lmake<CR>
nnoremap qwM  :<C-u>Lmake<Space>
nnoremap qw<Space>  :<C-u>Lmake<Space>
nnoremap qwg  :<C-u>Lgrep<Space>




" Tab pages  "{{{2
" Fallback  "{{{3

" the prefix key.
noremap <C-t>  <Nop>


" Basic  "{{{3

" Move new tabpage at the last.
nnoremap <silent> <C-t>n  :<C-u>tabnew \| :tabmove<CR>
nnoremap <silent> <C-t>c  :<C-u>tabclose<CR>
nnoremap <silent> <C-t>o  :<C-u>tabonly<CR>
nnoremap <silent> <C-t>i  :<C-u>tabs<CR>

nmap <C-t><C-n>  <C-t>n
nmap <C-t><C-c>  <C-t>c
nmap <C-t><C-o>  <C-t>o
nmap <C-t><C-i>  <C-t>i

nnoremap <silent> <C-t><Space>  :<C-u>TabpageTitle<CR>

nmap <silent> <C-t><C-@>  <C-t><Space>
nmap <silent> <C-t><C-Space>  <C-t><Space>


" Moving around tabpages.  "{{{3

nnoremap <silent> <C-t>j
\ :<C-u>execute 'tabnext' 1 + (tabpagenr() + v:count1 - 1) % tabpagenr('$')<CR>
nnoremap <silent> <C-t>k
\ :<C-u>execute 'tabprevious' v:count1 % tabpagenr('$')<CR>
nnoremap <silent> <C-t>K  :<C-u>tabfirst<CR>
nnoremap <silent> <C-t>J  :<C-u>tablast<CR>

nmap <C-t><C-j>  <C-t>j
nmap <C-t><C-k>  <C-t>k
nmap <C-t><C-t>  <C-t>j

" GNU screen like mappings.
" Note that the numbers in {lhs}s are 0-origin.  See also 'tabline'.
for i in range(10)
  execute 'nnoremap <silent>' ('<C-t>'.(i))  ((i+1).'gt')
endfor
unlet i


" Moving tabpages themselves.  "{{{3

nnoremap <silent> <C-t>l
\ :<C-u>execute 'tabmove' min([tabpagenr() + v:count1 - 1, tabpagenr('$')])<CR>
nnoremap <silent> <C-t>h
\ :<C-u>execute 'tabmove' max([tabpagenr() - v:count1 - 1, 0])<CR>
nnoremap <silent> <C-t>L  :<C-u>tabmove<CR>
nnoremap <silent> <C-t>H  :<C-u>tabmove 0<CR>

nmap <C-t><C-l>  <C-t>l
nmap <C-t><C-h>  <C-t>h




" Argument list  "{{{2

" the prefix key.
noremap <C-g>  <Nop>


nnoremap <silent> <C-g>l  :args<CR>
nnoremap <silent> <C-g>j  :next<CR>
nnoremap <silent> <C-g>k  :previous<CR>
nnoremap <silent> <C-g>J  :last<CR>
nnoremap <silent> <C-g>K  :first<CR>
nnoremap <silent> <C-g>wj  :wnext<CR>
nnoremap <silent> <C-g>wk  :wprevious<CR>
nnoremap <C-g><Space>  :<C-u>args<Space>

nmap <C-g><C-l>  <C-g>l
nmap <C-g><C-j>  <C-g>j
nmap <C-g><C-k>  <C-g>k
nmap <C-g><C-w><C-j>  <C-g>wj
nmap <C-g><C-w><C-k>  <C-g>wk

nmap <C-g><C-@>  <C-g><Space>
nmap <C-g><C-Space>  <C-g><Space>




" Command-line editting  "{{{2

cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-a>  <Home>
cnoremap <C-d>  <Delete>
cnoremap <C-y>  <C-r>"

cnoremap <C-p>  <Up>
cnoremap <C-n>  <Down>
cnoremap <Up>  <C-p>
cnoremap <Down>  <C-n>

" Like emacs kill-line.
cnoremap <C-k>
\ <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>


" Escape Command-line mode if the command line is empty (like <C-h>)
cnoremap <expr> <C-u>  getcmdline() == '' ? "\<C-c>" : "\<C-u>"
cnoremap <expr> <C-w>  getcmdline() == '' ? "\<C-c>" : "\<C-w>"

" Search slashes easily (too lazy to prefix backslashes to slashes)
cnoremap <expr> /  getcmdtype() == '/' ? '\/' : '/'




" Command-line window  "{{{2

autocmd MyAutoCmd CmdwinEnter *
\ call s:on_CmdwinEnter()

function! s:on_CmdwinEnter()
  nnoremap <buffer> <Esc><Esc>  <Esc><C-w>q
  inoremap <buffer> <Esc><Esc>  <Esc><C-w>q
  inoremap <buffer> <expr> <C-c>  pumvisible() ? "\<Esc>" : "\<C-c>\<C-c>"
  inoremap <buffer> <expr> <BS>
  \        getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<BS>"
  inoremap <buffer> <expr> <C-w>
  \        getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<C-w>"
  inoremap <buffer> <expr> <C-u>
  \        getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<C-u>"
  imap <buffer> <C-h>  <BS>

  startinsert!
endfunction




" Insert mode  "{{{2

" Like emacs mappings.
inoremap <C-b>  <Left>
inoremap <C-f>  <Right>
inoremap <C-a>  <Home>
inoremap <C-e>  <End>
inoremap <C-d>  <Delete>
inoremap <expr> <C-k>  repeat("\<Delete>", max([col('$') - col('.'), 1]))
inoremap <expr> <C-y>  pumvisible() ? "\<C-y>" : "\<C-r>+"

" Alternatives for the original actions.
inoremap <C-\>  <C-a>
inoremap <C-q>  <C-d>


" To be able to undo these types of deletion.
inoremap <C-w>  <C-g>u<C-w>
inoremap <C-u>  <C-g>u<C-u>


" Complete or indent.
inoremap <expr> <Tab>  pumvisible()
                   \ ? "\<C-n>"
                   \ : <SID>should_indent_rather_than_complete_p()
                   \ ? "\<C-i>"
                   \ : <SID>keys_to_complete()
inoremap <expr> <S-Tab>  pumvisible()
                     \ ? "\<C-p>"
                     \ : <SID>should_indent_rather_than_complete_p()
                     \ ? "\<C-i>"
                     \ : <SID>keys_to_complete()

function! s:should_indent_rather_than_complete_p()
  let m = match(getline('.'), '\S')
  return m == -1 || col('.') - 1 <= m
endfunction




" The <Space>  "{{{2

" to show <Space> in the bottom line.
map <Space>  [Space]

" fallback
noremap [Space]  <Nop>


nnoremap [Space]f  <Nop>
nnoremap [Space]fe  :<C-u>SetFileEncoding<Space>
nnoremap [Space]ff  :<C-u>SetFileFormat<Space>
nnoremap [Space]ft  :<C-u>SetFileType<Space>

nnoremap [Space]o  <Nop>
nnoremap <silent> [Space]oc  :<C-u>call <SID>toggle_colorcolumn()<CR>
nnoremap <silent> [Space]og  :<C-u>call <SID>toggle_grepprg(0)<CR>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]op  :<C-u>call <SID>toggle_option('paste')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>

nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>

nnoremap <silent> [Space]c  :<C-u>call <SID>close_temporary_windows()<CR>

nnoremap <silent> [Space]q  :<C-u>Help quickref<CR>
nnoremap <silent> [Space]m  :<C-u>marks<CR>
nnoremap <silent> [Space]r  :<C-u>registers<CR>

nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>


" Open a fold.
nnoremap [Space]l  zo

" Close a fold.
nnoremap [Space]h  zc

" Close all folds but including the cursor.
nnoremap [Space]v  zMzv


" Enter command-line window.
nnoremap [Space]:  q:
xnoremap [Space]:  q:




" Section jumping  "{{{2
"
" Enable *consistent* ]] and other motions in Visual and Operator-pending
" mode.  Because some ftplugins provide these motions only for Normal mode and
" other ftplugins provide these motions with some faults, e.g., not countable.

vnoremap <silent> ]]  :<C-u>call <SID>jump_section_v(']]')<CR>
vnoremap <silent> ][  :<C-u>call <SID>jump_section_v('][')<CR>
vnoremap <silent> [[  :<C-u>call <SID>jump_section_v('[[')<CR>
vnoremap <silent> []  :<C-u>call <SID>jump_section_v('[]')<CR>
onoremap <silent> ]]  :<C-u>call <SID>jump_section_o(']]')<CR>
onoremap <silent> ][  :<C-u>call <SID>jump_section_o('][')<CR>
onoremap <silent> [[  :<C-u>call <SID>jump_section_o('[[')<CR>
onoremap <silent> []  :<C-u>call <SID>jump_section_o('[]')<CR>




" Windows  "{{{2

" Synonyms for the default mappings, with single key strokes.
nnoremap <Tab>  <C-w>w
nnoremap <S-Tab>  <C-w>W


" Search the word nearest to the cursor in new window.
nnoremap <C-w>*  :<C-u>Split \| normal! *<CR>
nnoremap <C-w>#  :<C-u>Split \| normal! #<CR>


" This {lhs} overrides the default action (Move cursor to top-left window).
" But I rarely use its {lhs}s, so this mapping is not problematic.
nnoremap <silent> <C-w>t
\ :call <SID>move_window_into_tabpage(<SID>ask_tabpage_number())<CR>
function! s:ask_tabpage_number()
  echon 'Which tabpage to move this window into? '

  let c = nr2char(getchar())
  if c =~# '[0-9]'
    " Convert 0-origin number (typed by user) into 1-origin number (used by
    " Vim's internal functions).  See also 'tabline'.
    return 1 + char2nr(c) - char2nr('0')
  else
    return 0
  endif
endfunction
nmap <C-w><C-t>  <C-w>t


" Like "<C-w>q", but does ":quit!".
nnoremap <C-w>Q  :<C-u>quit!<CR>


nnoremap <silent> <C-w>y  :<C-u>Split<CR>
nmap <C-w><C-y>  <C-w>y




" Text objects  "{{{2

" Synonyms for <> and [], same as plugin surround.
onoremap aa  a>
vnoremap aa  a>
onoremap ia  i>
vnoremap ia  i>

onoremap ar  a]
vnoremap ar  a]
onoremap ir  i]
vnoremap ir  i]


" Select the last selected text.
onoremap <silent> gv  :<C-u>normal! gv<CR>


onoremap (  t(
vnoremap (  t(
onoremap )  t)
vnoremap )  t)




" Operators  "{{{2

call operator#user#define('search-forward',
\                         s:SID_PREFIX() . 'operator_search',
\                         'let v:searchforward = 1')
call operator#user#define('search-backward',
\                         s:SID_PREFIX() . 'operator_search',
\                         'let v:searchforward = 0')
vmap *  <Plug>(operator-search-forward)
vmap #  <Plug>(operator-search-backward)


call operator#user#define('translate', s:SID_PREFIX() . 'operator_translate')
Arpeggio map ot  <Plug>(operator-translate)


call operator#user#define('yank-clipboard',
\                         s:SID_PREFIX() . 'operator_yank_clipboard')
Arpeggio map oy  <Plug>(operator-yank-clipboard)
Arpeggio map oy  <Plug>(operator-yank-clipboard)




" Misc.  "{{{2

nnoremap <silent> <Leader><Leader>  :<C-u>update<CR>

if has('unix') && executable('sudo')
  nnoremap <silent> <Leader>w  :<C-u>w sudo:%<CR>
endif

nnoremap <C-h>  :<C-u>Help<Space>
nnoremap <C-o>  :<C-u>edit<Space>
nnoremap <C-w>.  :<C-u>edit .<CR>


" Expand with 'l' if the cursor on the holded text.
nnoremap <expr> l  foldclosed(line('.')) != -1 ? 'zo' : 'l'


" Delete a character with the black hole register.
nnoremap X  "_X
nnoremap x  "_x


" "Y" to work from the cursor to the end of line.
nnoremap Y  y$


" Jump list
nnoremap <C-j>  <C-i>
nnoremap <C-k>  <C-o>


" Disable some dangerous key.
nnoremap ZZ  <Nop>
nnoremap ZQ  <Nop>


" Use a backslash (\) to repeat last change.
" Since a dot (.) is used as <LocalLeader>.
nmap \  <Plug>(repeat-.)


" Disable solely typed <Leader>/<LocalLeader> to avoid unexpected behavior.
noremap <Leader>  <Nop>
noremap <LocalLeader>  <Nop>


" Like o/O, but insert additional [count] blank lines.
nnoremap <expr> <Plug>(arpeggio-default:o)
\        <SID>start_insert_mode_with_blank_lines('o')
nnoremap <expr> O
\        <SID>start_insert_mode_with_blank_lines('O')
function! s:start_insert_mode_with_blank_lines(command)
  if v:count != v:count1
    return a:command  " Behave the same as the default commands.
  endif

  if a:command ==# 'o'
    return "\<Esc>o" . repeat("\<CR>", v:count - 1)
  else  " a:command ==# 'O'
    return "\<Esc>O" . repeat("\<CR>\<Up>", v:count - 1) . "\<Esc>S"
  endif
endfunction


noremap <C-z>  <Nop>
nnoremap <C-z>  :<C-u>SuspendWithAutomticCD<CR>


" Show the lines which match to the last search pattern.
nnoremap g/  :global//print<CR>
vnoremap g/  :global//print<CR>


" Alternative <Esc>.
noremap <C-@>  <Esc>
inoremap <C-@>  <Esc>

" c_<Esc> mapped from something doesn't work the same as
" c_<Esc> directly typed by user.
cnoremap <C-@>  <C-c>


" Make searching directions consistent.
nnoremap <expr> n  <SID>search_forward_p() ? 'nzv' : 'Nzv'
nnoremap <expr> N  <SID>search_forward_p() ? 'Nzv' : 'nzv'
vnoremap <expr> n  <SID>search_forward_p() ? 'nzv' : 'Nzv'
vnoremap <expr> N  <SID>search_forward_p() ? 'Nzv' : 'nzv'
onoremap <expr> n  <SID>search_forward_p() ? 'n' : 'N'
onoremap <expr> N  <SID>search_forward_p() ? 'N' : 'n'

function! s:search_forward_p()
  return exists('v:searchforward') ? v:searchforward : s:TRUE
endfunction


" Show the syntax name under the cursor.
nnoremap <silent> gs
\ :<C-u>echo join(<SID>syntax_name_the_cursor(), '/')<CR>
function! s:syntax_name_the_cursor()
  return map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction


" git-diff-aware version of gf commands.
nnoremap <expr> gf  <SID>do_git_diff_aware_gf('gf')
nnoremap <expr> gF  <SID>do_git_diff_aware_gf('gF')
nnoremap <expr> <C-w>f  <SID>do_git_diff_aware_gf('<C-w>f')
nnoremap <expr> <C-w><C-f>  <SID>do_git_diff_aware_gf('<C-w><C-f>')
nnoremap <expr> <C-w>F  <SID>do_git_diff_aware_gf('<C-w>F')
nnoremap <expr> <C-w>gf  <SID>do_git_diff_aware_gf('<C-w>gf')
nnoremap <expr> <C-w>gF  <SID>do_git_diff_aware_gf('<C-w>gF')

function! s:do_git_diff_aware_gf(command)
  let target_path = expand('<cfile>')
  if target_path =~# '^[ab]/'  " with a peculiar prefix of git-diff(1)?
    if filereadable(target_path) || isdirectory(target_path)
      return a:command
    else
      " BUGS: Side effect - Cursor position is changed.
      let [_, c] = searchpos('\f\+', 'cenW')
      return c . '|' . 'v' . (len(target_path) - 2 - 1) . 'h' . a:command
    endif
  else
    return a:command
  endif
endfunction




" Filetypes  "{{{1
" All filetypes   "{{{2

autocmd MyAutoCmd FileType *
\ call s:on_FileType_any()

function! s:on_FileType_any()
  " Add 'dictionary' for filetype.
  let dictionary = expand('~/.vim/dict/') . &l:filetype . '.dict'
  if filereadable(dictionary)
    let &l:dictionary = dictionary
  endif

  " Make omni completion available for all filetypes.
  if &l:omnifunc == ''
    setlocal omnifunc=syntaxcomplete#Complete
  endif

  " Disable auto wrap.
  setlocal formatoptions-=t formatoptions-=c

  " Universal undo for indent scripts.
  if exists('b:undo_indent')
    let b:undo_indent .= ' | '
  else
    let b:undo_indent = ''
  endif
  let b:undo_indent .= 'setlocal
  \ autoindent<
  \ cindent<
  \ cinkeys<
  \ cinoptions<
  \ cinwords<
  \ copyindent<
  \ expandtab<
  \ indentexpr<
  \ indentkeys<
  \ lisp<
  \ lispwords<
  \ preserveindent<
  \ shiftround<
  \ shiftwidth<
  \ smartindent<
  \ smarttab<
  \ softtabstop<
  \ tabstop<
  \ '
endfunction


" Fix 'fileencoding' to use 'encoding'.
autocmd MyAutoCmd BufReadPost *
\   if &l:modifiable && !search('[^\x00-\x7F]', 'cnw', 100)
\ |   setlocal fileencoding=
\ | endif

" When editing a file, always jump to the last cursor position.
autocmd MyAutoCmd BufReadPost *
\   if line('''"') > 0 && line('''"') <= line('$')
\ |   execute 'normal! g''"'
\ | endif

" Unset 'paste' automatically.
autocmd MyAutoCmd InsertLeave *  set nopaste




" changelog  "{{{2

" Fix the new entry mapping bug.
autocmd MyAutoCmd FileType changelog
\ noremap <buffer> <silent> <Leader>o  :<C-u>NewChangelogEntry<CR>

let g:changelog_timeformat = '%Y-%m-%d'
let g:changelog_username  = 'emonkak <emonkak@gmail.com>'




" css  "{{{2

autocmd MyAutoCmd FileType css,sass
\ call s:set_short_indent()




" dosini (.ini)  "{{{2

autocmd MyAutoCmd FileType dosini
\ call s:on_FileType_dosini()

function! s:on_FileType_dosini()
  " Jumping around sections.
  nnoremap <buffer> <silent> ]]  :<C-u>call <SID>jump_section_n('/^\[')<CR>
  nnoremap <buffer> <silent> ][  :<C-u>call <SID>jump_section_n('/\n\[\@=')<CR>
  nnoremap <buffer> <silent> [[  :<C-u>call <SID>jump_section_n('?^\[')<CR>
  nnoremap <buffer> <silent> []  :<C-u>call <SID>jump_section_n('?\n\[\@=')<CR>

  " Folding sections.
  setlocal foldmethod=expr
  let &l:foldexpr = '(getline(v:lnum)[0] == "[") ? ">1" :'
  \               . '(getline(v:lnum) =~# ''^;.*\(__END__\|\*\*\*\)'' ? 0 : "=")'
endfunction




" haskell  "{{{2

autocmd MyAutoCmd FileType haskell
\   call s:set_short_indent()
\ | compiler ghc




" java  "{{{2

autocmd MyAutoCmd FileType java
\   setlocal cinoptions=:0,l1,g0,t0,(0,j1
\ | compiler javac




" javascript  "{{{2

autocmd MyAutoCmd FileType coffee,javascript
\ call s:set_short_indent()




" lua  "{{{2

autocmd MyAutoCmd FileType lua
\ call s:set_short_indent()




" objc  "{{{2

autocmd MyAutoCmd FileType objc
\   call s:set_short_indent()
\ | setlocal commentstring=//%s




" ocaml  "{{{2

autocmd MyAutoCmd FileType ocaml
\   call s:set_short_indent()
\ | setlocal commentstring=(*%s*)




" perl  "{{{2

autocmd MyAutoCmd FileType perl
\   call s:set_short_indent()
\ | setlocal include=




" python  "{{{2

autocmd MyAutoCmd FileType python
\ call s:set_short_indent()

let g:python_highlight_all = 1




" quickfix  "{{{2

autocmd MyAutoCmd FileType qf
\ setlocal nobuflisted nocursorline




" ruby  "{{{2

autocmd MyAutoCmd FileType ruby,yaml
\ call s:set_short_indent()




" scheme  "{{{2

let g:is_gauche = 1




" sh, zsh  "{{{2

autocmd MyAutoCmd FileType sh,zsh
\ call s:set_short_indent()

let g:is_bash = 1




" tex  "{{{2

autocmd MyAutoCmd FileType tex,plaintex
\   call s:on_FileType_tex()
\ | compiler tex

function! s:on_FileType_tex()
  call s:set_short_indent()

  inoreabbrev <buffer> \b  \textbf{}<Left>
  inoreabbrev <buffer> \i  \textit{}<Left>
  inoreabbrev <buffer> \r  \textrm{}<Left>
  inoreabbrev <buffer> \s  \textsf{}<Left>
  inoreabbrev <buffer> \t  \texttt{}<Left>
  inoreabbrev <buffer> \l  \begin{lstlisting}[]<CR><CR>\end{lstlisting}<Up>

  setlocal foldmarker=%{{{,%}}}
  setlocal iskeyword+=-
endfunction

let g:tex_flavor = 'latex'




" vim  "{{{2

autocmd MyAutoCmd FileType vim
\ call s:set_short_indent()

let g:vim_indent_cont = 0




" xml  "{{{2

autocmd MyAutoCmd FileType docbk,html,xhtml,xml,xslt
\ call s:on_FileType_xml()

autocmd MyAutoCmd FileType haml
\ call s:on_FileType_xml()

function! s:on_FileType_xml()
  call s:set_short_indent()

  " To deal with namespace prefixes and tag-name-including-hyphens.
  setlocal iskeyword+=45  " hyphen (-)
  setlocal iskeyword+=58  " colon (:)

  inoremap <buffer> </  </<C-x><C-o>
endfunction




" Plugins  "{{{1
" altr  "{{{2

call altr#reset()
call altr#define('%.m', '%.h')

nmap <F1>  <Plug>(altr-back)
nmap <F2>  <Plug>(altr-forward)




" eskk  "{{{2

autocmd MyAutoCmd User eskk-initialize-pre
\ call s:on_User_eskk_initial_pre()

function! s:on_User_eskk_initial_pre()
  for mode in ['hira', 'kata']
    let table_name = 'rom_to_' . mode
    let table = eskk#table#new(table_name . '*', table_name)
    call table.add_map('~', "\uff5e")
    call table.add_map('(', "\uff08")
    call table.add_map(')', "\uff09")
    call eskk#register_mode_table(mode, table)
  endfor
endfunction

let g:eskk#dictionary = {
\   'path': expand('~/.skk-eskk-jisyo'),
\   'sorted': 0,
\   'encoding': 'utf-8',
\ }
let g:eskk#large_dictionary = {
\   'path': expand('~/.skk/SKK-JISYO.L'),
\   'sorted': 1,
\   'encoding': 'euc-jp',
\ }

let g:eskk#directory = expand('~/.vim/info/eskk')
let g:eskk#egg_like_newline = 1
let g:eskk#enable_completion = 1
let g:eskk#statusline_mode_strings = {
\   'hira': "\u3042",
\   'kata': "\u30a2",
\   'ascii': 'A',
\   'zenei': "\u82f1",
\   'hankata': "\uff76\uff85",
\   'abbrev': '/',
\ }
let g:eskk#use_color_cursor = 0




" exjumplist  "{{{2

nmap <Esc><C-j>  <Plug>(exjumplist-next-buffer)
nmap <Esc><C-k>  <Plug>(exjumplist-previous-buffer)




" grex  "{{{2

nmap gy  <Plug>(operator-grex-yank)<Plug>(textobj-entire-a)
vmap gy  <Plug>(operator-grex-yank)
nmap gd  <Plug>(operator-grex-delete)<Plug>(textobj-entire-a)
vmap gd  <Plug>(operator-grex-delete)




" ku  "{{{2

autocmd MyAutoCmd FileType ku
\ call s:on_FileType_ku()

function! s:on_FileType_ku()
  call ku#default_key_mappings(s:TRUE)

  nmap <buffer> <Esc><Esc>  <Plug>(ku-cancel)
  imap <buffer> <Esc><Esc>  <Plug>(ku-cancel)

  imap <buffer> <expr> <Plug>(ku-%-cancel-when-empty)
  \    col('$') <= 2 ? '<Plug>(ku-cancel)' : ''
  inoremap <buffer> <expr> <Plug>(ku-%-delete-backword-char)
  \        (pumvisible() ? "\<C-e>" : '') . "\<BS>"
  inoremap <buffer> <expr> <Plug>(ku-%-delete-backword-word)
  \        (pumvisible() ? "\<C-e>" : '') . "\<C-w>"
  inoremap <buffer> <expr> <Plug>(ku-%-delete-backword-line)
  \        (pumvisible() ? "\<C-e>" : '') . "\<C-u>"

  imap <buffer> <BS>
  \    <Plug>(ku-%-cancel-when-empty)<Plug>(ku-%-delete-backword-char)
  imap <buffer> <C-w>
  \    <Plug>(ku-%-cancel-when-empty)<Plug>(ku-%-delete-backword-word)
  imap <buffer> <C-u>
  \    <Plug>(ku-%-cancel-when-empty)<Plug>(ku-%-delete-backword-line)
  imap <buffer> <C-h>  <BS>
endfunction


call ku#custom_action('common', 'cd', s:SID_PREFIX().'ku_common_action_my_cd')
call ku#custom_action('common', 'Yank',
\                     s:SID_PREFIX().'ku_common_action_Yank')
call ku#custom_action('common', 'yank',
\                     s:SID_PREFIX().'ku_common_action_yank')
call ku#custom_action('file', 'open-sudo',
\                     s:SID_PREFIX().'ku_file_action_open_sudo')
call ku#custom_action('file/current', 'open-sudo',
\                     s:SID_PREFIX().'ku_file_action_open_sudo')
call ku#custom_action('metarw/git', 'checkout',
\                     s:SID_PREFIX().'ku_metarw_git_action_checkout')

function! s:ku_common_action_my_cd(item)
  if isdirectory(a:item.word)
    CD `=a:item.word`
  else  " treat a:item as a file name
    CD `=fnamemodify(a:item.word, ':h')`
  endif
endfunction

function! s:ku_common_action_yank(item)
  call setreg('"', a:item.word, 'c')
endfunction
function! s:ku_common_action_Yank(item)
  call setreg('"', a:item.word, 'l')
endfunction

function! s:ku_file_action_open_sudo(item)
  edit `='sudo:' . fnamemodify(a:item.word, ':p')`
endfunction

function! s:ku_metarw_git_action_checkout(item)
  if a:item.ku__completed_p
    let branch_name = matchstr(a:item.word, '^git:\zs[^:]\+\ze:')
    let message = system('git checkout ' . shellescape(branch_name))
    if v:shell_error == 0
      echomsg 'git checkout' branch_name
      return 0
    else
      return message
    endif
  else
    return 'No such branch: ' . string(a:item.word)
  endif
endfunction


call ku#custom_key('common', 'y', 'yank')
call ku#custom_key('common', 'Y', 'Yank')
call ku#custom_key('buffer', 'd', 'delete')
call ku#custom_key('file', 's', 'open-sudo')
call ku#custom_key('file/current', 's', 'open-sudo')
call ku#custom_key('metarw/git', '/', 'checkout')
call ku#custom_key('metarw/git', '?', 'checkout')


call ku#custom_prefix('common', '.VIM', expand('~/.vim'))
call ku#custom_prefix('common', 'HOME', expand('~'))
call ku#custom_prefix('common', 'VIM', expand('$VIMRUNTIME'))
call ku#custom_prefix('common', '~', expand('~'))


nmap [Space]k  <Nop>
nnoremap <silent> [Space]ka  :<C-u>Ku args<CR>
nnoremap <silent> [Space]kb  :<C-u>Ku buffer<CR>
nnoremap <silent> [Space]kc  :<C-u>Ku colorscheme<CR>
nnoremap <silent> [Space]kf  :<C-u>Ku file<CR>
nnoremap <silent> [Space]kg  :<C-u>Ku metarw/git<CR>
nnoremap <silent> [Space]kh  :<C-u>Ku history<CR>
nnoremap <silent> [Space]kl  :<C-u>Ku file_rec<CR>
nnoremap <silent> [Space]kq  :<C-u>Ku quickfix<CR>
nnoremap <silent> [Space]kr  :<C-u>Ku register<CR>
nnoremap <silent> [Space]ks  :<C-u>Ku source<CR>
nnoremap <silent> [Space]kt  :<C-u>Ku tags<CR>
nnoremap <silent> [Space]kw  :<C-u>Ku myproject<CR>

nnoremap <silent> [Space]k/  :<C-u>Ku cmd_mru/search<CR>
nnoremap <silent> [Space]k:  :<C-u>Ku cmd_mru/cmd<CR>
nnoremap <silent> [Space]km  :<C-u>Ku file_mru<CR>

nnoremap <silent> [Space]kk  :<C-u>call ku#restart()<CR>


let g:ku_personal_runtime = expand('~/.vim')
let g:ku_file_mru_file = expand('~/.vim/info/ku/mru')
let g:ku_file_mru_ignore_pattern = '/$\|/\.git/\|^/\(/\|mnt\|tmp\)'
let g:ku_file_mru_limit = 200




" neocomplcache  "{{{2

imap <C-l>  <Plug>(neocomplcache_snippets_expand)
smap <C-l>  <Plug>(neocomplcache_snippets_expand)

inoremap <expr> <BS>  neocomplcache#smart_close_popup() . "\<C-h>"
inoremap <expr> <C-h>  neocomplcache#smart_close_popup() . "\<C-h>"


let g:neocomplcache_disable_auto_complete = 0
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_prefetch = 1
let g:neocomplcache_enable_smart_case = 1

let g:neocomplcache_clang_library_path = '/usr/lib/llvm'
if has('python')
  let g:neocomplcache_clang_use_library = 1
endif
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*\|\[ku\]\|\[quickrun output\]'
let g:neocomplcache_temporary_dir = expand('~/.vim/info/neocon')

if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'




" operator-comment  "{{{2

Arpeggio map oc  <Plug>(operator-comment)
Arpeggio map od  <Plug>(operator-uncomment)




" operator-replece  "{{{2

Arpeggio map or  <Plug>(operator-replace)




" operator-sort  "{{{2

nmap [Space]S  <Plug>(operator-sort)$
nmap [Space]s  <Plug>(operator-sort)
vmap [Space]s  <Plug>(operator-sort)




" quickrun  "{{{2

command! -complete=command -nargs=+ Capture  QuickRun vim -src <q-args>

let g:quickrun_config = {
\  '_': {
\    'split': printf('%%{%s ? "vertical" : ""}', s:vertical_p),
\  },
\  'dot': {
\    'exec': ['%c -Tps:cairo -o %s:p:r.ps %s'],
\  },
\  'java': {
\    'exec': ['javac %s', '%c -cp %s:h %s:t:r %a'],
\    'tempfile': '{fnamemodify(tempname(), ":h")}/{expand("%:t")}',
\  },
\  'javaapplet': {
\    'exec': ['echo "<applet code=%s:t:r width=500 height=500></applet>" > %s:p:r.html',
\             'appletviewer %s:p:r.html'],
\    'tempfile': '{fnamemodify(tempname(), ":h")}/{expand("%:t")}',
\  },
\  'javascript': {
\    'type': 'javascript/v8',
\  },
\  'javascript/v8': {
\    'command': executable('d8') ? 'd8' : 'v8',
\    'tempfile': '%{tempname()}.js',
\  },
\  'objc': {
\    'command': 'gcc',
\    'exec': ['%c %o %s -o %s:p:r', '%s:p:r %a'],
\    'tempfile': '%{tempname()}.m',
\  },
\  'tex': {
\    'type': executable('platex') ? 'platex' : '',
\  },
\  'tex/platex': {
\    'exec': ['platex -kanji=utf8 -interaction=nonstopmode -shell-escape -output-directory=%s:p:h %s',
\             'pxdvi %s:p:r.dvi'],
\  },
\  'xdefaults': {
\    'exec': ['xrdb -remove', 'xrdb -merge %s', 'xrdb -query'],
\  },
\ }




" ref  "{{{2

autocmd MyAutoCmd FileType ref
\ call s:on_FileType_ref()

function! s:on_FileType_ref()
  nmap <buffer> <silent> <CR>  <Plug>(ref-keyword)
  vmap <buffer> <silent> <CR>  <Plug>(ref-keyword)
  nmap <buffer> <silent> <C-]>  <Plug>(ref-keyword)
  vmap <buffer> <silent> <C-]>  <Plug>(ref-keyword)
  nmap <buffer> <silent> <C-j>  <Plug>(ref-forward)
  nmap <buffer> <silent> <C-k>  <Plug>(ref-back)
  nnoremap <buffer> q  <C-w>c
endfunction

nmap <silent> K  <Plug>(ref-keyword)
vmap <silent> K  <Plug>(ref-keyword)

nnoremap <silent> <Leader>a  :<C-u>call ref#jump('normal', 'alc2')<CR>
vnoremap <silent> <Leader>a  :<C-u>call ref#jump('visual', 'alc2')<CR>


let g:ref_cache_dir = expand('~/.vim/info/ref')
let g:ref_no_default_key_mappings = 1
let g:ref_open = 'Split'
let g:ref_perldoc_complete_head = 1
let g:ref_wikipedia_lang = 'ja'




" rsense  "{{{2

let g:rsenseHome = '/opt/rsense'
let g:rsenseUseOmniFunc = 1




" scratch  "{{{2

nmap <Leader>s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\ call s:on_User_plugin_scratch_initialize_after()

function! s:on_User_plugin_scratch_initialize_after()
  map <buffer> <CR>  <Plug>(scratch-evaluate!)
endfunction


let g:scratch_show_command = 'SplitTop | hide buffer'




" skeleton  "{{{2

autocmd MyAutoCmd User plugin-skeleton-detect
\ call s:on_User_plugin_skeleton_detect()

function! s:on_User_plugin_skeleton_detect()
  let _ = split(expand('%:p'), '/')
  let extensions = split(_[-1], '\.', 1)
  let directories = _[:-2]
  let type = directories[-1]

  if extensions[-1] ==# 'vim'
  \  && type =~# '^\v(autoload|colors|compiler|ftplugin|indent|plugin|syntax)'
    let after_p = directories[-2] == 'after'
    execute 'SkeletonLoad' (after_p ? 'vim-additional-' : 'vim-') . type
  elseif extensions[-2:-1] ==# ['user', 'js']
    SkeletonLoad userjs
  endif
endfunction

autocmd MyAutoCmd User plugin-skeleton-loaded
\ call s:on_User_plugin_skeleton_loaded()

function! s:on_User_plugin_skeleton_loaded()
  silent %s/<%=\(.\{-}\)%>/\=eval(submatch(1))/ge
  if search('<%|%>', 'w')
    if foldclosed(line('.'))
      normal! zv
    endif
    normal! "_da>
  else
    normal! gg
  endif
endfunction




" smartword  "{{{2

map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)




" submode  "{{{2

call submode#enter_with('scroll', 'nv', '', '[Space]j')
call submode#map('scroll', 'nv', 'e', 'j',
\                'line(".") != line("$") ? "<C-d>" : ""')
call submode#map('scroll', 'nv', 'e', 'k',
\                'line(".") != 1 ? "<C-u>" : ""')


call submode#enter_with('undo/redo', 'n', '', 'g-', 'g-')
call submode#enter_with('undo/redo', 'n', '', 'g+', 'g+')
call submode#map('undo/redo', 'n', '', '-', 'g-')
call submode#map('undo/redo', 'n', '', '+', 'g+')


call submode#enter_with('winsize', 'n', '', '[Space]w',
\                       ':<C-u>call '.s:SID_PREFIX().'submode_winsize()<CR>')
function! s:submode_winsize()
  let current = winnr()
  wincmd k | let above = winnr() | execute current "wincmd w"
  wincmd j | let below = winnr() | execute current "wincmd w"
  wincmd h | let left = winnr() | execute current "wincmd w"
  wincmd l | let right = winnr() | execute current "wincmd w"

  execute printf('call submode#map("winsize", "n", "r", "j", "<C-w>%s")',
  \ above != below && current == below ? "-" : "+")
  execute printf('call submode#map("winsize", "n", "r", "k", "<C-w>%s")',
  \ above != below && current == below ? "+" : "-")
  execute printf('call submode#map("winsize", "n", "r", "h", "<C-w>%s")',
  \ left != right && current == right ? ">" : "<")
  execute printf('call submode#map("winsize", "n", "r", "l", "<C-w>%s")',
  \ left != right && current == right ? "<" : ">")
endfunction


let g:submode_timeout = 0




" surround  "{{{2

nmap S  <Plug>Ysurround$
nmap s  <Plug>Ysurround
nmap ss  <Plug>Yssurround




" tohtml  "{{{2

let g:html_ignore_folding = 1
let g:html_no_pre = 0
let g:html_number_lines = 0
let g:html_use_css = 1
let g:use_xhtml = 1




" vcsi  "{{{2

" fallback
nnoremap <Leader>v  <Nop>


let g:vcsi_diff_in_commit_buffer_p = 1
let g:vcsi_open_command = 'Split | enew'
let g:vcsi_use_native_message_p = 1




" Fin.  "{{{1

if has('vim_starting')
  if filereadable(expand('~/.vimrc.local'))
    source ~/.vimrc.local
  endif
endif


" must be written at the last.  see :help 'secure'.
set secure




" __END__  "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker