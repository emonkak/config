" My vimrc
" Basic  "{{{1
" Absolute  "{{{2

let s:win_p = has('win32') || has('win64')


if has('vim_starting')
  set shiftwidth=4
  set tabstop=4
  if s:win_p
    set fileformat=unix
    set runtimepath=~/.vim,$VIMRUNTIME,~/.vim/after
  endif
endif


function! s:SID_PREFIX()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfunction




" Encodng  "{{{2

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


if s:win_p
  language C
  let $LANG = 'ja_JP.CP932'
  let $LC_MESSAGE = 'C'
  let $LC_TIME = 'C'
  set termencoding=cp932
endif




" Options  "{{{2

if has('gui_running')
  set guicursor=a:blinkon0
  if s:win_p
    set guifont=Consolas:h10.5
    set linespace=0
  else
    set guifont=Monospace\ 10
    set linespace=-2
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


set ambiwidth=double
set backspace=indent,eol,start
set nobackup
if has('clientserver')
  set clipboard=autoselectml,exclude:cons\|linux
endif
set diffopt=filler,vertical
set directory=~/tmp,/tmp
set fileformats=unix,dos,mac
set hidden
set history=100
if has('multi_byte_ime') || has('xim')
  set iminsert=0
  set imsearch=0
endif
set keywordprg=:help
set nowritebackup
if exists('+shellslash')
  set shellslash
endif
set spelllang=en_us
set virtualedit=block

set cmdheight=1
set display=lastline
set noequalalways
set foldmethod=marker
set nohlsearch
set laststatus=2
set linebreak
set list
let &listchars = "tab:\u00bb ,extends:<,trail:-"
set pumheight=20
set report=0
set ruler
set showcmd
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
let &statusline .= '%{&mod ? &ro ? "%*" : "**" : &ro ? "%%" : "--"}'
let &statusline .= ' %<%f %h%w'
let &statusline .= '%{&fenc == "" || &fenc == &enc ? "" : "[" . &fenc . "]"}'
let &statusline .= '%{&ff[0] == &ffs[0] ? "" : "[" . &ff . "]"}'
let &statusline .= '    '
let &statusline .= '%P (%l,%v)'
let &statusline .= '    '
let &statusline .= '%-4(%{eskk#statusline("[%s]")}%)'

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
call pathogen#runtime_append_all_bundles()




" Syntax  {{{1
" Stuffs  "{{{2

let s:FALSE = 0
let s:TRUE = !s:FALSE




" CD - wrapper of :cd to keep cwd for each tabpage  "{{{2

command! -complete=dir -nargs=* CD
\   if <q-args> == ''
\ |   execute 'cd' (expand('%') != '' ? '%:h' : '~')
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

autocmd MyAutoCmd BufReadPost *
\   if !exists('t:cwd') && buflisted(bufnr('%'))
\ |   cd %:p:h
\ |   let t:cwd = getcwd()
\ | endif




" Capture - the output of commands are written buffer.  "{{{2

command! -complete=command -nargs=+ Capture  call s:cmd_Capture(<q-args>)
function! s:cmd_Capture(args)
  redir => output
  silent execute a:args
  redir END
  let output = substitute(output, '^\n\+', '', '')

  Split
  edit `=printf('[capture:%s]', a:args)`
  nnoremap <buffer> q  <C-w>c
  setlocal buftype=nofile bufhidden=unload noswapfile nobuflisted
  call setline(1, split(output, '\n'))
endfunction




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




" HelpTagsAll - :helptags to all doc/ in &runtimepath  "{{{2

command! -bar -nargs=0 HelpTagsAll  call s:cmd_HelpTagsAll()
function! s:cmd_HelpTagsAll()
  for path in split(&runtimepath, ',')
    let doc = path . '/doc'
    if stridx(doc, $VIM) != 0 && isdirectory(doc) && filewritable(doc)
      helptags `=doc`
    endif
  endfor
endfunction




" Incrementor - sequence number substitutions.  "{{{2
"
" :Incrementor {pattern} [format] [first] [step]
"
"   Example: Print the line number in front of each line:
"   Incrementor ^ '%03d ' 1

command! -nargs=+ -range Incrementor
\ <line1>,<line2>call s:cmd_Incrementor(<q-args>)
function! s:cmd_Incrementor(args) range
  let args = vimproc#parser#split_args(a:args)
  let incrementor = call(s:SID_PREFIX() . 'incrementor', args[1:])
  execute printf('%d,%ds/%s/\=incrementor.call()/g',
  \              a:firstline,
  \              a:lastline,
  \              escape(args[0], '/'))
endfunction




" Rename - rename the current editing file.  "{{{2

command! -nargs=1 -complete=file Rename  call s:cmd_Rename(<q-args>)
function! s:cmd_Rename(name)
  let current = expand('%')
  if &readonly || !&modifiable || !filewritable(current)
    Hecho ErrorMsg 'This file cannot be changes.'
  elseif filereadable(a:name)
    Hecho ErrorMsg 'Renamed file already exists.'
  else
    file `=a:name`
    call delete(current)
    write
    redraw
    echo 'Renamed:' current '->' a:name
  endif
endfunction




" Scouter - measures battle power of a vimmer.  "{{{2

command! -bar -nargs=? -complete=file Scouter
\ echo s:cmd_Scouter(<q-args> == '' ? $MYVIMRC : expand(<q-args>))
function! s:cmd_Scouter(file)
  let pattern = '^\s*$\|^\s*"'
  let lines = readfile(a:file)
  let lines = split(substitute(join(lines, "\n"), '\n\s*\\', '', 'g'), "\n")
  return len(filter(lines,'v:val !~ pattern'))
endfunction




" SetFileType, SetFileEncoding, SetFileFormat - with completion.  "{{{2

command! -nargs=? -complete=filetype SetFileType
\ setlocal filetype=<args> | silent SkeletonLoad <args>
command! -nargs=? -complete=customlist,s:cmd_SetFileEncoding_complete SetFileEncoding
\ setlocal fileencoding=<args>
command! -nargs=? -complete=customlist,s:cmd_SetFileFormats_complete SetFileFormat
\ setlocal fileformat=<args>

function! s:cmd_SetFileEncoding_complete(arglead, cmdline, cursorpos)
  " {encoding: 0} see :help encoding-values.  "{{{
  let encodings = {
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
    let encodings[encoding] = 0
  endfor
  return sort(filter(keys(encodings), 'stridx(v:val, a:arglead) == 0'))
endfunction

function! s:cmd_SetFileFormats_complete(arglead, cmdline, cursorpos)
  return sort(filter(split(&fileformats, ','), 'stridx(v:val, a:arglead) == 0'))
endfunction

AlterCommand setf[iletype]  SetFileType




" Source - wrapper of :source with echo.  "{{{2

command! -bar -complete=file -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>

AlterCommand so[urce]  Source




" SuspendWithAutomticCD  "{{{2

command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCd()
function! s:cmd_SuspendWithAutomticCd()
  let available_p = s:FALSE
  let shell = split(&shell, '/')[-1]

  if exists('$TMUX') || (has('gui_running') && executable('tmux'))
    let windows = s:tmux_list_windows()
    if !empty(windows)
      let i = -1
      for [index, title] in windows
        if title ==# shell
          let i = index
          break
        endif
      endfor
      silent execute '!tmux'
      \              (i > -1 ? 'select-window -t '.i : 'new-window').'\;'
      \              'send-keys C-u " cd \"'.getcwd().'\"" C-m'
      redraw!
      let available_p = (v:shell_error == 0)
    endif
  elseif exists('$WINDOW') || (has('gui_running') && executable('screen'))
    silent execute '!screen -X eval'
    \              '''select '.shell.''''
    \              '''stuff "\025 cd \\"'.getcwd().'\\" \015"'''
    redraw!
    let available_p = (v:shell_error == 0)
  endif

  if !available_p
    suspend
  endif
endfunction

function! s:tmux_list_windows()
  let _ = vimproc#system('tmux list-windows')
  if vimproc#get_last_status() != 0
    return []
  endif
  return map(split(_, '\n'),
  \         'split(matchstr(v:val, "^\\d\\+:\\s\\S\\+"), ":\\s")')
endfunction




" TabpageTitle - name the current tabpage  "{{{2

command! -bar -nargs=* TabpageTitle
\   if <q-args> == ''
\ |   let t:title = input("Set tabpage's title to: ", '')
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
command! -bang -bar -complete=file -nargs=? Utf16
\ edit<bang> ++enc=utf-16le <args>
command! -bang -bar -complete=file -nargs=? Utf16be
\ edit<bang> ++enc=utf-16be <args>

command! -bang -bar -complete=file -nargs=? Jis  Iso2022jp<bang> <args>
command! -bang -bar -complete=file -nargs=? Sjis  Cp932<bang> <args>
command! -bang -bar -complete=file -nargs=? Unicode  Utf16<bang> <args>




" Utilities  "{{{1
" :grep wrappers  "{{{2

command! -bar -complete=file -nargs=+ Grep  call s:grep('grep', <f-args>)
command! -bar -complete=file -nargs=+ Lgrep  call s:grep('lgrep', <f-args>)
function! s:grep(command, ...)
  let target = a:0 > 1 ? join(a:000[:-2]) : '**/*'

  if &grepprg ==# 'internal'
    try
      execute a:command '/'.escape(a:000[-1], '/\ ').'/j' target
    catch /.*/
      redraw
      Hecho ErrorMsg v:exception
    endtry
  else
    if s:win_p
      set shell=sh.exe shellcmdflag=-c shellxquote=\""
      execute a:command.'!' shellescape(a:000[-1]) target
      set shell& shellcmdflag& shellxquote&
    else
      execute a:command.'!' shellescape(a:000[-1]) target
    endif
  endif

  if a:command ==# 'grep'
    if !empty(getqflist())
      copen
    endif
  elseif !empty(getloclist())  " lgrep
    lopen
  endif
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  Lgrep




" :make wrappers  "{{{2

command! -bar -complete=file -nargs=* Make  call s:make('make', <f-args>)
command! -bar -complete=file -nargs=* Lmake  call s:make('lmake', <f-args>)
function! s:make(command, ...)
  execute a:command.'!' join(a:000)
  if a:command ==# 'make'
    if !empty(filter(getqflist(), 'v:val.valid != 0'))
      copen
    endif
  elseif !empty(filter(getloclist(), 'v:val.valid != 0'))  " lmake
    lopen
  endif
endfunction

AlterCommand mak[e]  Make
AlterCommand lmak[e]  Lmake




" Font selector  "{{{2

if has('gui_running')
  command! -complete=customlist,s:cmd_Font_complete -nargs=* Font
  \ let &guifont = <q-args>
endif

function! s:cmd_Font_complete(arglead, cmdline, cursorpos)
  if !has('unix')
    return []  " Not available.
  endif
  let _ = [
  \   'Droid Sans Mono 10.5',
  \   'M+ 1m 10.5',
  \   'TheSans Mono 10',
  \ ]
  return filter(_, 'stridx(v:val, a:arglead) == 0')
endfunction




" Help-related stuffs  "{{{2

function! s:helpbufwinnr()
  let wn = 1
  while wn <= winnr('$')
    let bn = winbufnr(wn)
    if getbufvar(bn, '&buftype') == 'help'
      return [bn, wn]
    endif
    let wn = wn + 1
  endwhile
  return [-1, 0]
endfunction

function! s:close_help_window()
  let [help_bufnr, help_winnr] = s:helpbufwinnr()
  if help_bufnr == -1
    return
  endif

  let current_winnr = winnr()
  execute help_winnr 'wincmd w'
  execute 'wincmd c'
  if current_winnr < help_winnr
    execute current_winnr 'wincmd w'
  elseif help_winnr < current_winnr
    execute (current_winnr-1) 'wincmd w'
  else
    " NOP
  endif
endfunction




" Indent styles  {{{2

let s:INDENT_STYLES = {
\  '2space': 'setlocal expandtab tabstop& shiftwidth=2 softtabstop=2',
\  '4space': 'setlocal expandtab tabstop& shiftwidth=4 softtabstop=4',
\  '4tab': 'setlocal noexpandtab tabstop=4 shiftwidth=4 softtabstop&',
\  '8tab': 'setlocal noexpandtab tabstop=8 shiftwidth=8 softtabstop&',
\ }

command! -bar -complete=customlist,s:cmd_SetIndent_complete -nargs=1 SetIndent
\ execute get(s:INDENT_STYLES, <q-args>, '')

function! s:cmd_SetIndent_complete(arglead, cmdline, cursorpos)
  return sort(filter(keys(s:INDENT_STYLES), 'stridx(v:val, a:arglead) == 0'))
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




" Split-related stuffs  "{{{2

command! -bar -nargs=* -complete=file Split
\ call s:vertical_with('split', <f-args>)
command! -bar -nargs=* -complete=file SplitTop
\ call s:vertical_with('topleft', 'split', <f-args>)
command! -bar -nargs=* -complete=file SplitBottom
\ call s:vertical_with('botright', 'split', <f-args>)
command! -bar -nargs=* -complete=help Help
\ call s:vertical_with('help', <f-args>)
command! -bar -nargs=* -complete=file New
\ call s:vertical_with('new', <f-args>)

function! s:vertically()
  return winwidth(0) * 2 > winheight(0) * 8
endfunction

function! s:vertical_with(command, ...)
  execute s:vertically() ? 'vertical' : ''
  \       a:command
  \       join(a:000)
endfunction

AlterCommand sp[lit]  Split
AlterCommand h[elp]  Help
AlterCommand new  New




" Toggle options  "{{{2

function! s:toggle_colorcolumn()
  if !exists('b:textwidth')
    let b:textwidth = &l:textwidth
    if &l:textwidth == 0
      set textwidth=80
    endif
    setlocal colorcolumn=+1 colorcolumn?
  else
    let &l:textwidth = b:textwidth
    unlet b:textwidth
    setlocal colorcolumn& colorcolumn?
  endif
endfunction

function! s:toggle_grepprg(global_p)
  let VALUES = ['grep -nHE', 'git grep -n']
  let grepprg = &l:grepprg == '' ? &grepprg : &l:grepprg
  let i = (index(VALUES, grepprg) + 1) % len(VALUES)

  if a:global_p
    let &grepprg = VALUES[i]
  else
    let &l:grepprg = VALUES[i]
  endif
  set grepprg?
endfunction
if has('vim_starting')
  silent call s:toggle_grepprg(s:TRUE)
endif

function! s:toggle_option(option_name)
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
endfunction




" URI-related stuffs  "{{{2

function! s:uri_regexp(scheme_pattern)
  let scheme = '\(' . a:scheme_pattern . '\m\)'
  return scheme . '://\([0-9A-Za-z!#$&''()*+,-./:;=?@_~]\|%\d\{2}\)\+'
endfunction

function! s:uri_extract(str, scheme_pattern)
  let uris = []
  let pattern = s:uri_regexp(a:scheme_pattern)

  let begin = match(a:str, pattern)
  while begin > -1
    let end = matchend(a:str, pattern, begin)
    call add(uris, strpart(a:str, begin, end))
    let begin = match(a:str, pattern, end)
  endwhile

  return uris
endfunction

function! s:uri_open(uri)
  if s:win_p
    call vimproc#open(a:uri)
  else
    if exists('$BROWSER')
      let browser = $BROWSER
    elseif executable('google-chrome')
      let browser = 'google-chrome'
    elseif executable('opera')
      let browser = 'opera'
    else
      echoerr 'Browser not found.'
    endif
    call vimproc#system_bg([browser, a:uri])
  endif
endfunction




function! s:get_region(expr1, expr2)  "{{{2
  let [_, line1, col1, _] = getpos(a:expr1)
  let [_, line2, col2, _] = getpos(a:expr2)

  let region = getline(line1, line2)
  let visualmode = visualmode()

  if visualmode == "\<C-v>"
    call map(region, 's:strpart(v:val, col1 - 1, col2 - (col1 - 1))')
  else
    if line1 == line2  " single line
      let region[0] = s:strpart(region[-1], col1 - 1, col2 - (col1 - 1))
    else  " multi line
      let region[0] = s:strpart(region[0], col1 - 1)
      let region[-1] = s:strpart(region[-1], 0, col2)
    endif
    if visualmode ==# 'V'
      let region += ['']
    endif
  endif

  return region  " return [] of string.
endfunction




function! s:incrementor(...)  " {{{2
  let incrementor = {}
  let incrementor.format = get(a:000, 0, '%d')
  let incrementor.value = get(a:000, 1, 0)
  let incrementor.step = get(a:000, 2, 1)

  function incrementor.call() dict
    let val = printf(self.format, self.value)
    let self.value += self.step
    return val
  endfunction

  return incrementor
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
    let target_tabpagenr -= 1
    tabclose
  endif

  execute target_tabpagenr 'tabnext'
endfunction




function! s:strpart(src, start, ...)  "{{{2
  let s = strpart(a:src, a:start)
  if a:0 > 0
    let i = byteidx(strpart(s, a:1 - 1), 1) - 1
    return i == -1 ? s : strpart(s, 0, a:1 + i)
  else
    return s
  endif
endfunction




" Mappings  "{{{1
" Terminal-GUI interoperability  "{{{2

if has('gui_running')
  " NUL
  map <C-Space>  <C-@>
  map! <C-Space>  <C-@>

  noremap! <S-Insert>  <C-r>*
endif




" Tag jumping  "{{{2
" Fallback  "{{{3

" the prefix key.
nmap t  [Tag]
vmap t  [Tag]

" fallback
noremap [Tag]  <Nop>


" Basic  "{{{3

nnoremap [Tag]t  <C-]>
vnoremap [Tag]t  <C-]>
nnoremap <silent> [Tag]j  :tag<CR>
nnoremap <silent> [Tag]k  :pop<CR>
nnoremap <silent> [Tag]l  :<C-u>tags<CR>
nnoremap <silent> [Tag]n  :tnext<CR>
nnoremap <silent> [Tag]p  :tprevious<CR>
nnoremap <silent> [Tag]P  :<C-u>tfirst<CR>
nnoremap <silent> [Tag]N  :<C-u>tlast<CR>

" addition, interactive use.
nnoremap [Tag]<Space>  :<C-u>tag<Space>


" With the preview window  "{{{3

nnoremap [Tag]'t  <C-w>}
vnoremap [Tag]'t  <C-w>}
nnoremap <silent> [Tag]'n  :ptnext<CR>
nnoremap <silent> [Tag]'p  :ptprevious<CR>
nnoremap <silent> [Tag]'P  :<C-u>ptfirst<CR>
nnoremap <silent> [Tag]'N  :<C-u>ptlast<CR>
nnoremap <silent> [Tag]'c  :<C-u>pclose<CR>


" With :split  "{{{3

nnoremap <silent> tst  :<c-u>call <SID>vertical_with('wincmd', ']')<CR>
vnoremap <silent> tst  :<c-u>call <SID>vertical_with('wincmd', ']')<CR>




" QuickFix  "{{{2
" Fallback  "{{{3

" The prefix key.
nmap q  [Quickfix]

" fallback
noremap [Quickfix]  <Nop>

" Alternative key for the original action.
nnoremap Q  q


" For quickfix list  "{{{3

nnoremap <silent> [Quickfix]j  :cnext<CR>
nnoremap <silent> [Quickfix]k  :cprevious<CR>
nnoremap <silent> [Quickfix]r  :<C-u>crewind<CR>
nnoremap <silent> [Quickfix]K  :<C-u>cfirst<CR>
nnoremap <silent> [Quickfix]J  :<C-u>clast<CR>
nnoremap <silent> [Quickfix]fj  :<C-u>cnfile<CR>
nnoremap <silent> [Quickfix]fk  :<C-u>cpfile<CR>
nnoremap <silent> [Quickfix]l  :<C-u>clist<CR>
nnoremap <silent> [Quickfix]q  :<C-u>cc<CR>
nnoremap <silent> [Quickfix]o  :<C-u>copen<CR>
nnoremap <silent> [Quickfix]c  :<C-u>cclose<CR>
nnoremap <silent> [Quickfix]p  :<C-u>colder<CR>
nnoremap <silent> [Quickfix]n  :<C-u>cnewer<CR>
nnoremap <silent> [Quickfix]m  :<C-u>Make<CR>
nnoremap [Quickfix]M  :<C-u>Make<Space>
nnoremap [Quickfix]<Space>  :<C-u>Make<Space>
nnoremap [Quickfix]g  :<C-u>Grep<Space>


" For location list (mnemonic: Quickfix list for the current Window)  "{{{3

nnoremap <silent> [Quickfix]wj  :lnext<CR>
nnoremap <silent> [Quickfix]wk  :lprevious<CR>
nnoremap <silent> [Quickfix]wr  :<C-u>lrewind<CR>
nnoremap <silent> [Quickfix]wK  :<C-u>lfirst<CR>
nnoremap <silent> [Quickfix]wJ  :<C-u>llast<CR>
nnoremap <silent> [Quickfix]wfj  :<C-u>lnfile<CR>
nnoremap <silent> [Quickfix]wfk  :<C-u>lpfile<CR>
nnoremap <silent> [Quickfix]wl  :<C-u>llist<CR>
nnoremap <silent> [Quickfix]wq  :<C-u>ll<CR>
nnoremap <silent> [Quickfix]wo  :<C-u>lopen<CR>
nnoremap <silent> [Quickfix]wc  :<C-u>close<CR>
nnoremap <silent> [Quickfix]wp  :<C-u>lolder<CR>
nnoremap <silent> [Quickfix]wn  :<C-u>lnewer<CR>
nnoremap <silent> [Quickfix]wm  :<C-u>Lmake<CR>
nnoremap [Quickfix]wM  :<C-u>Lmake<Space>
nnoremap [Quickfix]w<Space>  :<C-u>Lmake<Space>
nnoremap [Quickfix]wg  :<C-u>Lgrep<Space>


" Tab pages  "{{{2
" Fallback  "{{{3

" the prefix key.
nmap <C-t>  [Tabbed]

" fallback
noremap [Tabbed]  <Nop>


" Basic  "{{{3

" Move new tabpage at the last.
nnoremap <silent> [Tabbed]n  :<C-u>tabnew \| :tabmove<CR>
nnoremap <silent> [Tabbed]c  :<C-u>tabclose<CR>
nnoremap <silent> [Tabbed]o  :<C-u>tabonly<CR>
nnoremap <silent> [Tabbed]i  :<C-u>tabs<CR>

nmap [Tabbed]<C-n>  <C-t>n
nmap [Tabbed]<C-c>  <C-t>c
nmap [Tabbed]<C-o>  <C-t>o
nmap [Tabbed]<C-i>  <C-t>i

nnoremap <silent> [Tabbed]<Space>  :<C-u>TabpageTitle<CR>


" Moving around tabpages.  "{{{3

nnoremap <silent> [Tabbed]j
\ :<C-u>execute 'tabnext' 1 + (tabpagenr() + v:count1 - 1) % tabpagenr('$')<CR>
nnoremap <silent> [Tabbed]k
\ :<C-u>execute 'tabprevious' v:count1 % tabpagenr('$')<CR>
nnoremap <silent> [Tabbed]K  :<C-u>tabfirst<CR>
nnoremap <silent> [Tabbed]J  :<C-u>tablast<CR>

nmap [Tabbed]<C-j>  <C-t>j
nmap [Tabbed]<C-k>  <C-t>k
nmap [Tabbed]<C-t>  <C-t>j

" GNU screen like mappings.
" Note that the numbers in {lhs}s are 0-origin.  See also 'tabline'.
for i in range(10)
  execute 'nnoremap <silent>' ('[Tabbed]'.(i))  ((i+1).'gt')
endfor
unlet i


" Moving tabpages themselves.  "{{{3

nnoremap <silent> [Tabbed]l
\ :<C-u>execute 'tabmove' min([tabpagenr() + v:count1 - 1, tabpagenr('$')])<CR>
nnoremap <silent> [Tabbed]h
\ :<C-u>execute 'tabmove' max([tabpagenr() - v:count1 - 1, 0])<CR>
nnoremap <silent> [Tabbed]L  :<C-u>tabmove<CR>
nnoremap <silent> [Tabbed]H  :<C-u>tabmove 0<CR>

nmap [Tabbed]<C-l>  <C-t>l
nmap [Tabbed]<C-h>  <C-t>h


" Argument list  "{{{2

" the prefix key.
nmap <C-g>  [Argument]

" fallback
noremap [Argument] <Nop>


nnoremap [Argument]<Space>  :<C-u>args<Space>
nnoremap <silent> [Argument]l  :args<CR>
nnoremap <silent> [Argument]j  :next<CR>
nnoremap <silent> [Argument]k  :previous<CR>
nnoremap <silent> [Argument]J  :last<CR>
nnoremap <silent> [Argument]K  :first<CR>
nnoremap <silent> [Argument]wj  :wnext<CR>
nnoremap <silent> [Argument]wk  :wprevious<CR>

nmap [Argument]<C-l>  <C-g>l
nmap [Argument]<C-j>  <C-g>j
nmap [Argument]<C-k>  <C-g>k
nmap [Argument]<C-w><C-j>  <C-g>wj
nmap [Argument]<C-w><C-k>  <C-g>wk




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

" Emacs like kill-line.
cnoremap <C-k>
\ <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>


" escape Command-line mode if the command line is empty (like <C-h>)
cnoremap <expr> <C-u>  getcmdline() == '' ? "\<Esc>" : "\<C-u>"
cnoremap <expr> <C-w>  getcmdline() == '' ? "\<Esc>" : "\<C-w>"

" Search slashes easily (too lazy to prefix backslashes to slashes)
cnoremap <expr> /  getcmdtype() == '/' ? '\/' : '/'




" Command-line window  "{{{2

nnoremap ;  q:
xnoremap ;  q:

autocmd MyAutoCmd CmdwinEnter *
\ call s:on_CmdwinEnter()

function! s:on_CmdwinEnter()
  nnoremap <buffer> <Esc><Esc>  <C-c><C-c>
  inoremap <buffer> <Esc><Esc>  <C-c><C-c>
  inoremap <buffer> <expr> <BS>
  \ getline('.') == '' ? "\<C-c>\<C-c>" : "\<BS>"
  inoremap <buffer> <expr> <C-h>
  \ getline('.') == '' ? "\<C-c>\<C-c>" : "\<C-h>"
  inoremap <buffer> <expr> <Tab>
  \ pumvisible() ? "\<C-n>" : <SID>keys_to_complete()
  inoremap <buffer> <expr> <C-c>
  \ pumvisible() ? "\<Esc>" : "\<C-c>"

  startinsert!
endfunction




" Insert mode  "{{{2

inoremap <C-b>  <Left>
inoremap <C-f>  <Right>

" Go to head, or tail.
inoremap <expr> <C-a>  col('.') <= match(getline('.'), '\S\zs')
                     \ ? "\<Home>" : "\<C-o>^"
inoremap <expr> <C-e>  pumvisible()
                     \ ? "\<C-e>"
                     \ : col('.') >= match(getline('.'), '\S\zs')
                     \ ? "\<End>" : "\<C-o>^"

inoremap <C-d>  <Delete>

inoremap <expr> <C-y>  pumvisible() ? "\<C-y>" : "\<C-r>\""

" Emacs like kill-line.
" BUGS: Can't use <C-x><X-k> (keyword completion).
inoremap <expr> <C-k>  col('.') == col('$') ? "\<C-o>gJ" : "\<C-o>D"

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
nnoremap [Space]fi  :<C-u>SetIndent<Space>
nnoremap [Space]ft  :<C-u>SetFileType<Space>

nnoremap <silent> [Space]fs
\ :<C-u>setlocal filetype? fileencoding? fileformat?<CR>

nnoremap [Space]o  <Nop>
nnoremap <silent> [Space]oc  :<C-u>call <SID>toggle_colorcolumn()<CR>
nnoremap <silent> [Space]og  :<C-u>call <SID>toggle_grepprg(0)<CR>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]op  :<C-u>call <SID>toggle_option('paste')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>

nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>

nnoremap <silent> [Space]?  :<C-u>call <SID>close_help_window()<CR>
nnoremap <silent> [Space]q  :<C-u>Help quickref<CR>

nnoremap <silent> [Space]m  :<C-u>marks<CR>
nnoremap <silent> [Space]r  :<C-u>registers<CR>

nnoremap <silent> [Space]d  :<C-u>bdelete<CR>
nnoremap <silent> [Space]D  :<C-u>bdelete!<CR>

nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>


" Open a fold.
nnoremap [Space]l  zo

" Close a fold.
nnoremap [Space]h  zc

" Close all folds but including the cursor.
nnoremap [Space]v  zMzv


" Yank to clipboard.
nnoremap [Space]y  "+y
vnoremap [Space]y  "+y
nnoremap [Space]Y  "+y$
vnoremap [Space]Y  "+y$

" Put in clipboard.
nnoremap [Space]p  "+p
vnoremap [Space]p  "+p
nnoremap [Space]P  "+P
vnoremap [Space]P  "+P




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

nmap <C-w><C-t>  <C-w>t

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


" Like "<C-w>q", but does ":quit!".
nnoremap <C-w>Q  :<C-u>quit!<CR>


nnoremap <silent> <C-w>y  :<C-u>Split<CR>




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
onoremap gv  :<C-u>normal! gv<CR>




" Operators  "{{{2

call operator#user#define('my-sort', s:SID_PREFIX() . 'operator_sort')
function! s:operator_sort(motion_wiseness)
  if a:motion_wiseness == 'char'
    let reg_u = [@", getregtype('"')]

    let @" = join(map(s:get_region("'[", "']"), 'join(sort(split(v:val)))'),
    \             "\n")
    normal! `[v`]p

    call setreg('"', reg_u[0], reg_u[1])
  else  " line or block
    '[,']sort
  endif
endfunction

nmap [Space]s  <Plug>(operator-my-sort)
vmap [Space]s  <Plug>(operator-my-sort)


call operator#user#define('my-open-uri', s:SID_PREFIX() . 'operator_open_uri')
function! s:operator_open_uri(motion_wiseness)
  if a:motion_wiseness == 'line'
    let region = getline("'[", "']")
  else  " char or block
    let region = s:get_region("'[", "']")
  endif

  for line in region
    for uri in s:uri_extract(line, 'https\?')
      call s:uri_open(uri)
    endfor
  endfor
endfunction

nmap go  <Plug>(operator-my-open-uri)iW
vmap go  <Plug>(operator-my-open-uri)


call operator#user#define('caw-wrap-comment',
\                         s:SID_PREFIX() . 'operator_caw_wrap_comment')
call operator#user#define('caw-wrap-uncomment',
\                         s:SID_PREFIX() . 'operator_caw_wrap_uncomment')

function! s:operator_caw_wrap_comment(motion_wiseness)
  let v = operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal! `[' . v . "`]\<Esc>"
  call caw#do_wrap_comment('v')
endfunction

function! s:operator_caw_wrap_uncomment(motion_wiseness)
  let v = operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal! `[' . v . "`]\<Esc>"
  call caw#do_uncomment_wrap('v')
endfunction




" Misc.  "{{{2

if has('unix') && executable('sudo')
  nnoremap <silent> <Leader>#  :<C-u>w sudo:%<CR>
endif
nnoremap <silent> <Leader><Leader>  :<C-u>update<CR>

nnoremap <C-h>  :<C-u>Help<Space>
nnoremap <C-o>  :<C-u>edit<Space>


" Expand with 'l' if the cursor on the holded text.
nnoremap <expr> l  foldclosed(line('.')) != -1 ? 'zo' : 'l'


" Move cursor by display lines when wrapping.
noremap j  gj
noremap k  gk
noremap gj  j
noremap gk  k


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
nnoremap <expr> o  <SID>start_insert_mode_with_blank_lines('o')
nnoremap <expr> O  <SID>start_insert_mode_with_blank_lines('O')

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


" Search for the selected text.
vnoremap <silent> *  :<C-u>call <SID>search_the_selected_text_literaly('n')<CR>
vnoremap <silent> #  :<C-u>call <SID>search_the_selected_text_literaly('N')<CR>

function! s:search_the_selected_text_literaly(search_command)
  let region = join(map(s:get_region("'<", "'>"), 'escape(v:val, "\\/")'),
  \                 '\n')

  let @/ = '\V' . region
  call histadd('/', '\V' . region)
  execute 'normal!' a:search_command

  let v:searchforward = a:search_command ==# 'n'
endfunction


noremap <C-z>  <Nop>
nnoremap <C-z>  :<C-u>SuspendWithAutomticCD<CR>


" Show the lines which match to the last search pattern.
nnoremap g/  :global//print<CR>
vnoremap g/  :global//print<CR>


" Echo the name of the syntax item under the cursor.
nnoremap <silent> gs  :<C-u>call <SID>show_syntax_name_under_the_cursor()<CR>
function! s:show_syntax_name_under_the_cursor()
  let names = []
  for id in synstack(line('.'), col('.'))
    call add(names, synIDattr(id, 'name'))
  endfor
  echo join(names, ', ')
endfunction

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




" Filetypes  "{{{1
" All filetypes   "{{{2

autocmd MyAutoCmd FileType *
\ call s:on_FileType_any()

function! s:on_FileType_any()
  " Add 'dictionary' for filetype.
  let dict = expand('~/.vim/dict/') . &l:filetype . '.dict'
  if filereadable(dict)
    execute 'setlocal dictionary+=' . dict
  endif

  " Make omni completion available for all filetypes.
  if &l:omnifunc == ''
    setlocal omnifunc=syntaxcomplete#Complete
  endif

  " Disable Auto-wrap.
  setlocal formatoptions-=t formatoptions-=c
endfunction


" Fix 'fileencoding' to use 'encoding'.
autocmd MyAutoCmd BufReadPost *
\   if &modifiable && !search('[^\x00-\x7F]', 'cnw')
\ |   setlocal fileencoding=
\ | endif

" When editing a file, always jump to the last cursor position.
autocmd MyAutoCmd BufReadPost *
\   if line('''"') > 0 && line('''"') <= line('$')
\ |   execute 'normal! g''"'
\ | endif

" Unset 'paste' automatically.
autocmd MyAutoCmd InsertLeave *  set nopaste




" asciidoc  "{{{2

autocmd MyAutoCmd FileType asciidoc
\ call s:on_FileType_asciidoc()

function! s:on_FileType_asciidoc()
  " Folding oneline titles.
  setlocal foldmethod=expr
  let &l:foldexpr = s:SID_PREFIX() . 'asciidoc_fold(v:lnum)'
endfunction

function! s:asciidoc_fold(lnum)
  let current = matchend(getline(a:lnum), '^=\{1,5}\ze\s\+\S') - 1
  let next = matchend(getline(a:lnum + 1), '^=\{1,5}\ze\s\+\S') - 1
  return current > 0 ? current
  \    : next > 0 ? '<' . next : '='
endfunction




" changelog  "{{{2

" Fix the new entry mapping bug.
autocmd MyAutoCmd FileType changelog
\ noremap <buffer> <silent> <Leader>o  :<C-u>NewChangelogEntry<CR>

let g:changelog_timeformat = '%Y-%m-%d'
let g:changelog_username  = s:win_p ? $USERNAME : $USER




" css  "{{{2

autocmd MyAutoCmd FileType css
\ SetIndent 2space




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
\   SetIndent 2space
\ | compiler ghc




" java  "{{{2

autocmd MyAutoCmd FileType java
\   call s:on_FileType_java()
\ | compiler javac

function! s:on_FileType_java()
  setlocal cinoptions=:0,l1,g0,t0,(0,j1
  nnoremap <buffer> <silent> <LocalLeader>a  :<C-u>QuickRun javaapplet -mode n<CR>
  vnoremap <buffer> <silent> <LocalLeader>a  :<C-u>QuickRun javaapplet -mode v<CR>
endfunction

let g:java_highlight_all = 1
let g:java_highlight_functions = 1




" javascript  "{{{2

autocmd MyAutoCmd FileType javascript
\ SetIndent 2space




" lua  "{{{2

autocmd MyAutoCmd FileType lua
\ SetIndent 2space




" perl  "{{{2

autocmd MyAutoCmd FileType perl
\   SetIndent 2space
\ | setlocal include=




" python  "{{{2

autocmd MyAutoCmd FileType python
\ SetIndent 4space

let g:python_highlight_all = 1




" ruby  "{{{2

autocmd MyAutoCmd FileType ruby
\ SetIndent 2space




" scheme  "{{{2

let g:is_gauche = 1
let g:scheme_rainbow = 1




" sh, zsh  "{{{2

autocmd MyAutoCmd FileType sh,zsh
\ SetIndent 2space

let g:is_bash = 1




" tex  "{{{2

autocmd MyAutoCmd FileType tex,plaintex
\   call s:on_FileType_tex()
\ | compiler tex

function! s:on_FileType_tex()
  SetIndent 2space
  setlocal foldmarker=%{{{,%}}}
endfunction

let g:tex_flavor = 'latex'




" vim  "{{{2

autocmd MyAutoCmd FileType vim
\ SetIndent 2space

let g:vim_indent_cont = 0




" xml  "{{{2

autocmd MyAutoCmd FileType docbk,html,xhtml,xml,xslt
\ call s:on_FileType_xml()

function! s:on_FileType_xml()
  SetIndent 2space
  inoremap <buffer> </  </<C-x><C-o>
endfunction




" Plugins  "{{{1
" caw  "{{{2

" fallback
map [Space]c  <Nop>

map [Space]ci  <Plug>(caw:i:comment)
map [Space]cI  <Plug>(caw:I:comment)
map [Space]ca  <Plug>(caw:a:comment)
map [Space]cw  <Plug>(caw:wrap:comment)
map [Space]co  <Plug>(operator-caw-wrap-comment)

map [Space]cuu  <Plug>(caw:uncomment) 
map [Space]cui  <Plug>(caw:i:uncomment)
map [Space]cua  <Plug>(caw:a:uncomment)
map [Space]cuw  <Plug>(caw:w:uncomment)
map [Space]cuo  <Plug>(operator-caw-wrap-uncomment)


let g:caw_no_default_keymappings = 1
let g:caw_sp_a_left = '  '




" eskk  "{{{2

autocmd MyAutoCmd User eskk-initialize-pre
\ call s:on_User_eskk_initial_pre()

function! s:on_User_eskk_initial_pre()
  let t = eskk#table#new('rom_to_hira*', 'rom_to_hira')
  call t.add_map('~', '～')
  call t.add_map('(', '（')
  call t.add_map(')', '）')
  call eskk#register_mode_table('hira', t)
endfunction

let g:eskk#dictionary = {
\  'path': expand('~/.skk-eskk-jisyo'),
\  'sorted': 0,
\  'encoding': 'utf-8',
\ }
let g:eskk#large_dictionary = {
\  'path': expand('~/.skk/SKK-JISYO.mime'),
\  'sorted': 1,
\  'encoding': 'euc-jp',
\ }

let g:eskk#directory = expand('~/.vim/info/eskk')
let g:eskk#egg_like_newline = 1
let g:eskk#enable_completion = 0
let g:eskk#show_annotation = 0
let g:eskk#statusline_mode_strings = {
\  'hira': 'あ',
\  'kata': 'ア',
\  'ascii': 'A',
\  'zenei': '英',
\  'hankata': 'ｶﾅ',
\  'abbrev': "/"
\ }
let g:eskk#use_color_cursor = 0




" exjumplist  "{{{2

nmap <C-S-j>  <Plug>(exjumplist-next-buffer)
nmap <C-S-k>  <Plug>(exjumplist-previous-buffer)




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
  imap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
  nmap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
endfunction


call ku#custom_action('common', 'cd', s:SID_PREFIX().'ku_common_action_my_cd')
call ku#custom_action('common', 'Yank',
\                     s:SID_PREFIX().'ku_common_action_Yank')
call ku#custom_action('common', 'yank',
\                     s:SID_PREFIX().'ku_common_action_yank')
call ku#custom_action('metarw/git', 'checkout',
\                     s:SID_PREFIX().'ku_metarw_git_action_checkout')

function! s:ku_common_action_my_cd(item)
  if isdirectory(a:item.word)
    execute 'CD' a:item.word
  else  " treat a:item as a file name
    execute 'CD' fnamemodify(a:item.word, ':h')
  endif
endfunction

function! s:ku_common_action_yank(item)
  call setreg('"', a:item.word, 'c')
endfunction
function! s:ku_common_action_Yank(item)
  call setreg('"', a:item.word, 'l')
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
call ku#custom_key('metarw/git', '/', 'checkout')
call ku#custom_key('metarw/git', '?', 'checkout')


call ku#custom_prefix('common', '.VIM', expand('~/.vim'))
call ku#custom_prefix('common', 'HOME', expand('~'))
call ku#custom_prefix('common', 'VIM', expand('$VIMRUNTIME'))
call ku#custom_prefix('common', '~', expand('~'))


nmap [Space]k  <Nop>
nnoremap <silent> [Space]ka  :<C-u>Ku args<CR>
nnoremap <silent> [Space]kB  :<C-u>Ku buffer<CR>
nnoremap <silent> [Space]kb  :<C-u>Ku buffer/tab<CR>
nnoremap <silent> [Space]kf  :<C-u>Ku file<CR>
nnoremap <silent> [Space]kg  :<C-u>Ku metarw/git<CR>
nnoremap <silent> [Space]kh  :<C-u>Ku history<CR>
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
let g:ku_file_mru_file = expand('~/.vim/info/ku/')
\                      . (s:win_p ? 'mru_win' : 'mru')
let g:ku_file_mru_ignore_pattern = '\v/$|^//|^/cygdrive/|^/mnt/|^/media/|^/tmp/'
let g:ku_file_mru_limit = 200




" narrow  "{{{2

" fallback
noremap [Space]x  <Nop>

call operator#user#define_ex_command('narrow', 'Narrow')
map [Space]xn  <Plug>(operator-narrow)

noremap <silent> [Space]xw  :<C-u>Widen<CR>




" neocomplcache  "{{{2

imap <C-l>  <Plug>(neocomplcache_snippets_expand)
smap <C-l>  <Plug>(neocomplcache_snippets_expand)

inoremap <expr> <BS>  neocomplcache#smart_close_popup() . "\<C-h>"
inoremap <expr> <C-h>  neocomplcache#smart_close_popup() . "\<C-h>"


let g:neocomplcache_disable_auto_complete = 1
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_auto_select = 0
let g:neocomplcache_enable_smart_case = 1

let g:neocomplcache_auto_completion_start_length = 2
let g:neocomplcache_manual_completion_start_length = 2

let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*\|\[ku\]\|\[quickrun output\]'
let g:neocomplcache_temporary_dir = expand('~/.vim/info/neocomplcache')




" operator-replece  "{{{2

map _  <Plug>(operator-replace)




" quickrun  "{{{2

let g:loaded_quicklaunch = 1
let g:quickrun_config = {
\  '*': {
\    'split': '{'.s:SID_PREFIX().'vertically() ? "vertical" : ""}',
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
\  'tex': {
\    'command': executable('pxdvi') ? 'pxdvi' :
\               executable('xdvi') ? 'xdvi' : '',
\    'exec': ['platex -kanji=utf8 -interaction=nonstopmode -shell-escape -output-directory=%s:p:h %s',
\             '%c %s:p:r.dvi'],
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
  nmap <buffer> <silent> <C-]>  <Plug>(ref-keyword)
  nmap <buffer> <silent> <C-j>  <Plug>(ref-forward)
  nmap <buffer> <silent> <C-k>  <Plug>(ref-back)
  nnoremap <buffer> q  <C-w>c
endfunction

nmap <silent> K  <Plug>(ref-keyword)
vmap <silent> K  <Plug>(ref-keyword)

nnoremap <silent> <Leader>a  :<C-u>call ref#jump('normal', 'alc')<CR>
vnoremap <silent> <Leader>a  :<C-u>call ref#jump('visual', 'alc')<CR>


let g:ref_alc_start_linenumber = 36
let g:ref_cache_dir = expand('~/.vim/info/ref')
let g:ref_no_default_key_mappings = 1
let g:ref_open = 'Split'
let g:ref_perldoc_complete_head = 1

AlterCommand ref  Ref




" scratch  "{{{2

nmap <Leader>s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\ call s:on_User_plugin_scratch_initialize_after()

function! s:on_User_plugin_scratch_initialize_after()
  map <buffer> <CR>  <Plug>(scratch-evaluate!)
  map <buffer> q  <Plug>(scratch-close)
endfunction


let g:scratch_show_command = 'SplitTop | hide buffer'




" skeleton  "{{{2

autocmd MyAutoCmd User plugin-skeleton-detect
\ call s:on_User_plugin_skeleton_detect()

function! s:on_User_plugin_skeleton_detect()
  let _ = split(expand('%:p'), '/')
  let extension = matchstr(_[-1], '^[^.]*\.\zs[0-9A-Za-z.]\+')
  let directories = _[:-2]
  let type = directories[-1]

  if extension ==# 'vim'
  \  && type =~# '\v^(autoload|colors|compiler|ftplugin|indent|plugin|syntax)'
    let after_p = directories[-2] == 'after'

    if after_p
      execute 'SkeletonLoad' 'vim-additional-' . type
    else
      execute 'SkeletonLoad' 'vim-' . type
    endif
  elseif extension ==# 'user.js'
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

" The default mapping ys for <Plug>Ysurround is not consistent with
" the default mappings of vi -- y is for yank.
nmap S  <Plug>Ysurround$
nmap s  <Plug>Ysurround
nmap ss  <Plug>Yssurround




" tohtml  "{{{2

let g:html_use_css = 1
let g:html_no_pre = 1
let g:html_ignore_folding = 1
let g:html_number_lines = 0




" vcsi  "{{{2

" fallback
nnoremap <Leader>v  <Nop>


let g:vcsi_diff_in_commit_buffer_p = 1
let g:vcsi_open_command = 'Split | enew'
let g:vcsi_use_native_message_p = 1




" vimfiler  "{{{2

autocmd MyAutoCmd FileType vimfiler
\ setlocal isfname=33-126,161-255

if executable('feh')
  call vimfiler#set_execute_file('bmp,gif,jpeg,jpg,png', 'feh')
endif
if executable('evince')
  call vimfiler#set_execute_file('pdf', 'evince')
endif
if executable('oobase')
  call vimfiler#set_execute_file('doc,odt,ott', 'oowriter')
  call vimfiler#set_execute_file('ods,xls', 'oocalc')
  call vimfiler#set_execute_file('odp,ppt', 'oodraw')
endif

nmap <C-w>.  <Plug>(vimfiler_create)


let g:vimfiler_as_default_explorer = 1
let g:vimfiler_edit_command = 'tabedit'
let g:vimfiler_enable_clipboard = 1
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_split_command = 'Split'
let g:vimfiler_trashbox_directory = expand('~/.trash')




" vimshell  "{{{2

autocmd MyAutoCmd FileType vimshell
\ call s:on_FileType_vimshell()

function! s:on_FileType_vimshell()
  call vimshell#altercmd#define('la', 'ls -Fa')
  call vimshell#altercmd#define('ll', 'ls -Fl')
  call vimshell#altercmd#define('lla', 'ls -Fla')
  call vimshell#altercmd#define('grep', 'grep -E')

  imap <buffer> <BS>  <Plug>(vimshell_another_delete_backward_char)
  imap <buffer> <C-h>  <Plug>(vimshell_another_delete_backward_char)
endfunction

nmap <C-@>  <Plug>(vimshell_split_switch)


let g:vimshell_user_prompt = ''
let g:vimshell_user_prompt .= s:win_p ? '$USERNAME' : '$USER'
let g:vimshell_user_prompt .= '."@"'
let g:vimshell_user_prompt .= '.hostname()'
let g:vimshell_user_prompt .= '." "'
let g:vimshell_user_prompt .= '.fnamemodify(getcwd(), ":p:~:h")'
let g:vimshell_user_prompt .= '." "'
let g:vimshell_user_prompt .= '.vimshell#vcs#info("[%s:%b]", "[%s:%b|%a]")'
let g:vimshell_prompt = 'YUKI.N' . ($USER ==# 'root' ? '# ' : '> ')

let g:vimshell_cd_command = 'CD'
let g:vimshell_enable_smart_case = 1
let g:vimshell_ignore_case = 0
if s:win_p
  let g:vimshell_interactive_no_echoback_commands = {
  \  'ghci': 1,
  \  'gosh': 1,
  \  'python': 1,
  \  'scala': 1,
  \ }
endif
let g:vimshell_split_command = 'Split'
let g:vimshell_split_height = 50
let g:vimshell_temporary_directory = expand('~/.vim/info/vimshell')




" Fin.  "{{{1

" must be written at the last.  see :help 'secure'.
set secure




" __END__  "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
