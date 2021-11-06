" My .vimrc
" Basic  "{{{1
" Absolute  "{{{2

if has('vim_starting')
  if has('win32') || has('win64')
    set runtimepath^=~\\vimfiles\\bundle\\*
    set runtimepath+=~\\vimfiles\\bundle\\*\\after
  else
    set runtimepath^=~/.vim/bundle/*
    set runtimepath+=~/.vim/bundle/*/after
  endif
endif

if has('kaoriya')
  " Kill default configs
  call delete($VIM . '/vimrc')
  call delete($VIM . '/gvimrc')
endif

function! s:SID_PREFIX() abort
  return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfunction




" Encoding  "{{{2

set encoding=utf-8

if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'

  if iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif

  let &fileencodings = 'utf-8'
  let &fileencodings .= ',' . s:enc_jis
  let &fileencodings .= ',' . s:enc_euc
  let &fileencodings .= ',' . 'cp932'
  let &fileencodings .= ',' . 'ucs-bom'

  unlet s:enc_euc
  unlet s:enc_jis
endif


if has('win32') || has('win64')
  language C
  set fileformat=unix
  set termencoding=cp932
elseif has('gui_macvim')
  language C
endif




" Options  "{{{2

if (1 < &t_Co || has('gui')) && has('syntax') && !exists('g:syntax_on')
  syntax enable
  if !exists('g:colors_name')
    colorscheme basic256
  endif
endif


filetype plugin indent on


if has('gui_running')
  set guicursor=a:blinkwait4000-blinkon1500-blinkoff500
  if has('gui_gtk2') || has('gui_gtk3')
    set guifont=Monospace\ 10
    set linespace=0
  elseif has('gui_macvim')
    set guifont=Monaco:h12
    set linespace=5
    set macmeta
    set transparency=0
    set visualbell
  elseif has('gui_win32')
    set guifont=Consolas:h10.5
    set guifontwide=Meiryo:h10.5
    set renderoptions=type:directx,renmode:5
    set linespace=5
  endif
  set guioptions=AcgM
endif


if has('kaoriya')
  set ambiwidth=auto
else
  set ambiwidth=double
endif
set backspace=indent,eol,start
set nobackup
if has('gui_running') && exists('+clipboard')
  set clipboard=autoselectml,exclude:cons\|linux
  if has('unnamedplus')
    set clipboard+=unnamedplus
  endif
endif
set cmdheight=1
set complete&
set complete-=i
set completeopt=menuone,longest
if has('conceal')
  set concealcursor=nc
  set conceallevel=0
endif
set confirm
set diffopt=filler,vertical
set directory&
set directory-=.
set directory^=~/tmp
set display=lastline
set noequalalways
set foldmethod=marker
set fileformats=unix,dos,mac
set hidden
set history=1000
if has('langmap') && exists('+langremap')
  set nolangremap
endif
set laststatus=2
set linebreak
set list
let &listchars = "tab:\u00bb ,trail:-,extends:>,precedes:<,conceal:|,nbsp:."
if has('multi_byte_ime') || has('xim')
  set iminsert=0
  set imsearch=0
endif
set keywordprg=:help
set undofile
set undodir=~/tmp,/tmp
set nowritebackup
set nrformats-=octal
set pumheight=20
set ruler
set scrolloff=5
set showcmd
set showtabline=1
if exists('+shellslash')
  set shellslash
endif
set spelllang=en_us
set splitbelow
set splitright
set synmaxcol=1000
set title
set titlestring=Vim:\ %f\ %h%r%m
set ttimeoutlen=50
set updatetime=1000
set viminfo^='1000 viminfo-='100
set virtualedit=block
set nowrapscan
set wildmenu
set wildmode=full


set autoindent
if exists('+breakindent')
  set breakindent
endif
set cinoptions=:1s,l1,g0,t0,(0,W1s
set formatoptions=roqnlmM1
set formatlistpat&
let &formatlistpat .= '\|^\s*[*+-]\s*'
set ignorecase
set incsearch
set shiftround
set smartcase
set smartindent
if exists('+tagcase')
  set tagcase=match
endif


let &statusline = ''
let &statusline .= '%<%f %h%m%r%w'
let &statusline .= '%='
let &statusline .= '['
let &statusline .=   '%{&l:fileencoding == "" ? &encoding : &l:fileencoding}'
let &statusline .=   '%{&l:bomb ? "/BOM" : ""}'
let &statusline .= ']'
let &statusline .= '[%{&l:fileformat}]'
let &statusline .= '   %-14.(%l,%c%V%) %P'


function! s:my_tabline() abort  "{{{
  let s = ''

  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let curbufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears

    let no = (i <= 10 ? i - 1 : '#')  " display 0-origin tabpagenr.
    let mod = getbufvar(curbufnr, '&modified') ? '+' : ' '
    let title = gettabvar(i, 'title')
    let title = title != '' ? title : fnamemodify(bufname(curbufnr), ':t')
    let title = title != '' ? title : '[No Name]'

    let s .= '%' . i . 'T'
    let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
    let s .= no
    let s .= mod
    let s .= title
    let s .= '%#TabLineFill#'
    let s .= '  '
  endfor

  let branch_name = s:vcs_branch_name(getcwd())
  let s .= '%#TabLineFill#%T'
  let s .= '%=%#TabLine#'
  let s .= branch_name
  let s .= ' | %<'
  let s .= fnamemodify(getcwd(), ':t')
  return s
endfunction  "}}}
let &tabline = '%!' . s:SID_PREFIX() . 'my_tabline()'


if exists('$TMUX')
  " Fix window title on tmux.
  let &t_fs = "\<C-g>"
  let &t_ts = "\<Esc>]2;"
endif


let g:mapleader = ','
let g:maplocalleader = '.'




" Misc.  "{{{2

augroup MyAutoCmd
  autocmd!
augroup END


call altercmd#load()
call arpeggio#load()
call metarw#define_wrapper_commands(0)




" Commands  {{{1
" :grep wrappers  "{{{2

command! -complete=file -nargs=+ Grep  call s:grep('grep', s:suitable_grepprg(), [<f-args>])
command! -complete=file -nargs=+ Lgrep  call s:grep('lgrep', s:suitable_grepprg(), [<f-args>])
command! -complete=file -nargs=+ GnuGrep  call s:grep('grep', 'grep -nHE', [<f-args>])
command! -complete=file -nargs=+ LGnuGrep  call s:grep('lgrep', 'grep -nHE', [<f-args>])
command! -complete=file -nargs=+ GitGrep  call s:grep('grep', 'git grep -n', [<f-args>])
command! -complete=file -nargs=+ LGitGrep  call s:grep('lgrep', 'git grep -n', [<f-args>])
command! -complete=file -nargs=+ Rg  call s:grep('grep', 'rg --vimgrep --no-heading --no-column', [<f-args>])
command! -complete=file -nargs=+ LRg  call s:grep('lgrep', 'rg --vimgrep --no-heading --no-column', [<f-args>])

function! s:grep(command, grepprg, args) abort
  let target = join(a:args[:-2], ' ')
  let original_grepprg = &l:grepprg

  let &l:grepprg = a:grepprg

  try
    if a:grepprg ==# 'internal'
      execute a:command '/'.escape(a:args[-1], '|/ ').'/j' target
    else
      execute a:command.'!' escape(shellescape(a:args[-1]), '|') target
    endif
  finally
    let &l:grepprg = original_grepprg
  endtry

  if a:command ==# 'grep'
    cwindow
  else  " lgrep
    lwindow
  endif
endfunction

function! s:suitable_grepprg() abort
  return s:find_nearest_parent_file_or_directory(getcwd(), '.git/') != ''
  \    ? 'git grep -n'
  \    : executable('rg')
  \    ? 'rg --vimgrep --no-heading --no-column'
  \    : executable('grep')
  \    ? 'grep -nHE'
  \    : &l:grepprg != ''
  \    ? &l:grepprg
  \    : &grepprg
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  LGrep




" :make wrappers  "{{{2

command! -bar -complete=file -nargs=* Make  call s:make('make', [<f-args>])
command! -bar -complete=file -nargs=* Lmake  call s:make('lmake', [<f-args>])
function! s:make(command, args) abort
  let original_winnr = winnr()
  try
    execute a:command.'!' join(a:args)
  catch
    echohl ErrorMsg
    echo v:exception
    echohl None
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

command! -nargs=? -complete=customlist,s:complete_fileencoding FileEncoding
\ setlocal fileencoding=<args>
function! s:complete_fileencoding(arglead, cmdline, cursorpos) abort
  " :help encoding-values.  "{{{
  let ENCODINGS = {
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
    let ENCODINGS[encoding] = 0
  endfor
  return sort(filter(keys(ENCODINGS), 's:starts_with(a:arglead, v:val)'))
endfunction

command! -nargs=? -complete=customlist,s:complete_fileformats FileFormat
\ setlocal fileformat=<args>
function! s:complete_fileformats(arglead, cmdline, cursorpos) abort
  return sort(filter(split(&fileformats, ','), 's:starts_with(a:arglead, v:val)'))
endfunction

command! -bar -nargs=1 TabIndent
\ setlocal noexpandtab softtabstop< tabstop=<args> shiftwidth=<args>
command! -bar -nargs=1 SpaceIndent
\ setlocal expandtab tabstop< softtabstop=<args> shiftwidth=<args>




" BufferCleaner  "{{{2

command! -bang -nargs=0 BufferCleaner  call s:cmd_BufferCleaner(<bang>0)
function! s:cmd_BufferCleaner(banged_p) abort
  let bufnrs = range(1, bufnr('$'))
  call filter(bufnrs, 'bufexists(v:val)
  \               && buflisted(v:val)
  \               && (bufname(v:val) == "" || !filereadable(bufname(v:val)))
  \               && (a:banged_p || !getbufvar(v:val, "&modified"))')
  for bufnr in bufnrs
    silent execute bufnr 'bdelete'.(a:banged_p ? '!' : '')
  endfor
  echo printf('%d buffer(s) deleted', len(bufnrs))
endfunction




" CD  "{{{2

command! -nargs=* -complete=customlist,s:complete_cdpath CD
\ call s:cmd_CD(<q-args>)

function! s:complete_cdpath(arglead, cmdline, cursorpos) abort
  return map(uniq(sort(globpath(&cdpath,
  \                             join(split(a:cmdline, '\s', !0)[1:], ' ') . '*/',
  \                             0,
  \                             !0))),
  \          'v:val[:-2]')
endfunction

function! s:cmd_CD(path) abort
  if a:path != ''
    cd `=a:path`
  else
    let project_root = s:find_nearest_parent_file_or_directory(expand('%:p:h'), '.git/')
    if project_root != ''
      cd `=project_root`
    else
      cd
    endif
  endif
  let t:cwd = getcwd()
  echo t:cwd
endfunction

AlterCommand cd  CD

autocmd MyAutoCmd TabEnter *
\   if !exists('t:cwd')
\ |   let t:cwd = getcwd()
\ | endif
\ | if isdirectory(t:cwd)
\ |   cd `=t:cwd`
\ | endif




" DiffOrig   "{{{2

command! DiffOrig
\   vertical new
\ | setlocal buftype=nofile
\ | read ++edit #
\ | 0d_
\ | diffthis
\ | wincmd p
\ | diffthis




" FoldDump  "{{{2

command! -range=% FoldDump
\ <line1>,<line2>global/^/echo printf("%*d [%2s] %s", len(line('$')), line('.'), eval(substitute(&l:foldexpr, 'v:lnum', line('.'), '')), getline('.'))




" Note  "{{{2

command! -bar -nargs=? Note
\ execute 'edit' printf('%s/Sync/Documents/Notes/%s%s.md', $HOME, strftime('%Y-%m-%d'), <q-args> != '' ? '_' . <q-args> : '')




" HelpTagsAll  "{{{2

command! -bang -nargs=0 HelpTagsAll  call s:cmd_HelpTagsAll(<bang>0)
function! s:cmd_HelpTagsAll(banged_p) abort
  for path in split(globpath(&runtimepath, 'doc'), '\n')
    if filewritable(path)
      helptags `=path`
      if a:banged_p
        echo fnamemodify(path, ':~')
      endif
    endif
  endfor
endfunction




" Rename  "{{{2

command! -complete=file -nargs=1 Rename  call s:cmd_Rename(<q-args>)
function! s:cmd_Rename(name) abort
  let current = expand('%')
  if &l:readonly || !&l:modifiable || (filereadable(current) && !filewritable(current))
    echohl ErrorMsg
    echo 'This file cannot be changes:' a:name
    echohl None
  elseif filereadable(a:name)
    echohl ErrorMsg
    echo 'Renamed file already exists:' a:name
    echohl None
  else
    let directory = fnamemodify(a:name, ':p:h')
    if !isdirectory(directory)
      call mkdir(directory, 'p')
    endif
    file `=a:name`
    call delete(current)
    write
    redraw
    echo 'Renamed:' current '->' a:name
  endif
endfunction




" Reverse  {{{2

command! -bar -range=% Reverse  <line1>,<line2>g/^/m<line1>-1 | nohlsearch




" Seq  "{{{2
"
" :Seq /{pattern}[/format][/options]
"
" Example: Prints the number in front of each line
" :Seq/^/%03d /
"
" Example: Sets the start number and the step
" :Seq/^/%d /100+2

command! -range -nargs=+ Seq
\ <line1>,<line2>call s:cmd_Seq(<q-args>)
function! s:cmd_Seq(args) abort range
  let parse_pattern = '\v^([\x00-\xff]&[^\\"|[:alnum:][:blank:]])'
  \                 . '(%(\\.|.){-})'
  \                 .   '%(\1(%(\\.|.){-})'
  \                 .      '%(\1(%(\\.|.){-}))?)?$'
  let matches = matchlist(a:args, parse_pattern)
  if empty(matches)
    throw 'invalid arguments'
  endif

  let separator = matches[1]
  let pattern = matches[2]
  let format = matches[3]
  let options = matches[4]

  let incrementer = {
  \   'format': format,
  \   'current': 1,
  \   'step': 1,
  \ }

  let substitute_options = ''
  for c in split(options, '\([+-]\?\d\+\|.\)\zs')
    if c =~ '^\d\+'
      let incrementer.current = str2nr(c)
    elseif c =~ '^[+-]\d\+'
      let incrementer.step = str2nr(c)
    else
      let substitute_options .= c
    endif
  endfor

  function incrementer.call() dict
    let next = printf(self.format, self.current)
    let self.current += self.step
    return next
  endfunction

  execute printf('%d,%dsubstitute%s%s%s\=incrementer.call()%s%s',
  \              a:firstline,
  \              a:lastline,
  \              separator,
  \              pattern,
  \              separator,
  \              separator,
  \              substitute_options)
endfunction




" Source  "{{{2

command! -bar -complete=file -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>

AlterCommand so[urce]  Source




" SuspendWithAutomticCD  "{{{2

command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCD()
function! s:cmd_SuspendWithAutomticCD() abort
  let shell = split(&shell, '/')[-1]

  if has('gui_running') && has('macunix')
    silent !open -a iTerm
  elseif exists('$TMUX')
    let select_command = 'new-window'
    let target_pane = ':$'
    for window in split(system('tmux list-windows'), '\n')
      let matches = matchlist(window, '^\(\d\+\):\s\(\w\+\)')
      if !empty(matches) && matches[2] ==# shell
        let select_command = 'select-window -t '.matches[1]
        let target_pane = ':'.matches[1]
        break
      endif
    endfor
    silent execute '!tmux'
    \              select_command '\;'
    \              'send-keys -t '.target_pane.' C-u " cd \"'.getcwd().'\"" C-m'
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




" SyntaxName "{{{2

command! -bar -nargs=0 SyntaxName
\ echo join(<SID>syntax_name(line('.'), col('.')), '/')
function! s:syntax_name(line, col) abort
  let names = []

  for syn_id in synstack(a:line, a:col)
    let syn_trans_id = synIDtrans(syn_id)
    let name = synIDattr(syn_id, 'name')

    if syn_id != syn_trans_id
      let name .= '<' . synIDattr(syn_trans_id, 'name') . '>'
    endif

    call add(names, name)
  endfor

  return names
endfunction




" TabpageTitle  "{{{2

command! -bar -nargs=* TabpageTitle
\   if <q-args> == ''
\ |   let t:title = input("Set tabpage's title to: ", '')
\ | else
\ |   let t:title = <q-args>
\ | endif
\ | redraw!




" Sum  "{{{2

function! s:reduce_matched_items(func, initial, first_line, last_line, pattern, options) abort
  let reducer = { 'func': a:func, 'acc': a:initial }

  function reducer.step(value) dict
    let self.acc = call(self.func, [self.acc, a:value])
  endfunction

  execute printf('silent %d,%dsubstitute/%s/\=reducer.step(submatch(0))/%s',
  \              a:first_line,
  \              a:last_line,
  \              a:pattern,
  \              a:options)

  let @/ = a:pattern

  return reducer.acc
endfunction

function! s:sum(acc, value) abort
  return a:acc + eval(a:value)
endfunction
command! -bang -range -nargs=* Sum
\ call append(<line2>, string(s:reduce_matched_items(s:SID_PREFIX() . 'sum', 0, <line1>, <line2>, <q-args> != '' ? <q-args> : '\d\+\%(\.\d\+\)\?', <bang>0 ? 'en' : 'egn')))




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

command! -bang -bar -complete=file -nargs=? Dos
\ edit<bang> ++ff=dos <args>
command! -bang -bar -complete=file -nargs=? Unix
\ edit<bang> ++ff=unix <args>
command! -bang -bar -complete=file -nargs=? Mac
\ edit<bang> ++ff=mac <args>

command! -bang -bar -complete=file -nargs=? Jis  Iso2022jp<bang> <args>
command! -bang -bar -complete=file -nargs=? Sjis  Cp932<bang> <args>
command! -bang -bar -complete=file -nargs=? Unicode  Utf16<bang> <args>




" Utilities  "{{{1
" High-level key sequences  "{{{2

function! s:keys_to_complete() abort
  if &l:filetype ==# 'vim'
    return "\<C-x>\<C-v>"
  elseif &l:omnifunc != ''
    return "\<C-x>\<C-o>"
  elseif &l:completefunc != ''
    return "\<C-x>\<C-u>"
  else
    return "\<C-n>"
  endif
endfunction


function! s:keys_to_insert_one_character() abort
  echohl ModeMsg
  echo '-- INSERT (one char) --'
  echohl None
  return nr2char(getchar()) . "\<Esc>"
endfunction


function! s:keys_to_stop_insert_mode_completion() abort
  if pumvisible()
    return "\<C-e>"
  else
    return "\<Space>\<BS>"
  endif
endfunction




" Jump sections  "{{{2

" for normal mode.  a:pattern is '/regexp' or '?regexp'.
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


" for visual mode.  a:motion is '[[', '[]', ']]' or ']['.
function! s:jump_section_v(motion) abort
  execute 'normal!' "gv\<Esc>"
  execute 'normal' v:count1 . a:motion
  let line = line('.')
  let col = col('.')

  normal! gv
  call cursor(line, col)
endfunction


" for operator-pending mode.  a:motion is '[[', '[]', ']]' or ']['.
function! s:jump_section_o(motion) abort
  execute 'normal' v:count1 . a:motion
endfunction





" Plugin manager  "{{{2

command! -bang -complete=customlist,s:complete_bundle -nargs=+ Bundle
\ call s:bundle._dispatch(<f-args>, <bang>0)
function! s:complete_bundle(arglead, cmdline, cursorpos) abort
  return filter(keys(s:bundle),
  \             'v:val =~# "^[a-z]" && s:starts_with(a:arglead, v:val)')
endfunction

let s:bundle = {
\   'DIR': $HOME . '/.vim/bundle'
\ }

function! s:bundle._dispatch(action, ...) abort dict  "{{{3
  call call(get(self, a:action, self.help), a:000, self)
endfunction


function! s:bundle.help(banged_p) abort dict  "{{{3
  echo 'Usage:'
  echo printf('  :Bundle [%s]',
  \           join(filter(keys(self), 'v:val =~# "^[a-z]"'), '|'))
endfunction


function! s:bundle.install(package, banged_p) abort dict  "{{{3
  let m = matchlist(a:package, '^\(\%(git\|https\?\)://[^/]\+\)\?/*\(.\+\)')

  if !empty(m)
    let repository = (m[1] == '' ? 'git://github.com' : m[1])
    \              . '/'
    \              . substitute(m[2], '\%(\.git\)\?$', '.git', '')

    let bundle = self.DIR . '/' . substitute(m[2], '/', '_', '')
    let command = join([
    \   'git',
    \   'clone',
    \   repository,
    \   shellescape(bundle)
    \ ])

    if isdirectory(bundle)
      echohl ErrorMsg
      echo 'Already installed package:' a:package
      echohl None
    else
      echo system(command)
      silent! execute 'source' bundle . '/plugin/*.vim'
    endif
  else
    echohl ErrorMsg
    echo 'Invalid package:' a:package
    echohl None
  endif
endfunction


function! s:bundle.list(banged_p) abort dict  "{{{3
  let bundles = split(glob(self.DIR . '/*'), "\n")

  echohl Title
  echo len(bundles) 'plugins available'
  echohl None

  for bundle in bundles
    let head = bundle . '/.git/HEAD'
    if filereadable(head)
      echo readfile(head, '')[0] fnamemodify(bundle, ':t')
    else
      echo fnamemodify(bundle, ':t')
    endif
  endfor
endfunction


function! s:bundle.update(banged_p) abort dict  "{{{3
  for bundle in split(glob(self.DIR . '/*'), "\n")
    if !a:banged_p
    \  && localtime() - getftime(bundle . '/.git/FETCH_HEAD') < 60 * 60 * 24
      echohl Title
      echo fnamemodify(bundle, ':t') 'is already updated'
      echohl None
      continue
    endif

    let git = join([
    \   'git',
    \   '--git-dir',
    \   shellescape(bundle . '/.git'),
    \   '--work-tree',
    \   shellescape(bundle)
    \ ])

    echohl Title
    echo 'Updating' fnamemodify(bundle, ':t') '...'
    echohl None

    let remote = split(system(git . ' remote -v'), '\n', 1)
    if v:shell_error != 0 || remote[0] !~# '^origin\s\(git\|https\?\)://'
      continue
    endif

    let result = system(git . ' pull --rebase')
    if v:shell_error == 0
      echo result
    else
      echohl ErrorMsg
      echomsg 'Error:' fnamemodify(bundle, ':t')
      echomsg result
      echohl None
    endif
  endfor
endfunction




" Toggle options  "{{{2

function! s:toggle_foldmethod(global_p) abort
  let VALUES = ['marker', 'expr']
  let foldmethod = &l:foldmethod == '' ? &foldmethod : &l:foldmethod
  let i = (index(VALUES, foldmethod) + 1) % len(VALUES)

  if a:global_p
    let &foldmethod = VALUES[i]
    set foldmethod?
  else
    let &l:foldmethod = VALUES[i]
    setlocal foldmethod?
  endif
endfunction


function! s:toggle_option(option_name) abort
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
endfunction


function! s:toggle_colorcolumn() abort
  if exists('b:textwidth')
    let &l:textwidth = b:textwidth
    unlet b:textwidth
    setlocal colorcolumn& colorcolumn?
  else
    let b:textwidth = &l:textwidth
    if b:textwidth == 0
      setlocal textwidth=100
    endif
    setlocal colorcolumn=+1 colorcolumn?
  endif
endfunction




" Window helpers  "{{{2

function! s:vertical_p() abort
  return winwidth(0) > winheight(0) * 5
endfunction

function! s:vertical_with(command, args) abort
  execute s:vertical_p() ? 'vertical' : ''
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




" VCS branch name  "{{{2
" Returns the name of the current branch of the given directory.
let s:_vcs_branch_name_cache = {}  " dir_path = [branch_name, cache_key]


function! s:vcs_branch_name(dir) abort
  let cache_entry = get(s:_vcs_branch_name_cache, a:dir, 0)
  if cache_entry is 0
  \  || cache_entry[1] !=# s:_vcs_branch_name_cache_key(a:dir)
    unlet cache_entry
    let cache_entry = s:_vcs_branch_name(a:dir . '/.git')
    let s:_vcs_branch_name_cache[a:dir] = cache_entry
  endif

  return cache_entry[0]
endfunction


function! s:_vcs_branch_name_cache_key(dir) abort
  return getftime(a:dir . '/.git/HEAD') . getftime(a:dir . '/.git/MERGE_HEAD')
endfunction


function! s:_vcs_branch_name(dir) abort
  if isdirectory(a:dir)
    if isdirectory(a:dir . '/rebase-apply')
      if filereadable(a:dir . '/rebase-apply/rebasing')
        let additional_info = 'REBASE'
      elseif filereadable(a:dir . '/rebase-apply/applying')
        let additional_info = 'AM'
      else
        let additional_info = 'AM/REBASE'
      endif
      let head_info = s:first_line(a:dir . '/HEAD')
    elseif filereadable(a:dir . '/rebase-merge/interactive')
      let additional_info = 'REBASE-i'
      let head_info = s:first_line(a:dir . '/rebase-merge/head-name')
    elseif isdirectory(a:dir . '/rebase-merge')
      let additional_info = 'REBASE-m'
      let head_info = s:first_line(a:dir . '/rebase-merge/head-name')
    elseif filereadable(a:dir . '/MERGE_HEAD')
      let additional_info = 'MERGING'
      let head_info = s:first_line(a:dir . '/HEAD')
    else  " Normal case
      let additional_info = ''
      let head_info = s:first_line(a:dir . '/HEAD')
    endif

    let branch_name = matchstr(head_info, '^\(ref: \)\?refs/heads/\zs\S\+\ze$')
    if branch_name == ''
      let lines = readfile(a:dir . '/logs/HEAD')
      let co_lines = filter(lines, 'v:val =~# "checkout: moving from"')
      let log = empty(co_lines) ? '' : co_lines[-1]
      let branch_name = substitute(log, '^.* to \([^ ]*\)$', '\1', '')
      if branch_name == ''
        let branch_name = '(unknown)'
      endif
    endif
    if additional_info != ''
      let branch_name .= ' ' . '(' . additional_info . ')'
    endif
  elseif filereadable(a:dir)
    return s:_vcs_branch_name(matchstr(s:first_line(a:dir), 'gitdir:\s\zs.*'))
  else  " Not in a git repository.
    let branch_name = ''
  endif

  return [branch_name, s:_vcs_branch_name_cache_key(a:dir)]
endfunction




function! s:close_temporary_windows() abort  "{{{2
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




function! s:find_nearest_parent_file_or_directory(path, filenames) abort  "{{{2
  let filenames = type(a:filenames) == type([]) ? a:filenames : [a:filenames]
  for filename in filenames
    if filename[-1:] ==# '/'
        let modifiers = ':p:h:h'
        let found_path = finddir(filename, a:path . ';')
    else
        let modifiers = ':p:h'
        let found_path = findfile(filename, a:path . ';')
    endif
    if !empty(found_path)
      return fnamemodify(found_path, modifiers)
    endif
  endfor
  return ''
endfunction




function! s:first_line(file) abort  "{{{2
  let lines = readfile(a:file, '', 1)
  return 1 <= len(lines) ? lines[0] : ''
endfunction




function! s:move_window_into_tabpage(target_tabpagenr) abort  "{{{2
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




function! s:operator_increment(motion_wiseness) abort  "{{{2
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]g'."\<C-a>"
endfunction




function! s:operator_decrement(motion_wiseness) abort  "{{{2
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]g'."\<C-x>"
endfunction




function! s:operator_search(motion_wiseness) abort  "{{{2
  let reg_0 = [@0, getregtype('0')]

  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]"0y'

  let @/ = '\V' . substitute(escape(@0, '\'), '\n', '\\n', 'g')
  call histadd('/', @/)
  execute 'normal!' v:searchforward ? 'n' : 'N'

  call setreg('0', reg_0[0], reg_0[1])
endfunction




function! s:operator_speak(motion_wiseness) abort  "{{{2
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]y'

  let text = @"

  if text =~ '[^\x00-\x7F]'
    call google_translate#speak(text, 'ja')
  else
    call google_translate#speak(text, 'en')
  endif
endfunction




function! s:operator_translate(motion_wiseness) abort  "{{{2
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]y'

  let text = @"

  if text =~ '[^\x00-\x7F]'
    let @" = google_translate#translate(text, 'ja', 'en')
  else
    let @" = google_translate#translate(text, 'en', 'ja')
  endif

  echo trim(@", "\n") . "\n"
endfunction




function! s:operator_yank_clipboard(motion_wiseness) abort  "{{{2
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal' '`['.visual_command.'`]"+y'
endfunction




function! s:starts_with(x, y) abort  "{{{2
  return a:x ==# strpart(a:y, 0, len(a:x))
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
nnoremap qwg  :<C-u>LGrep<Space>




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
for s:i in range(10)
  execute 'nnoremap <silent>' ('<C-t>'.(s:i))  ((s:i+1).'gt')
endfor
unlet s:i


" Moving tabpages themselves.  "{{{3

nnoremap <silent> <C-t>l
\ :<C-u>execute 'tabmove' '+'.v:count1<CR>
nnoremap <silent> <C-t>h
\ :<C-u>execute 'tabmove' '-'.v:count1<CR>
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

" Move the cursor instead of selecting a different match
cnoremap <Left> <Space><BS><Left>
cnoremap <Right> <Space><BS><Right>
cmap <C-b>  <Left>
cmap <C-f>  <Right>

cnoremap <C-o>  <C-f>

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

function! s:on_CmdwinEnter() abort
  nnoremap <buffer> <Esc><Esc>  <Esc><C-w>q
  inoremap <buffer> <Esc><Esc>  <Esc><C-w>q
  inoremap <buffer> <expr> <C-c>  pumvisible() ? "\<Esc>" : "\<C-c>\<C-c>"
  inoremap <buffer> <expr> <BS>
         \ getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<BS>"
  inoremap <buffer> <expr> <C-w>
         \ getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<C-w>"
  inoremap <buffer> <expr> <C-u>
         \ getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<C-u>"
  imap <buffer> <C-h>  <BS>

  startinsert!
endfunction




" Insert mode  "{{{2

" Like emacs mappings.
inoremap <C-b>  <Left>
inoremap <C-f>  <Right>

inoremap <C-d>  <Delete>
inoremap <expr> <C-a>
     \   indent(line('.')) > 0 && virtcol('.') > indent(line('.')) + 1
     \ ? "\<Home>\<S-Right>"
     \ : "\<Home>"
inoremap <expr> <C-e>
       \ virtcol('.') < indent(line('.')) ? "\<S-Right>" : "\<End>"
inoremap <expr> <C-k>
       \ repeat("\<Delete>", max([strchars(getline('.')[col('.') - 1:]), 1]))
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

function! s:should_indent_rather_than_complete_p() abort
  return getline('.')[col('.') - 2] !~ '^\S'
endfunction




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




" The <Space>  "{{{2

" to show <Space> in the bottom line.
map <Space>  [Space]

" fallback
noremap [Space]  <Nop>


nnoremap <silent> [Space]w  :<C-u>w<CR>
if executable('sudo')
  nnoremap <silent> [Space]W  :<C-u>w sudo:%<CR>
endif

nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>

nnoremap <silent> [Space]c  :<C-u>call <SID>close_temporary_windows()<CR>

nnoremap [Space]f  <Nop>
nnoremap [Space]fe  :<C-u>FileEncoding<Space>
nnoremap [Space]ff  :<C-u>FileFormat<Space>
nnoremap [Space]fr  :<C-u>Rename <C-r>%
nnoremap [Space]fs  :<C-u>setlocal filetype? fileencoding? fileformat?<CR>
nnoremap [Space]ft  :<C-u>setfiletype<Space>

" Append one character.
nnoremap [Space]A  A<C-r>=<SID>keys_to_insert_one_character()<CR>
nnoremap [Space]a  a<C-r>=<SID>keys_to_insert_one_character()<CR>

" Insert one character.
nnoremap [Space]I  I<C-r>=<SID>keys_to_insert_one_character()<CR>
nnoremap [Space]i  i<C-r>=<SID>keys_to_insert_one_character()<CR>

" Put from clipboard.
nmap [Space]p  "+p

" Open a fold.
nnoremap [Space]l  zo

" Close a fold.
nnoremap [Space]h  zc

" Close all folds but including the cursor.
nnoremap [Space]v  zMzv

" Enter command-line window.
nnoremap [Space]:  q:
xnoremap [Space]:  q:

nnoremap [Space]o  <Nop>
nnoremap <silent> [Space]oc  :<C-u>call <SID>toggle_colorcolumn()<CR>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]op  :<C-u>call <SID>toggle_option('paste')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ot  :<C-u>TableModeToggle<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>
nnoremap <silent> [Space]oz  :<C-u>call <SID>toggle_foldmethod(0)<CR>

nnoremap <silent> [Space]q  :<C-u>Help quickref<CR>
nnoremap <silent> [Space]m  :<C-u>marks<CR>
nnoremap <silent> [Space]r  :<C-u>registers<CR>

nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>




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
function! s:ask_tabpage_number() abort
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


" Select the last chaged text - "c" stands for "C"hanged.
nnoremap <Plug>(textobj-last-changed-text)  `[v`]h
onoremap <silent> <Plug>(textobj-last-changed-text)  :<C-u>normal gc<CR>
vnoremap <silent> <Plug>(textobj-last-changed-text)  :<C-u>normal gc<CR>
map gc  <Plug>(textobj-last-changed-text)


" Select the last selected text.
onoremap <silent> gv  :<C-u>normal! gv<CR>


" Alias for brackets
onoremap (  t(
vnoremap (  t(
onoremap )  t)
vnoremap )  t)




" Operators  "{{{2
" operator-increment and decrement "{{{3

call operator#user#define('increment',
\                         s:SID_PREFIX() . 'operator_increment')
call operator#user#define('search-backward',
\                         s:SID_PREFIX() . 'operator_decrement')
map g<C-a>  <Plug>(operator-increment)
map g<C-x>  <Plug>(operator-deincrement)


" operator-search  "{{{3

call operator#user#define('search-forward',
\                         s:SID_PREFIX() . 'operator_search',
\                         'let v:searchforward = 1')
call operator#user#define('search-backward',
\                         s:SID_PREFIX() . 'operator_search',
\                         'let v:searchforward = 0')
vmap *  <Plug>(operator-search-forward)
vmap #  <Plug>(operator-search-backward)


" operator-translate  "{{{3

call operator#user#define('translate', s:SID_PREFIX() . 'operator_translate')
Arpeggio map ot  <Plug>(operator-translate)


" operator-yank-clipboard  "{{{3

call operator#user#define('yank-clipboard',
\                         s:SID_PREFIX() . 'operator_yank_clipboard')
Arpeggio map oy  <Plug>(operator-yank-clipboard)




" Misc.  "{{{2

nnoremap <silent> <Leader><Leader>  :<C-u>update<CR>


nnoremap <C-h>  :<C-u>Help<Space>
nnoremap <C-o>  :<C-u>edit<Space>
nnoremap <C-w>.  :<C-u>edit .<CR>


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
function! s:start_insert_mode_with_blank_lines(command) abort
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

function! s:search_forward_p() abort
  return exists('v:searchforward') ? v:searchforward : 1
endfunction

" 'gf' with split
noremap <silent> gs  :<c-u>call <SID>vertical_with('wincmd', ['f'])<CR>
noremap <silent> gS  :<c-u>call <SID>vertical_with('wincmd', ['F'])<CR>




" Filetypes  "{{{1
" All filetypes   "{{{2

autocmd MyAutoCmd FileType *
\ call s:on_FileType_any()

function! s:on_FileType_any() abort
  if &l:completefunc == ''
    setlocal completefunc=autoprogramming#complete
  endif

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


" Optimize a huge file loading.
autocmd MyAutoCmd BufReadPre *
\   if getfsize(expand("<afile>")) > 1024 * 1024 * 10
\ |   set eventignore+=FileType
\ |   setlocal noswapfile
\ | else
\ |   set eventignore-=FileType
\ | endif


" Fix 'fileencoding' to use 'encoding'.
autocmd MyAutoCmd BufReadPost *
\   if &l:modifiable && !search('[^\x00-\x7F]', 'cnw', 100)
\ |   setlocal fileencoding=
\ | endif


" Load project-specific vimrc.
autocmd MyAutoCmd BufNewFile,BufReadPost *
\ call s:load_local_vimrc(expand('<afile>:p:h'))
function! s:load_local_vimrc(path) abort
  let path = s:find_nearest_parent_file_or_directory(a:path, '.vimrc.local')
  if !empty(path)
    source `path`
  endif
endfunction


" Unset 'paste' automatically.
autocmd MyAutoCmd InsertLeave *  set nopaste


" Visualize ideographic spaces.
autocmd MyAutoCmd VimEnter,WinEnter *  match Underlined /[\u3000\ufff9-\ufffc]/


" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler.
autocmd MyAutoCmd BufReadPost *
\   if line("'\"") >= 1 && line("'\"") <= line("$")
\ |   execute "normal! g`\""
\ | endif


" Prevent doubly `filetype` setting on `*.ebuild` and `*.eclass` files.
let g:ft_ignore_pat = 'e\(build\|class\)$'




" actionscript  "{{{2

autocmd MyAutoCmd FileType actionscript
\   TabIndent 4
\ | setlocal commentstring=//%s
\ | compiler ant




" blade  "{{{2

autocmd MyAutoCmd FileType blade
\ SpaceIndent 4




" c  "{{{2

autocmd MyAutoCmd FileType c
\ setlocal commentstring=//%s




" changelog  "{{{2

" Fix the new entry mapping bug.
autocmd MyAutoCmd FileType changelog
\ noremap <buffer> <silent> <Leader>o  :<C-u>NewChangelogEntry<CR>

let g:changelog_timeformat = '%Y-%m-%d'
let g:changelog_username = 'Shota Nozaki <emonkak@gmail.com>'




" coffee  "{{{2

autocmd MyAutoCmd FileType coffee
\ SpaceIndent 2




" cs  "{{{2

let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']

autocmd MyAutoCmd FileType cs
\ call s:on_FileType_cs()

function! s:on_FileType_cs() abort
  SpaceIndent 4

  setlocal commentstring=//%s
  setlocal textwidth=100

  inoreabbrev <buffer> ///  /// <summary><CR><CR></summary><Up>
endfunction




" css, less, sass, scss  "{{{2

autocmd MyAutoCmd FileType css,less,sass,scss
\   SpaceIndent 2
\ | setlocal iskeyword+=-




" dockerfile "{{{2

autocmd MyAutoCmd FileType dockerfile
\ SpaceIndent 4




" dosini (.ini)  "{{{2

autocmd MyAutoCmd FileType dosini
\ call s:on_FileType_dosini()

function! s:on_FileType_dosini() abort
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




" git  "{{{2

autocmd MyAutoCmd FileType gitcommit
\ setlocal nofoldenable




" glsl  "{{{2

autocmd MyAutoCmd FileType glsl
\ SpaceIndent 4




" haskell  "{{{2

autocmd MyAutoCmd FileType haskell
\   SpaceIndent 2
\ | compiler cabal

autocmd MyAutoCmd FileType cabal
\ SpaceIndent 2

" Fix hsc highlighting
let g:hs_allow_hash_operator = 1

let g:haskell_conceal = 0




" haxe  "{{{2

autocmd MyAutoCmd FileType haxe
\   SpaceIndent 2
\ | setlocal commentstring=//%s




" java  "{{{2

autocmd MyAutoCmd FileType java
\   SpaceIndent 4
\ | compiler gradle
\ | setlocal cinoptions=:1s,l1,g0,t0,(0,j1
\ | setlocal foldmethod=syntax foldnestmax=2
\ | setlocal textwidth=100




" javascript  "{{{2

autocmd MyAutoCmd FileType javascript
\   SpaceIndent 4
\ | setlocal iskeyword-=58 iskeyword+=$
\ | setlocal cinoptions-=(0

let g:jsx_ext_required = 0




" json  "{{{2

autocmd MyAutoCmd FileType json
\ SpaceIndent 4




" lua  "{{{2

autocmd MyAutoCmd FileType lua
\ SpaceIndent 2




" markdown  "{{{2

autocmd MyAutoCmd FileType markdown
\ SpaceIndent 4




" nginx  "{{{2

autocmd MyAutoCmd FileType nginx
\ SpaceIndent 4




" objc  "{{{2

autocmd MyAutoCmd FileType objc
\   SpaceIndent 4
\ | setlocal commentstring=//%s




" ocaml  "{{{2

autocmd MyAutoCmd FileType ocaml
\   SpaceIndent 2
\ | setlocal commentstring=(*%s*)




" perl  "{{{2

autocmd MyAutoCmd FileType perl
\ SpaceIndent 2




" php  "{{{2

autocmd MyAutoCmd FileType php
\ call s:on_FileType_php()

function! s:on_FileType_php() abort
  SpaceIndent 4
  compiler psalm
  setlocal commentstring=//%s
  setlocal iskeyword+=$

  inoreabbrev <buffer> /** /**<Space>*/<Left><Left><Left>
endfunction

let g:PHP_vintage_case_default_indent = 1
let g:PHP_noArrowMatching = 1




" python  "{{{2

autocmd MyAutoCmd FileType python
\ SpaceIndent 4

let g:python_highlight_all = 1




" quickfix  "{{{2

autocmd MyAutoCmd FileType qf
\ setlocal nobuflisted nocursorline




" ruby  "{{{2

autocmd MyAutoCmd FileType ruby
\ SpaceIndent 2




" rust  "{{{2

autocmd MyAutoCmd FileType rust
\   SpaceIndent 4
\ | compiler cargo

let g:cargo_makeprg_params = 'build'




" scala  "{{{2

autocmd MyAutoCmd FileType scala
\ SpaceIndent 2




" scheme  "{{{2

let g:is_gauche = 1




" sh, zsh  "{{{2

autocmd MyAutoCmd FileType sh,zsh
\ SpaceIndent 2

let g:is_bash = 1




" sql  "{{{2

autocmd MyAutoCmd FileType sql
\   SpaceIndent 2
\ | setlocal commentstring=--%s




" swift  "{{{2

autocmd MyAutoCmd FileType swift
\ SpaceIndent 4




" tex  "{{{2

autocmd MyAutoCmd FileType tex,plaintex
\   call s:on_FileType_tex()
\ | compiler tex

function! s:on_FileType_tex() abort
  SpaceIndent 2

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




" typescript  "{{{2

autocmd MyAutoCmd FileType typescript
\   SpaceIndent 4
\ | setlocal makeprg=./node_modules/.bin/tsc
\ | setlocal commentstring=//%s
\ | setlocal iskeyword-=:




" vim  "{{{2

autocmd MyAutoCmd FileType vim
\   SpaceIndent 2
\ | setlocal keywordprg=:help

let g:vim_indent_cont = 0
let g:vimsyn_embed = 'l'




" xml  "{{{2

autocmd MyAutoCmd FileType ant,docbk,html,mustache,smarty,svg,xhtml,xml,xslt
\ call s:on_FileType_xml()

function! s:on_FileType_xml() abort
  SpaceIndent 2

  " Complete proper end-tags.
  " In the following description, {|} means the cursor position.

  " Insert the end tag after the cursor.
  " Before: <code{|}
  " After:  <code>{|}</code>
  inoremap <buffer> <LT><LT>  ><LT>/<C-x><C-o><C-r>=
                             \<SID>keys_to_stop_insert_mode_completion()
                             \<CR><C-o>F<LT>

  " Wrap the cursor with the tag.
  " Before: <code{|}
  " After:  <code>
  "           {|}
  "         </code>
  inoremap <buffer> >>  ><CR>X<CR><LT>/<C-x><C-o><C-d><C-r>=
                       \<SID>keys_to_stop_insert_mode_completion()
                       \<CR><C-o><Up><BS>

  " To deal with namespace prefixes and tag-name-including-hyphens.
  setlocal iskeyword+=-
  setlocal iskeyword+=:
endfunction


" For light weight template engines
autocmd MyAutoCmd FileType haml,jade,slim
\ SpaceIndent 2




" yaml  "{{{2

autocmd MyAutoCmd FileType yaml
\ SpaceIndent 2




" Plugins  "{{{1
" accelerate  "{{{2

call accelerate#map('nv', 'e', '<C-u>', 'repeat("\<C-u>", v:count1)')
call accelerate#map('nv', 'e', '<C-d>', 'repeat("\<C-d>", v:count1)')

call accelerate#map('nv', 'e', 'j', 'v:count == 0 ? "gj" : "j"')
call accelerate#map('nv', 'e', 'k', 'v:count == 0 ? "gk" : "k"')
call accelerate#map('nv', '', 'h', 'h')
call accelerate#map('nv', 'e', 'l', 'foldclosed(line(".")) != -1 ? "zo" : "l"')




" altr  "{{{2

call altr#reset()
call altr#define('%.m', '%.h')

nmap <F1>  <Plug>(altr-back)
nmap <F2>  <Plug>(altr-forward)




" exjumplist  "{{{2

nmap <Esc><C-j>  <Plug>(exjumplist-next-buffer)
nmap <Esc><C-k>  <Plug>(exjumplist-previous-buffer)
nmap <M-C-j>  <Plug>(exjumplist-next-buffer)
nmap <M-C-k>  <Plug>(exjumplist-previous-buffer)




" fakeclip  "{{{2

let g:fakeclip_provide_clipboard_key_mappings = 1




" gfdiff  "{{{2

nmap gh  <Plug>(gfdiff-to)




" grex  "{{{2

nmap gy  <Plug>(operator-grex-yank)<Plug>(textobj-entire-a)
vmap gy  <Plug>(operator-grex-yank)
nmap gd  <Plug>(operator-grex-delete)<Plug>(textobj-entire-a)
vmap gd  <Plug>(operator-grex-delete)




" ku  "{{{2

autocmd MyAutoCmd FileType ku
\ call s:on_FileType_ku()

function! s:on_FileType_ku() abort
  call ku#default_key_mappings(1)

  iunmap <buffer> <C-j>
  iunmap <buffer> <C-k>

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

function! s:ku_common_action_my_cd(item) abort
  if isdirectory(a:item.word)
    execute 'CD' a:item.word
  else  " treat a:item as a file name
    execute 'CD' fnamemodify(a:item.word, ':h')
  endif
endfunction

function! s:ku_common_action_yank(item) abort
  call setreg('"', a:item.word, 'c')
endfunction
function! s:ku_common_action_Yank(item) abort
  call setreg('"', a:item.word, 'l')
endfunction

function! s:ku_file_action_open_sudo(item) abort
  edit `='sudo:' . fnamemodify(a:item.word, ':p')`
endfunction

function! s:ku_metarw_git_action_checkout(item) abort
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
if executable('portageq')
  call ku#custom_prefix('common', 'PORT', system('portageq portdir')[:-2])
end


nmap [Space]k  <Nop>
nnoremap <silent> [Space]ka  :<C-u>Ku args<CR>
nnoremap <silent> [Space]kb  :<C-u>Ku buffer<CR>
nnoremap <silent> [Space]kc  :<C-u>Ku colorscheme<CR>
nnoremap <silent> [Space]kf  :<C-u>Ku file<CR>
nnoremap <silent> [Space]kg  :<C-u>Ku metarw/git<CR>
nnoremap <silent> [Space]kh  :<C-u>Ku history<CR>
nnoremap <silent> [Space]kj  :<C-u>Ku file/current<CR>
nnoremap <silent> [Space]kl  :<C-u>Ku file_project<CR>
nnoremap <silent> [Space]kn  :<C-u>Ku file<CR>~/Sync/Documents/Notes/
nnoremap <silent> [Space]ko  :<C-u>Ku oldfiles<CR>
nnoremap <silent> [Space]kq  :<C-u>Ku quickfix<CR>
nnoremap <silent> [Space]kr  :<C-u>Ku register<CR>
nnoremap <silent> [Space]ks  :<C-u>Ku source<CR>
nnoremap <silent> [Space]kt  :<C-u>Ku tags<CR>
nnoremap <silent> [Space]kw  :<C-u>Ku myproject<CR>
nnoremap <silent> [Space]kz  :<C-u>Ku fold<CR>

nnoremap <silent> [Space]k/  :<C-u>Ku cmd_mru/search<CR>
nnoremap <silent> [Space]k:  :<C-u>Ku cmd_mru/cmd<CR>
nnoremap <silent> [Space]km  :<C-u>Ku file_mru<CR>

nnoremap <silent> [Space]kk  :<C-u>call ku#restart()<CR>


let g:ku_file_mru_file = expand('~/.vimmru')
let g:ku_file_mru_ignore_pattern = '/$\|/\.git/\|^/\(/\|mnt\|tmp\)'
let g:ku_file_mru_limit = 1000





" lsp  "{{{2

autocmd MyAutoCmd User lsp_buffer_enabled
\ call s:on_lsp_buffer_enabled()
function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes

  if &l:foldmethod != 'expr'
    setlocal foldmethod=expr
    setlocal foldexpr=lsp#ui#vim#folding#foldexpr()
    setlocal foldtext=lsp#ui#vim#folding#foldtext()
  endif

  if exists('+tagfunc')
    setlocal tagfunc=lsp#tagfunc
  endif

  nmap <buffer> <silent> K  <Plug>(lsp-hover)

  nnoremap <buffer> <LocalLeader>l  <Nop>
  nmap <buffer> <silent> <LocalLeader>la  <Plug>(lsp-code-action)
  nmap <buffer> <silent> <LocalLeader>ld  <Plug>(lsp-definition)
  nmap <buffer> <silent> <LocalLeader>li  <Plug>(lsp-implementation)
  nmap <buffer> <silent> <LocalLeader>lr  <Plug>(lsp-references)
  nmap <buffer> <silent> <LocalLeader>lt  <Plug>(lsp-type-definition)
endfunction

autocmd MyAutoCmd User lsp_setup
\ call s:on_lsp_setup()
function! s:on_lsp_setup() abort
  if executable('haskell-language-server-wrapper')
    call lsp#register_server({
    \   'name': 'haskell-language-server',
    \   'cmd': {server_info -> [
    \     'haskell-language-server-wrapper',
    \     '--lsp',
    \     '--cwd', lsp#utils#uri_to_path(server_info['root_uri'](server_info)),
    \   ]},
    \   'root_uri': {server_info -> lsp#utils#path_to_uri(
    \      lsp#utils#find_nearest_parent_file_directory(
    \        lsp#utils#get_buffer_path(),
    \        ['.git/', 'Setup.hs', 'stack.yml']
    \      )
    \   )},
    \   'allowlist': ['haskell'],
    \ })
  endif
  if executable('rls')
    call lsp#register_server({
    \   'name': 'rls',
    \   'cmd': {server_info -> ['rls']},
    \   'root_uri': {server_info -> lsp#utils#path_to_uri(
    \      lsp#utils#find_nearest_parent_file_directory(
    \        lsp#utils#get_buffer_path(),
    \        ['.git/', 'Cargo.toml']
    \      )
    \   )},
    \   'allowlist': ['rust'],
    \ })
  endif
endfunction

let g:lsp_diagnostics_float_cursor = 1
let g:lsp_diagnostics_highlights_enabled = 1
let g:lsp_diagnostics_signs_error = {'text': 'X'}
let g:lsp_diagnostics_signs_hint = {'text': '?'}
let g:lsp_diagnostics_signs_information = {'text': 'i'}
let g:lsp_diagnostics_signs_warning = {'text': '!'}
let g:lsp_diagnostics_virtual_text_enabled = 0

let g:lsp_document_highlight_enabled = 0
let g:lsp_document_code_action_signs_enabled = 0

let g:lsp_tagfunc_source_methods = ['definition']

" let g:lsp_log_verbose = 1
" let g:lsp_log_file = expand('~/.vim/info/vim-lsp.log')

command! LspRestart  call lsp#ui#vim#stop_server() | edit




" metarw  "{{{2

let g:metarw_gist_safe_write = 1




" operator-camelize  "{{{2

map <Leader>c  <Plug>(operator-camelize)
map <Leader>C  <Plug>(operator-decamelize)




" operator-comment  "{{{2

Arpeggio map oc  <Plug>(operator-comment)
Arpeggio map od  <Plug>(operator-uncomment)




" operator-replece  "{{{2

Arpeggio map or  <Plug>(operator-replace)




" operator-sort  "{{{2

nmap [Space]S  <Plug>(operator-sort)$
vmap [Space]S  <Plug>(operator-sort)$
nmap [Space]s  <Plug>(operator-sort)
vmap [Space]s  <Plug>(operator-sort)




" quickrun  "{{{2

command! -complete=command -nargs=+ Capture  QuickRun vim -src <q-args>

let g:quickrun_config = {
\   '_': {
\     'outputter/buffer/opener': '%{'.s:SID_PREFIX().'vertical_p() ? "vsplit" : "split"}',
\   },
\   'c': {
\     'type': 'c/clang'
\   },
\   'objc': {
\     'type': 'objc/clang'
\   },
\   'objc/clang': {
\     'command': 'clang',
\     'cmdopt': '-framework Foundation -framework AppKit',
\     'exec': ['%c %o -o %s:p:r %s', '%s:p:r %a'],
\     'tempfile': '%{fnamemodify(tempname(), ":r")}.m',
\     'hook/sweep/files': '%S:p:r',
\   },
\   'cpp': {
\     'type': 'cpp/clang++',
\     'cmdopt': '-std=c++11'
\   },
\   'dot': {
\     'exec': ['%c -Tsvg -o %s:p:r.svg %s']
\   },
\   'php/hhvm': {
\     'command': 'hhvm',
\     'exec': ['%c %a %s']
\   },
\   'javascript': {
\     'type': 'javascript/nodejs'
\   },
\   'javascript/nodejs': {
\     'cmdopt': '--use_strict --harmony',
\     'command': 'node',
\     'tempfile': '%{tempname()}.js'
\   },
\   'json': {
\     'cmdopt': '.',
\     'command': 'jq',
\   },
\   'markdown/marked': {
\     'outputter': 'null',
\     'command': 'open',
\     'cmdopt': '-g',
\     'exec': '%c %o -a Marked %s'
\   },
\   'rust': {
\     'cmdopt': '-A dead_code --edition 2018',
\     'command': 'rustc',
\     'exec': ['%c %o %s -o %s:p:r', 'RUST_BACKTRACE=1 %s:p:r %a'],
\     'tempfile': '%{fnamemodify(tempname(), ":r")}.rs',
\     'hook/shebang/enable': 0,
\     'hook/sweep/files': '%S:p:r',
\   },
\   'sql': {
\     'type': 'sql/mysql'
\   },
\   'sql/mysql': {
\     'command': 'mysql',
\     'exec': ['%c --host 127.0.0.1 --user root %a < %s']
\   },
\   'typescript': {
\     'type': 'typescript/ts-node'
\   },
\   'typescript/ts-node': {
\     'command': 'ts-node',
\     'cmdopt': ' O "{\"strict\": true, \"target\": \"ES2021\"}"',
\     'exec': ['%c %o %s %a'],
\   },
\   'xdefaults': {
\     'command': 'mysql',
\     'exec': ['xrdb -remove', 'xrdb -merge %s', 'xrdb -query']
\   }
\ }

nmap <Leader>r  <Plug>(quickrun)
vmap <Leader>r  <Plug>(quickrun)




" ref  "{{{2

autocmd MyAutoCmd FileType ref
\ call s:on_FileType_ref()

function! s:on_FileType_ref() abort
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

nnoremap <silent> <Leader>a  :<C-u>call ref#jump('normal', 'alc')<CR>
vnoremap <silent> <Leader>a  :<C-u>call ref#jump('visual', 'alc')<CR>

AlterCommand ref  Ref


let g:ref_alc2_overwrite_alc = 1
let g:ref_no_default_key_mappings = 1
let g:ref_open = 'Split'
let g:ref_perldoc_complete_head = 1
let g:ref_phpmanual_path = '/usr/share/php-docs/en/php-chunked-xhtml'
let g:ref_wikipedia_lang = 'ja'




" scratch  "{{{2

nmap <Leader>s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\ call s:on_User_plugin_scratch_initialize_after()

function! s:on_User_plugin_scratch_initialize_after() abort
  map <buffer> <CR>  <Plug>(scratch-evaluate!)
endfunction


let g:scratch_show_command = 'SplitTop | hide buffer'




" skeleton  "{{{2

autocmd MyAutoCmd BufNewFile LICENSE
\ SkeletonLoad license-mit

autocmd MyAutoCmd User plugin-skeleton-detect
\ call s:on_User_plugin_skeleton_detect()

function! s:on_User_plugin_skeleton_detect() abort
  let _ = split(expand('%:p'), '/')
  if len(_) == 0
    return
  endif
  let filename = _[-1]
  let directories = _[:-2]

  if filename =~# '\.user\.js$'
    SkeletonLoad userjs
  elseif filename =~# '\.toml$'
    SkeletonLoad cargo
  elseif filename =~# '\.txt$' && get(directories, -1) ==# 'doc'
    SkeletonLoad vim-doc
  elseif filename =~# '\.vim$'
  \  && get(directories, -1) =~#
  \     '^\v(autoload|colors|compiler|ftdetect|ftplugin|indent|plugin|syntax)'
    if get(directories, -2) ==# 'after'
      execute 'SkeletonLoad' 'vim-additional-'.directories[-1]
    else
      execute 'SkeletonLoad' 'vim-'.directories[-1]
    endif
  endif
endfunction

autocmd MyAutoCmd User plugin-skeleton-loaded
\ call s:on_User_plugin_skeleton_loaded()

function! s:on_User_plugin_skeleton_loaded() abort
  silent %s/<%=\s*\(.\{-}\)\s*%>/\=eval(submatch(1))/ge
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




" smartinput  "{{{2

let g:smartinput_no_default_key_mappings = 1

if exists('g:loaded_smartinput')
  call smartinput#clear_rules()
  call smartinput#define_default_rules()
endif

call smartinput#define_rule({
\   'at': '\%#', 'char': '{', 'input': '{',
\   'syntax': ['Comment']
\ })
call smartinput#define_rule({
\   'at': '/\*\*\%#', 'char': '<CR>', 'input': '<CR><CR>/<Up><Space>',
\ })

" for PHP  "{{{
call smartinput#define_rule({
\   'at': '\%#', 'char': '@', 'input': '$this->',
\   'filetype': ['php']
\ })
call smartinput#define_rule({
\   'at': '\%#[$A-Za-z]', 'char': '@', 'input': '@',
\   'filetype': ['php']
\ })
call smartinput#define_rule({
\   'at': '\%#', 'char': '@', 'input': '@',
\   'filetype': ['php'],
\   'syntax': ['Comment', 'Constant', 'None']
\ })
" }}}

call smartinput#define_rule({
\   'at': '\%#', 'char': "'", 'input': "'",
\   'filetype': ['rust'],
\ })

call smartinput#map_trigger_keys()




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


call submode#enter_with('winsize', 'n', '', '<C-w><Space>',
\                       ':<C-u>call '.s:SID_PREFIX().'submode_winsize()<CR>')
call submode#enter_with('winsize', 'n', '', '<C-w><C-@>',
\                       ':<C-u>call '.s:SID_PREFIX().'submode_winsize()<CR>')
function! s:submode_winsize() abort
  let current = winnr()
  wincmd k | let above = winnr() | execute current 'wincmd w'
  wincmd j | let below = winnr() | execute current 'wincmd w'
  wincmd h | let left = winnr() | execute current 'wincmd w'
  wincmd l | let right = winnr() | execute current 'wincmd w'

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

" must be written at the last.  see :help 'secure'.
set secure




" __END__  "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
