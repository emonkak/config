" My vimrc
" Basic  "{{{1
" Absolute  "{{{2

let s:cygwin_p = has('win32unix')
let s:win_p = has('win32') || has('win64')


if has('vim_starting')
  " Don't reset twice on reloading - 'compatible' has so many side effects.
  set nocompatible  " to use many extensions of Vim.

  set fileformat=unix
  set shiftwidth=4
  set tabstop=4

  if s:win_p
    set shellslash
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
    set guifont=Envy\ Code\ M+:h10
  else
    set guifont=Monospace\ 10.5
  endif
  set guioptions=AcgM
  set linespace=0
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
set directory=~/tmp
set fileformats=unix,dos,mac
set grepprg=internal
set hidden
set history=100
if has('multi_byte_ime') || has('xim')
  set iminsert=0
  set imsearch=0
endif

set cmdheight=1
set completeopt=longest,menu
set display=lastline
set foldmethod=marker
set nohlsearch
set laststatus=2
set linebreak
set list
set listchars=tab:>-,extends:<,trail:-
set pumheight=20
set showcmd
set showtabline=2
set splitbelow
set splitright
set ttimeoutlen=50
set wildmenu
set nowrapscan

set autoindent
set cinoptions=:0,l1,g0,t0,(0,W1s
set formatoptions=tcroqnlM1
set ignorecase
set incsearch
set isfname=33-126,161-255
set smartcase
set smartindent

set title
set titlestring=Vim:\ %f\ %h%r%m
if exists('$TMUX')
  let &t_fs = "\<C-g>"
  let &t_ts = "\<Esc>]2;"
endif

let &statusline = ''
let &statusline .= '%<(%{(&filetype == "" ? "none" : &filetype)})'
let &statusline .= ' '
let &statusline .= '%f %h%m%r%w'
let &statusline .= '[%{(&fileencoding == "" ? &encoding : &fileencoding)}'
let &statusline .= ':%{&fileformat}]'
let &statusline .= '%{eskk#statusline()}'
let &statusline .= '%='
let &statusline .= '%-14.(%l,%c%V%) %P'

function! s:my_tabline()  "{{{
  let s = ''

  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let curbufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears

    let no = (i <= 10 ? i - 1 : '#')  " display 0-origin tabpagenr.
    let mod = len(filter(bufnrs, 'getbufvar(v:val, "&modified")')) ? '+' : ' '
    let title = fnamemodify(bufname(curbufnr), ':t')
    let title = len(title) ? title : '[No Name]'

    let s .= '%'.i.'T'
    let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
    let s .= no
    let s .= mod
    let s .= title
    let s .= '%#TabLineFill#'
    let s .= '  '
  endfor

  let s .= '%#TabLineFill#%T'
  let s .= '%=%#TabLine#'
  let s .= '| %<'
  let s .= exists('t:cwd') ? fnamemodify(t:cwd, ':p:~:h') : ''
  return s
endfunction "}}}
let &tabline = '%!' . s:SID_PREFIX() . 'my_tabline()'


let g:mapleader = ','
let g:maplocalleader = '.'


let g:html_use_css = 1
let g:html_no_pre = 1
let g:html_ignore_folding = 1
let g:html_number_lines = 0




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




" Source - wrapper of :source with echo.  "{{{2

command! -bar -complete=file -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>

AlterCommand so[urce]  Source




" SuspendWithAutomticCD  "{{{2

command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCd()
function! s:cmd_SuspendWithAutomticCd()
  if exists('$TMUX')  " tmux available.
    let _ = split(system('tmux list-windows'), '\n')
    let _ = map(filter(_, 'v:val =~# "^\\d\\+:"'),
    \           'split(v:val, "^\\d\\+\\zs:\\s\\|\\s\\[[0-9x]\\+]\\+$")')
    let shell = split(&shell, '/')[-1]
    let i = -1
    for [index, title] in _
      if title ==# shell
        let i = index
        break
      endif
    endfor
    silent execute '!tmux'
    \              (i > -1 ? 'select-window -t '.i : 'new-window').'\;'
    \              'send-keys C-u " cd \"'.getcwd().'\"" C-m'
    redraw!
  elseif exists('$WINDOW')  " GNU Screen available.
    " BUGS: When shell window does not exist, keys would be send to vim.
    let shell = split(&shell, '/')[-1]
    silent execute '!screen -X eval'
    \              '''select '.shell.''''
    \              '''stuff "\025 cd \\"'.getcwd().'\\" \015"'''
    redraw!
  else  " Not working terminal multiplexer.
    suspend
  endif
endfunction




" CD - wrapper of :cd to keep cwd for each tabpage  "{{{2

command! -complete=file -nargs=? CD
\   if empty(<q-args>) && len(expand('%'))
\ |   cd %:p:h
\ | else
\ |   cd <args>
\ | endif
\ | let t:cwd = getcwd()

AlterCommand cd  CD

autocmd MyAutoCmd TabEnter *
\   if exists('t:cwd')
\ |   cd `=t:cwd`
\ | endif

autocmd MyAutoCmd BufReadPost *
\   if !exists('t:cwd')
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

command! -bang -bar -complete=file -nargs=? Jis  Iso2022jp<bang> <args>
command! -bang -bar -complete=file -nargs=? Sjis  Cp932<bang> <args>




" Rename - rename the current file.  "{{{2

command! -nargs=1 -complete=file Rename  call s:cmd_Rename(<q-args>)
function! s:cmd_Rename(name)
  let current = expand('%')
  if !&modifiable || !filewritable(current)
    Hecho ErrorMsg 'This file can not be changes.'
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




" HelpTagsAll - :helptags to all doc/ in &runtimepath  "{{{2

command! -bar -nargs=0 HelpTagsAll  call s:cmd_HelpTagsAll()
function! s:cmd_HelpTagsAll()
  for path in split(&runtimepath, ',')
    let doc = path . '/doc'
    if isdirectory(doc) && filewritable(doc)
      helptags `=doc`
    endif
  endfor
endfunction




" Scouter - calculate vim power level.  "{{{2

command! -bar -bang -nargs=? -complete=file Scouter
\ echo s:cmd_Scouter(empty(<q-args>) ? $MYVIMRC : expand(<q-args>), <bang>0)
function! s:cmd_Scouter(file, banged_p)
  let pattern = '^\s*$\|^\s*"'
  let lines = readfile(a:file)
  if !a:banged_p
    let lines = split(substitute(join(lines, "\n"), '\n\s*\\', '', 'g'), "\n")
  endif
  return len(filter(lines,'v:val !~ pattern'))
endfunction




" Utilities  "{{{1
" :grep wrappers  "{{{2

command! -bar -complete=file -nargs=+ Grep  call s:grep('grep', [<f-args>])
command! -bar -complete=file -nargs=+ Lgrep  call s:grep('lgrep', [<f-args>])
function! s:grep(command, args)
  let target = len(a:args) > 1 ? join(a:args[:-2]) : '**/*'
  try
    execute a:command '/'.escape(a:args[-1], '/').'/j' target
  catch /.*/
    redraw
    Hecho ErrorMsg v:exception
  endtry
  if a:command ==# 'grep'
    if len(getqflist())
      copen
    endif
  elseif len(getloclist())  " lgrep
    lopen
  endif
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  Lgrep




" :make wrappers  "{{{2

command! -bar -complete=file -nargs=* Make  call s:make('make', [<f-args>])
command! -bar -complete=file -nargs=* Lmake  call s:make('lmake', [<f-args>])
function! s:make(command, args)
  let window = winnr()
  execute a:command.'!' join(a:args)
  if a:command ==# 'make'
    cwindow
  else  " lmake
    lwindow
  endif
  execute window 'wincmd w'
endfunction

AlterCommand mak[e]  Make
AlterCommand lmak[e]  Lmake




" Indent style  {{{2

command! -bar -complete=customlist,s:complete_indent_width -nargs=1 IndentTab
\ setlocal noexpandtab tabstop=<args> shiftwidth=<args> softtabstop&
command! -bar -complete=customlist,s:complete_indent_width -nargs=1 IndentSpace
\ setlocal expandtab tabstop& shiftwidth=<args> softtabstop=<args>

function! s:complete_indent_width(arglead, cmdline, cursorpos)
  return ['2', '4', '8']
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
  endif
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




" Split nicely with  "{{{2

command! -bar -bang -nargs=* -complete=file Split
\ call s:split_nicely_with(['split', <f-args>], <bang>0)
command! -bar -bang -nargs=* -complete=help Help
\ call s:split_nicely_with(['help', <f-args>], <bang>0)
command! -bar -bang -nargs=* -complete=file New
\ call s:split_nicely_with(['new', <f-args>], <bang>0)

function! s:vertically()
    return winwidth(0) * 2 > winheight(0) * 8
endfunction
function! s:split_nicely_with(args, banged_p)
  execute s:vertically() ? 'vertical' : ''
  \       a:args[0] . (a:banged_p ? '!' : '')
  \       join(a:args[1:])
endfunction


AlterCommand sp[lit] Split
AlterCommand h[elp]  Help
AlterCommand new  New




function! s:get_region()  "{{{2
  let [_, line1, col1, _] = getpos("'<")
  let [_, line2, col2, _] = getpos("'>")

  let region = getline(line1, line2)
  if line1 == line2  " single line
    let region[0] = strpart(region[-1], col1 - 1, col2 - (col1 - 1))
  else  " multi line
    let region[0] = strpart(region[0], col1 - 1)
    let region[-1] = strpart(region[-1], 0, col2)
  endif
  if visualmode() ==# 'V'
    let region += ['']
  endif

  return region  " return [] of string.
endfunction




function! s:keys_to_complete()  "{{{2
  if len(&l:completefunc)
    return "\<C-x>\<C-u>"
  elseif len(&l:omnifunc)
    return "\<C-x>\<C-o>"
  else
    return "\<C-n>"
  endif
endfunction




function! s:toggle_option(option_name)  "{{{2
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
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
nnoremap [Tag]<Space>  :<C-u>tag<Space>


" With the preview window  "{{{3

nnoremap [Tag]'t  <C-w>}
vnoremap [Tag]'t  <C-w>}
nnoremap <silent> [Tag]'n  :ptnext<CR>
nnoremap <silent> [Tag]'p  :ptprevious<CR>
nnoremap <silent> [Tag]'P  :<C-u>ptfirst<CR>
nnoremap <silent> [Tag]'N  :<C-u>ptlast<CR>
nnoremap <silent> [Tag]'c  :<C-u>pclose<CR>


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
\ call s:init_cmdwin()

function! s:init_cmdwin()
  nnoremap <buffer> <Esc><Esc>  <C-c><C-c>
  inoremap <buffer> <Esc><Esc>  <C-c><C-c>
  inoremap <buffer> <expr> <C-c>  pumvisible() ? "\<C-e>" : "\<C-c>"
  inoremap <buffer> <expr> <Tab>  pumvisible() ? "\<C-n>" : <SID>keys_to_complete()
  inoremap <buffer> <expr> <S-Tab>  pumvisible() ? "\<C-p>" : <SID>keys_to_complete()
  startinsert!
endfunction




" Insert mode  "{{{2

inoremap <C-b>  <Left>
inoremap <C-f>  <Right>
inoremap <C-a>  <Home>
inoremap <expr> <C-e>  pumvisible() ? "\<C-e>" : "\<End>"
inoremap <C-d>  <Delete>

" Alternative keys for the original action.
inoremap <C-\>  <C-a>
inoremap <C-q>  <C-d>


" Emacs like kill-line.
" BUGS: Can't use <C-x><X-k> (keyword completion).
inoremap <expr> <C-k>  col('.') == col('$') ? "\<C-o>gJ" : "\<C-o>D"


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


nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>

nnoremap <silent> [Space]?  :<C-u>call <SID>close_help_window()<CR>

nnoremap [Space]o  <Nop>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>

nnoremap [Space]f  <Nop>
nnoremap [Space]fe  :<C-u>set fileencoding=
nnoremap [Space]ff  :<C-u>set fileformat=
nnoremap [Space]ft  :<C-u>set filetype=

nnoremap <silent> [Space]q  :<C-u>Help quickref<CR>

nnoremap <silent> [Space]m  :<C-u>marks<CR>
nnoremap <silent> [Space]r  :<C-u>registers<CR>

nnoremap <silent> [Space]d  :<C-u>bdelete<CR>
nnoremap <silent> [Space]D  :<C-u>bdelete!<CR>

nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>

nmap [Space]s  <Plug>(operator-my-sort)
vmap [Space]s  <Plug>(operator-my-sort)


" Open a fold.
nnoremap [Space]l  zo

" Close a fold.
nnoremap [Space]h  zc

" Close all folds but including the cursor.
nnoremap [Space]v  zMzv

" Yank to the clipboard.
nnoremap [Space]y  "+y
vnoremap [Space]y  "+y
nnoremap [Space]Y  "+Y
vnoremap [Space]Y  "+Y

" Put the clipboard.
nnoremap [Space]p  "+p
vnoremap [Space]p  "+p
nnoremap [Space]P  "+P
vnoremap [Space]P  "+P




" Section jumping  "{{{2

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
nnoremap <Tab> <C-w>w
nnoremap <S-Tab> <C-w>W


" Search the word nearest to the cursor in new window.
nnoremap <C-w>*  <C-w>s*
nnoremap <C-w>#  <C-w>s#


" Like "<C-w>q", but does ":quit!".
nnoremap <C-w>Q  :<C-u>quit!<CR>


nnoremap <silent> <C-w>y  :<C-u>Split<CR>




" Operators  "{{{2

" User key mappings will be defined later - see [Space].
call operator#user#define_ex_command('my-sort', 'sort')




" Misc.  "{{{2

nnoremap <silent> <Leader><Leader>  :<C-u>update<CR>

nnoremap <C-h>  :<C-u>Help<Space>
nnoremap <C-o>  :<C-u>edit<Space>


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
vnoremap *  :<C-u>call <SID>search_the_selected_text_literaly('n')<CR>
vnoremap #  :<C-u>call <SID>search_the_selected_text_literaly('N')<CR>

function! s:search_the_selected_text_literaly(search_command)
  let region = join(map(s:get_region(), 'escape(v:val, "\\/")'), '\n')

  let @/ = '\V' . region
  call histadd('/', '\V' . region)
  execute 'normal!' a:search_command

  let v:searchforward = a:search_command ==# 'n'
endfunction


noremap <C-z>  <Nop>
nnoremap <C-z>  :<C-u>SuspendWithAutomticCD<CR>


" Show the lines which match to the last search pattern.
nnoremap <count> g/  :global//print<CR>
vnoremap <count> g/  :global//print<CR>


" Show the name of the syntax item under the cursor.
nnoremap gs  :<C-u>echo synIDattr(synID(line("."), col("."), 1), "name")<CR>


" Experimental: alternative <Esc>
noremap <C-@>  <Esc>
noremap! <C-@>  <Esc>

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
  " Load the dictionary for filetype.
  let dict = expand('~/.vim/dict/').&filetype.'.dict'
  if filereadable(dict)
    let &l:dictionary = dict
  endif

  " Make omni completion available for all filetypes.
  if &l:omnifunc == ''
    setlocal omnifunc=syntaxcomplete#Complete
  endif
endfunction


" Fix 'fileencoding' to use 'encoding'.
autocmd MyAutoCmd BufReadPost *
\   if &modifiable && !search('[^\x00-\x7F]', 'cnw')
\ |   setlocal fileencoding=
\ | endif

" When editing a file, always jump to the last cursor position.
autocmd MyAutoCmd BufReadPost *
\   if line('''"') > 0 && line ('''"') <= line('$')
\ |   execute 'normal g''"'
\ | endif




" changelog  "{{{2

" Fix the new entry mapping bug.
autocmd MyAutoCmd FileType changelog
\ noremap <buffer> <silent> <Leader>o  :<C-u>NewChangelogEntry<CR>

let g:changelog_timeformat = "%Y-%m-%d"
let g:changelog_username  = s:win_p ? $USERNAME : $USER




" css  "{{{2

autocmd MyAutoCmd FileType css
\ IndentSpace 2




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
  \               . '(getline(v:lnum) =~# ''^;.*\%(__END__\|\*\*\*\)'' ? 0 : "=")'
endfunction




" haskell  "{{{2

autocmd MyAutoCmd FileType haskell
\   IndentSpace 2
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




" lisp  "{{{2

autocmd MyAutoCmd FileType lisp
\ IndentSpace 2




" lua  "{{{2

autocmd MyAutoCmd FileType lua
\ IndentSpace 2




" perl  "{{{2

autocmd MyAutoCmd FileType perl
\ IndentSpace 2




" python  "{{{2

autocmd MyAutoCmd FileType python
\   IndentSpace 2
\ | let python_highlight_numbers = 1
\ | let python_highlight_builtins = 1
\ | let python_highlight_space_errors = 1




" ruby  "{{{2

autocmd MyAutoCmd FileType ruby
\ IndentSpace 2




" scheme  "{{{2

autocmd MyAutoCmd FileType scheme
\ IndentSpace 2

let g:is_gauche = 1




" sh, zsh  "{{{2

autocmd MyAutoCmd FileType sh,zsh
\ IndentSpace 2

let g:is_bash = 1




" tex  "{{{2

autocmd MyAutoCmd FileType tex,plaintex
\   call s:on_FileType_tex()
\ | compiler tex

function! s:on_FileType_tex()
  IndentSpace 2
  setlocal foldmarker=%{{{,%}}}
  nnoremap <buffer> <silent> <LocalLeader>p  :<C-u>!dvipdfmx %:p:r.dvi<CR>
endfunction




" vim  "{{{2

autocmd MyAutoCmd FileType vim
\ call s:on_FileType_vim()

function! s:on_FileType_vim()
  IndentSpace 2
  nnoremap <buffer> <silent> K  :<C-u>Help <C-r><C-w><CR>
  vnoremap <buffer> <silent> K  :<C-u>Help <C-r>=join(<SID>get_region())<CR><CR>
endfunction

let g:vim_indent_cont = 0




" xml  "{{{2

autocmd MyAutoCmd FileType html,xhtml,xml,xslt
\ call s:on_FileType_xml()

function! s:on_FileType_xml()
  IndentSpace 2
  inoremap <buffer> </  </<C-x><C-o>
endfunction




" Plugins  "{{{1
" eskk  "{{{2

let g:eskk_dictionary = {
\  'path': expand('~/.skk-eskk-jisyo'),
\  'sorted': 0,
\  'encoding': 'utf-8',
\ }
let g:eskk_large_dictionary = {
\  'path': (s:win_p || s:cygwin_p)
\        ? expand('~/.skkime/SKK-JISYO.L') : '/usr/share/skk/SKK-JISYO.L',
\  'sorted': 1,
\  'encoding': 'euc-jp',
\ }

let g:eskk_directory = expand('~/.vim/info/eskk')
let g:eskk_egg_like_newline = 1
let g:eskk_enable_completion = 0
let g:eskk_show_annotation = 1
let g:eskk_use_color_cursor = 0




" exjumplist  "{{{2

" <C-j>/<C-k> for consistency with my UI key mappings on jumplist.
nmap <Esc><C-j>  <Plug>(exjumplist-next-buffer)
nmap <Esc><C-k>  <Plug>(exjumplist-previous-buffer)




" grex  "{{{2

nmap gy  <Plug>(operator-grex-yank)<Plug>(textobj-entire-a)
vmap gy  <Plug>(operator-grex-yank)
nmap gd  <Plug>(operator-grex-delete)<Plug>(textobj-entire-a)
vmap gd  <Plug>(operator-grex-delete)




" ku  "{{{2

autocmd MyAutoCmd FileType ku
\ call s:ku_my_settings()

function! s:ku_my_settings()
  call ku#default_key_mappings(s:TRUE)
  imap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
  nmap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
endfunction


call ku#custom_action('common', 'cd', s:SID_PREFIX().'ku_common_action_my_cd')
call ku#custom_action('common', 'Yank', s:SID_PREFIX().'ku_common_action_Yank')
call ku#custom_action('common', 'yank', s:SID_PREFIX().'ku_common_action_yank')
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


call ku#custom_prefix('common', '.vim', expand('~/.vim'))
call ku#custom_prefix('common', 'HOME', expand('~'))
call ku#custom_prefix('common', 'VIM', expand('$VIMRUNTIME'))
call ku#custom_prefix('common', '~', expand('~'))


nmap [Space]k  <Nop>
nnoremap <silent> [Space]ka  :<C-u>Ku args<CR>
nnoremap <silent> [Space]kb  :<C-u>Ku buffer<CR>
nnoremap <silent> [Space]kf  :<C-u>Ku file<CR>
nnoremap <silent> [Space]kh  :<C-u>Ku history<CR>
nnoremap <silent> [Space]kk  :<C-u>call ku#restart()<CR>
nnoremap <silent> [Space]kq  :<C-u>Ku quickfix<CR>
nnoremap <silent> [Space]ks  :<C-u>Ku source<CR>
nnoremap <silent> [Space]kw  :<C-u>Ku myproject<CR>

nnoremap <silent> [Space]kg  :<C-u>Ku metarw/git<CR>

nnoremap <silent> [Space]k/  :<C-u>Ku cmd_mru/search<CR>
nnoremap <silent> [Space]k:  :<C-u>Ku cmd_mru/cmd<CR>
nnoremap <silent> [Space]km  :<C-u>Ku file_mru<CR>


let g:ku_personal_runtime = expand('~/.vim')
let g:ku_file_mru_file = expand('~/.vim/info/ku/')
\                      . (s:win_p ? 'mru_win' : 'mru')
let g:ku_file_mru_ignore_pattern = '/$\|^//\|^/cygdrive/\|^/mnt/\|^/media/'




" narrow  "{{{2

noremap [Space]x  <Nop>
noremap <silent> [Space]xn  :Narrow<CR>
noremap <silent> [Space]xw  :<C-u>Widen<CR>




" neocomplcache  "{{{2

imap <C-l>  <Plug>(neocomplcache_snippets_expand)
smap <C-l>  <Plug>(neocomplcache_snippets_expand)

let g:neocomplcache_disable_auto_complete = 1
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_auto_select = 0
let g:neocomplcache_enable_smart_case = 1

let g:neocomplcache_auto_completion_start_length = 2
let g:neocomplcache_manual_completion_start_length = 2

let g:neocomplcache_lock_buffer_name_pattern = '\*neoui\*\|\[neoui\]'
let g:neocomplcache_temporary_dir = expand('~/.vim/info/neocomplcache')





" operator-replece  "{{{2

map _  <Plug>(operator-replace)




" quickrun  "{{{2

let g:loaded_quicklaunch = 1
let g:quickrun_config = {
\  '*': {
\    'split': '{'.s:SID_PREFIX().'vertically() ? "vertical" : "" }',
\  },
\  'javaapplet': {
\    'exec': ['echo "<applet code=%s:t:r width=500 height=500></applet>" > %s:p:r.html',
\             'appletviewer %s:p:r.html'],
\  },
\  'markdown': {
\    'exec': ['markdown.pl %s | tee %s:p:r.html'],
\  },
\  'tex': {
\    'command': executable('pxdvi') ? 'pxdvi' :
\               executable('xdvi') ? 'xdvi' : '',
\    'exec': ['platex -kanji=utf8 -interaction=nonstopmode -output-directory=%s:p:h %s',
\             '%c %s:p:r.dvi'],
\  },
\  'xdefaults': {
\    'exec': ['xrdb -remove', 'xrdb -merge %s', 'xrdb -query'],
\  },
\ }





" ref  "{{{2

autocmd MyAutoCmd FileType ref
\ call s:ref_my_settings()

function! s:ref_my_settings()
  nmap <buffer> <C-]>  <Plug>(ref-keyword)
  nmap <buffer> <C-j>  <Plug>(ref-forward)
  nmap <buffer> <C-k>  <Plug>(ref-back)
  nnoremap <buffer> q  <C-w>c
endfunction

nnoremap <silent> <Leader>t  :<C-u>call ref#jump('normal', 'alc')<CR>
vnoremap <silent> <Leader>t  :<C-u>call ref#jump('visual', 'alc')<CR>

AlterCommand ref  Ref


let g:ref_cache_dir = expand('~/.vim/info/ref')
let g:ref_no_default_key_mappings = 1
let g:ref_open = 'Split'
let g:ref_perldoc_complete_head = 1



" scratch  "{{{2

nmap <Leader>s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\ call s:scratch_my_settings()

function! s:scratch_my_settings()
  map <buffer> <C-m>  <Plug>(scratch-evaluate)
  map <buffer> q  <Plug>(scratch-close)
endfunction


let g:scratch_show_command = 'Split | hide buffer'




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
\                       ':<C-u>call '.s:SID_PREFIX().'submode_resize_window()<CR>')

function! s:submode_resize_window()
  let current = winnr()
  wincmd j | let target1 = winnr() | execute current "wincmd w"
  wincmd l | let target2 = winnr() | execute current "wincmd w"

  execute printf('call submode#map("winsize", "n", "r", "j", "<C-w>%s")',
  \ current == target1 ? "-" : "+")
  execute printf('call submode#map("winsize", "n", "r", "k", "<C-w>%s")',
  \ current == target1 ? "+" : "-")
  execute printf('call submode#map("winsize", "n", "r", "h", "<C-w>%s")',
  \ current == target2 ? ">" : "<")
  execute printf('call submode#map("winsize", "n", "r", "l", "<C-w>%s")',
  \ current == target2 ? "<" : ">")
endfunction


let g:submode_timeout = 0




" surround  "{{{2

" The default mapping ys for <Plug>Ysurround is not consistent with
" the default mappings of vi -- y is for yank.
nmap s  <Plug>Ysurround
nmap ss  <Plug>Yssurround




" template  "{{{2

autocmd MyAutoCmd User plugin-template-loaded
\   silent %s/<%=\(.\{-}\)%>/\=eval(submatch(1))/ge
\ | if search('<%|%>')
\ |   execute 'normal! "_da>'
\ | endif




" vcsi  "{{{2

let g:vcsi_open_command = 'Split | enew'
let g:vcsi_diff_in_commit_buffer_p = 1
let g:vcsi_use_native_message_p = 1




" vimfiler  "{{{2

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
let g:vimfiler_split_command = 'Split'
let g:vimfiler_trashbox_directory = expand('~/.trash')




" vimshell  "{{{2

autocmd MyAutoCmd FileType vimshell
\   call vimshell#altercmd#define('ls', 'ls -F')
\ | call vimshell#altercmd#define('la', 'ls -Fa')
\ | call vimshell#altercmd#define('ll', 'ls -Fl')
\ | call vimshell#altercmd#define('lla', 'ls -Fla')
\ | call vimshell#altercmd#define('grep', 'grep -E')

map <C-@>  <Plug>(vimshell_split_switch)

nnoremap !  :<C-u>VimShellInteractive<Space>
nnoremap &  :<C-u>VimShellExecute<Space>


let g:vimshell_user_prompt = ''
let g:vimshell_user_prompt .= s:win_p ? '$USERNAME' : '$USER'
let g:vimshell_user_prompt .= '."@"'
let g:vimshell_user_prompt .= '.hostname()'
let g:vimshell_user_prompt .= '." "'
let g:vimshell_user_prompt .= '.fnamemodify(getcwd(), ":p:~:h")'
let g:vimshell_user_prompt .= '." "'
let g:vimshell_user_prompt .= '.vimshell#vcs#info("[%s:%b]", "[%s:%b|%a]")'
let g:vimshell_prompt = 'YUKI.N' . ($USER ==# 'root' ? '# ' : '> ')

let g:vimshell_enable_smart_case = 1
let g:vimshell_ignore_case = 0

let g:vimshell_split_command = 'Split'
let g:vimshell_temporary_directory = expand('~/.vim/info/vimshell')




" Fin.  "{{{1

" must be written at the last.  see :help 'secure'.
set secure




" __END__  "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
