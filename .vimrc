" My vimrc
" Basic  "{{{1
" Absolute  "{{{2

if has('vim_starting')
  " Don't reset twice on reloading - 'compatible' has so many side effects.
  set nocompatible  " to use many extensions of Vim.

  if has('win32') || has('win64')
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




" Options  "{{{2

if has('gui_running')
  set guicursor=a:blinkon0
  if has('gui_gtk2')
    set guifont=Monospace\ 10.5
  else
    set guifont=Consolas:h10.5
  endif
  set guioptions=cgM
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
if has('clientserver')
  set clipboard=unnamed,exclude:cons\|linux
endif
set diffopt=filler,vertical
set directory=~/tmp
set fileformats=unix,dos,mac
set grepprg=internal
set hidden
set history=100
set nobackup
if has('multi_byte_ime') || has('xim')
  set iminsert=0
  set imsearch=0
endif
if exists('+shellslash')
  set shellslash
endif

set cmdheight=1
set completeopt=longest,menu
set display=lastline
set foldmethod=marker
set laststatus=2
set linebreak
set list
set listchars=tab:>\ ,extends:<,trail:-
set nohlsearch
set nowrapscan
set pumheight=20
set showcmd
set splitbelow
set splitright
set ttimeoutlen=50
set wildmenu

set autoindent
set cinoptions=:0,l1,g0,t0,(0,j1
if &expandtab == 0
  set shiftwidth=4
  set tabstop=4
endif
set formatlistpat&
let &formatlistpat .= '\|^\s*[*+-]\s*'
set formatoptions=tcroqnlM1
set ignorecase
set incsearch
set smartcase
set smartindent

if exists('$TMUX')
  let &t_fs = "\<C-g>"
  let &t_ts = "\<Esc>]2;"
endif
set title
set titlestring=Vim:\ %f\ %h%r%m

let &statusline = ''
let &statusline .= '(%{(&filetype == "" ? "none" : &filetype)})'
let &statusline .= ' %<%f %h%m%r%w'
let &statusline .= '[%{(&l:fileencoding == "" ? &encoding : &l:fileencoding).":".&fileformat}]'
let &statusline .= '%{eskk#get_stl()}'
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
call pathogen#helptags()




" Syntax  {{{1
" Stuffs  "{{{2

let s:FALSE = 0
let s:TRUE = !s:FALSE

let s:cygwin_p = has('win32unix')
let s:win_p = has('win32') || has('win64')




" Source - wrapper of :source with echo.  "{{{2

command! -bar -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>

AlterCommand so[urce]  Source




" SuspendWithAutomticCD  "{{{2

if !exists('s:TMUX_AVAILABLE_P')
  let s:TMUX_AVAILABLE_P = exists('$TMUX')
endif

command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCD()

function! s:cmd_SuspendWithAutomticCD()
  if s:TMUX_AVAILABLE_P
    let windows = split(system('tmux list-windows'), ':\s\|\s\S\+\n')
    let index = index(windows, split(&shell, '/')[-1])
    silent execute '!tmux'
    \              (index > -1 ? 'select-window -t '.windows[index - 1] : 'new-window') '\;'
    \              'send-keys C-u " cd' fnameescape(getcwd()) '" C-m'
    redraw!
    let s:TMUX_AVAILABLE_P = (v:shell_error == 0)
  else
    suspend
  endif
endfunction




" CD - wrapper of :cd to keep cwd for each tabpage  "{{{2

command! -complete=file -nargs=? CD
\ call s:tabpage_cd(<q-args>)

function! s:tabpage_cd(directory)
  if len(a:directory)
    let target = fnameescape(a:directory)
  else
    let target = len(expand('%')) ? fnameescape(expand('%:p:h')) : ''
  endif
  execute 'cd' target
  let t:cwd = getcwd()
  echo t:cwd
endfunction

AlterCommand cd  CD


autocmd MyAutoCmd TabEnter *
\   if exists('t:cwd')
\ |   execute 'cd' fnameescape(t:cwd)
\ | endif

autocmd MyAutoCmd BufReadPost *
\   if !exists('t:cwd')
\ |   execute 'cd' fnameescape(expand('%:p:h'))
\ |   let t:cwd = getcwd()
\ | endif




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




" Utilities  "{{{1
" :grep wrappers  "{{{2

command! -bar -complete=file -nargs=+ Grep
\ call s:grep('grep', [<f-args>])
command! -bar -complete=file -nargs=+ Lgrep
\ call s:grep('lgrep', [<f-args>])

function! s:grep(command, args)
  let target = len(a:args) > 1 ? join(a:args[:-2]) : '**/*'
  execute a:command '/'.a:args[-1].'/j' target
  if len(getqflist()) != 0
    execute stridx(a:command, 'l') == 0 ? 'lopen' : 'copen'
  endif
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  Lgrep




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




" Split nicely  "{{{2

command! -bar -bang -nargs=* -complete=file Split
\ call s:split_nicely_with(['split', <f-args>], <bang>0)
command! -bar -bang -nargs=* -complete=help Help
\ call s:split_nicely_with(['help', <f-args>], <bang>0)
command! -bar -bang -nargs=* -complete=file New
\ call s:split_nicely_with(['new', <f-args>], <bang>0)

function! s:vertically()
    return winwidth(0) * 2 > winheight(0) * 8
endfunction

function! s:split_nicely_with(args, banged)
  execute s:vertically() ? 'vertical' : ''
  \       a:args[0] . (a:banged ? '!' : '')
  \       join(a:args[1:])
endfunction


AlterCommand sp[lit] Split
AlterCommand h[elp]  Help
AlterCommand new  New




" Rename  "{{{2

command! -nargs=1 -complete=file Rename
\ call s:rename(<q-args>)

function! s:rename(name)
  let current = expand('%:p')
  if !filereadable(a:name) && filewritable(current)
    execute 'file' a:name
    write
    call delete(current)
  endif
endfunction




function! s:get_region()  "{{{2
  let [_, start_line, start_col, _] = getpos("'<")
  let [_, end_line, end_col, _] = getpos("'>")

  let region = getline(start_line, end_line)
  if start_line == end_line  " single line
    let region[0] = strpart(region[-1], start_col - 1, end_col - (start_col - 1))
  else  " multi line
    let region[0] = strpart(region[0], start_col - 1)
    let region[-1] = strpart(region[-1], 0, end_col)
  endif
  if visualmode() ==# 'V'
    let region += ['']
  endif

  return region
endfunction




function! s:set_short_indent()  "{{{2
  setlocal expandtab softtabstop=2 shiftwidth=2
endfunction




function! s:toggle_option(option_name)  "{{{2
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
endfunction




" Mappings  "{{{1
" QuickFix  "{{{2
" Fallback  "{{{3

" The prefix key.
nmap q  [Quickfix]

" fallback
noremap [Quickfix] <Nop>

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
nnoremap <silent> [Quickfix]m  :<C-u>make<CR>
nnoremap [Quickfix]M  :<C-u>make<Space>
nnoremap [Quickfix]<Space>  :<C-u>make<Space>
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
nnoremap <silent> [Quickfix]wm  :<C-u>lmake<CR>
nnoremap [Quickfix]wM  :<C-u>lmake<Space>
nnoremap [Quickfix]w<Space>  :<C-u>lmake<Space>
nnoremap [Quickfix]wg  :<C-u>Lgrep<Space>




" Tab pages  "{{{2
" Fallback  "{{{3

" the prefix key.
nmap <C-t>  [Tabbed]

" fallback
noremap [Tabbed] <Nop>




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
cnoremap <C-k>  <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>




" Insert mode  "{{{2

inoremap <C-b>  <Left>
inoremap <C-f>  <Right>
inoremap <C-a>  <C-o>^
inoremap <C-e>  <End>
inoremap <C-d>  <Delete>

" Emacs like kill-line.
inoremap <expr> <C-k>  (col('.') == col('$') ? '<C-o>gJ' : '<C-o>D')

" Alternative key for the original action.
inoremap <C-q>  <C-d>
inoremap <C-\>  <C-a>


" To be able to undo these types of deletion.
inoremap <C-w>  <C-g>u<C-w>
inoremap <C-u>  <C-g>u<C-u>


" Complete or indent.
inoremap <expr> <Tab>  pumvisible()
                   \ ? '<C-n>'
                   \ : <SID>should_indent_rather_than_complete_p()
                   \ ? '<C-i>'
                   \ : <SID>keys_to_complete()
inoremap <expr> <S-Tab>  pumvisible()
                     \ ? '<C-p>'
                     \ : <SID>should_indent_rather_than_complete_p()
                     \ ? '<C-i>'
                     \ : <SID>keys_to_complete()

function! s:should_indent_rather_than_complete_p()
  let m = match(getline('.'), '\S')
  return m == -1 || col('.') - 1 <= m
endfunction

function! s:keys_to_complete()
  if len(&l:completefunc)
    return "\<C-x>\<C-u>"
  elseif len(&l:omnifunc)
    return "\<C-x>\<C-o>"
  else
    return "\<C-n>"
  endif
endfunction




" The <Space>  "{{{2

" to show <Space> in the bottom line.
map <Space> [Space]

" fallback
noremap [Space] <Nop>


" Toggle option.
nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>

" Set file option.
nnoremap [Space]fe  :<C-u>set fileencoding=
nnoremap [Space]ff  :<C-u>set fileformat=
nnoremap [Space]ft  :<C-u>set filetype=

" Change tab style.
nnoremap <silent> [Space]tt :<C-u>call <SID>toggle_option('expandtab')<CR>
nnoremap <silent> [Space]t2 :<C-u>setlocal shiftwidth=2 softtabstop=2<CR>
nnoremap <silent> [Space]t4 :<C-u>setlocal shiftwidth=4 softtabstop=4<CR>
nnoremap <silent> [Space]t8 :<C-u>setlocal shiftwidth=8 softtabstop=8<CR>


" Easily check registers and marks.
nnoremap <silent> [Space]m  :<C-u>marks<CR>
nnoremap <silent> [Space]r  :<C-u>registers<CR>

" Open quick reference guide.
nnoremap <silent> [Space]q  :<C-u>Help quickref<CR>

" Change current directory.
nnoremap <silent> [Space]cd  :<C-u>CD<CR>

" Reload .vimrc
nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>


" Open a fold.
nnoremap [Space]l  zo

" Close a fold.
nnoremap [Space]h  zc

" Close all folds but including the cursor.
nnoremap [Space]v  zMzv


" Sort operator.
nmap [Space]s  <Plug>(operator-my-sort)
vmap [Space]s  <Plug>(operator-my-sort)




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
nnoremap <C-w>.  :<C-u>edit .<CR>
nnoremap <C-z>  :<C-u>SuspendWithAutomticCD<CR>


" Move cursor by display lines when wrapping.
noremap j  gj
noremap k  gk
noremap gj  j
noremap gk  k

" Delete a character with the black hole register.
nnoremap X "_X
nnoremap x "_x

" "Y" to work from the cursor to the end of line.
nnoremap Y y$


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
vnoremap *  :<C-u>call <SID>search_selected_text('n')<CR>
vnoremap #  :<C-u>call <SID>search_selected_text('N')<CR>

function! s:search_selected_text(search_command)
  let region = join(map(s:get_region(), 'escape(v:val, "\/")'), '\n')

  let @/ = '\V' . region
  call histadd('/', '\V' . region)
  execute 'normal!' a:search_command

  let v:searchforward = a:search_command ==# 'n'
endfunction


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
  let dict = expand('~/.vim/dict/').&l:filetype.'.dict'
  if filereadable(dict)
    let &l:dictionary = dict
  endif

  " Make omni completion available for all filetypes.
  if &l:omnifunc == ''
    setlocal omnifunc=syntaxcomplete#Complete
  endif
endfunction


" Fix 'fileencoding' to use 'encoding'
autocmd MyAutoCmd BufReadPost *
\   if &modifiable && !search('[^\x00-\x7F]', 'cnw')
\ |   setlocal fileencoding=
\ | endif




" changelog  "{{{2

" Fix the new entry mapping.
autocmd MyAutoCmd FileType changelog
\ noremap <buffer> <silent> <Leader>o  :<C-u>NewChangelogEntry<CR>




" css  "{{{2

autocmd MyAutoCmd FileType css
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
\ call s:set_short_indent()




" java  "{{{2

autocmd MyAutoCmd FileType java
\ call s:on_FileType_java()

function! s:on_FileType_java()
  nnoremap <silent> <LocalLeader>a  :<C-u>QuickRun javaapplet -mode n<CR>
  vnoremap <silent> <LocalLeader>a  :<C-u>QuickRun javaapplet -mode v<CR>

  setlocal cinoptions=:0,l1,g0,t0,(0,j1
  let &l:makeprg = 'javac -Xlint:unchecked -Xlint:deprecation %'
  let &l:errorformat = '%E%f:%l: %m,%C%\S%\+: %.%# %m,%Z%p^,%C%.%#'
endfunction




" lua  "{{{2

autocmd MyAutoCmd FileType lua
\ call s:set_short_indent()




" perl  "{{{2

autocmd MyAutoCmd FileType perl
\   compiler perl
\ | call s:set_short_indent()




" python  "{{{2

autocmd MyAutoCmd FileType python
\   call s:set_short_indent()
\ | let python_highlight_numbers = 1
\ | let python_highlight_builtins = 1
\ | let python_highlight_space_errors = 1




" registry  "{{{2

autocmd MyAutoCmd FileType registry
\ call s:on_FileType_registry()

function! s:on_FileType_registry()
  " Fix the default syntax to properly highlight.
  syntax match registryHead  "Windows Registry Editor Version \d\+\.\d\+"

  syntax clear registryString
  syntax match registryString  "\".\{-}\"" contains=registryGUID,registrySpecial

  syntax clear registrySubKey
  syntax match registrySubKey  "^\".\{-}\"="
  syntax match registrySubKey  "^@="

  highlight clear registryHead
  highlight link registryHead  PreProc
endfunction




" ruby  "{{{2

autocmd MyAutoCmd FileType ruby
\ call s:set_short_indent()




" scheme  "{{{2

autocmd MyAutoCmd FileType scheme
\ call s:set_short_indent()

let g:is_gauche = 1




" sh, zsh  "{{{2

autocmd MyAutoCmd FileType sh,zsh
\ call s:set_short_indent()

let g:is_bash = 1




" tex  "{{{2

autocmd MyAutoCmd FileType tex,plaintex
\   call s:set_short_indent()
\ | setlocal foldmarker=%{{{,%}}}
\ | let &l:makeprg = 'platex --kanji=utf8 -shell-escape -file-line-error -interaction=nonstopmode % && dvipdfmx %:p:r.dvi'
\ | let &l:errorformat = '%f:%l: %m'


let g:tex_flavor = "latex"




" vim  "{{{2

autocmd MyAutoCmd FileType vim
\   call s:set_short_indent()
\ | nnoremap <buffer> <silent> K  :<C-u>Help <C-r><C-w><CR>
\ | vnoremap <buffer> <silent> K  :<C-u>Help <C-r>=<SID>get_region()[0]<CR><CR>

autocmd MyAutoCmd FileType help
\   nnoremap <buffer> <silent> K  :<C-u>Help <C-r><C-w><CR>
\ | vnoremap <buffer> <silent> K  :<C-u>Help <C-r>=<SID>get_region()[0]<CR><CR>
\ | nnoremap <buffer> q  <C-w>c


let g:vim_indent_cont = 0




" xml  "{{{2

autocmd MyAutoCmd FileType html,xhtml,xml,xslt
\ call s:on_FileType_xml()

function! s:on_FileType_xml()
  call s:set_short_indent()
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
\  'path': (s:win_p || s:cygwin_p) ? expand('~/.skkime/SKK-JISYO.L') : '/usr/share/skk/SKK-JISYO.L',
\  'sorted': 1,
\  'encoding': 'euc-jp',
\ }

let g:eskk_egg_like_newline = 1
let g:eskk_show_annotation = 1




" exjumplist  "{{{2

" <C-j>/<C-k> for consistency with my UI key mappings on jumplist.
nmap <Esc><C-j>  <Plug>(exjumplist-next-buffer)
nmap <Esc><C-k>  <Plug>(exjumplist-previous-buffer)




" fakeclip  "{{{2

let g:fakeclip_x_selection = 'clipboard'




" grex  "{{{2

nmap gy  <Plug>(operator-grex-yank)<Plug>(textobj-entire-a)
vmap gy  <Plug>(operator-grex-yank)
nmap gd  <Plug>(operator-grex-delete)<Plug>(textobj-entire-a)
vmap gd  <Plug>(operator-grex-delete)




" ku  "{{{2

autocmd MyAutoCmd FileType ku
\   call ku#default_key_mappings(s:TRUE)
\ | call s:ku_my_key_mappings()

function! s:ku_my_key_mappings()
  imap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
  nmap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
endfunction


call ku#custom_action('common', 'cd', s:SID_PREFIX().'ku_common_action_my_cd')
call ku#custom_action('common', 'Yank', s:SID_PREFIX().'ku_common_action_Yank')
call ku#custom_action('common', 'yank', s:SID_PREFIX().'ku_common_action_yank')

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


call ku#custom_key('common', 'y', 'yank')
call ku#custom_key('common', 'Y', 'Yank')
call ku#custom_key('buffer', 'd', 'delete')

call ku#custom_prefix('common', '.vim', expand('~/.vim'))
call ku#custom_prefix('common', 'HOME', expand('~'))
call ku#custom_prefix('common', 'VIM', expand('$VIMRUNTIME'))
call ku#custom_prefix('common', '~', expand('~'))


nnoremap <silent> [Space]ka  :<C-u>Ku args<CR>
nnoremap <silent> [Space]kb  :<C-u>Ku buffer<CR>
nnoremap <silent> [Space]kf  :<C-u>Ku file<CR>
nnoremap <silent> [Space]kg  :<C-u>Ku metarw/git<CR>
nnoremap <silent> [Space]kh  :<C-u>Ku history<CR>
nnoremap <silent> [Space]kk  :<C-u>call ku#restart()<CR>
nnoremap <silent> [Space]kq  :<C-u>Ku quickfix<CR>
nnoremap <silent> [Space]ks  :<C-u>Ku source<CR>

nnoremap <silent> [Space]k/  :<C-u>Ku cmd_mru/search<CR>
nnoremap <silent> [Space]k:  :<C-u>Ku cmd_mru/cmd<CR>
nnoremap <silent> [Space]km  :<C-u>Ku file_mru<CR>


if s:win_p
  let g:ku_personal_runtime = expand('~/.vim')
  let g:ku_file_mru_file = expand('~/.vim/info/ku/mru_win')
else
  let g:ku_file_mru_file = expand('~/.vim/info/ku/mru')
endif
let g:ku_file_mru_limit = 200
let g:ku_file_mru_ignore_pattern = '\v/$|^/cygdrive/|^/mnt/|^/media/|^//'




" narrow  "{{{2

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

let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*\|\[ku\]'
let g:neocomplcache_temporary_dir = expand('~/.vim/info/neocomplcache')




" operator-replece  "{{{2

map _  <Plug>(operator-replace)




" quickrun  "{{{2

nnoremap <silent> <Leader>R  :<C-u>QuickRun >: -mode n<CR>
vnoremap <silent> <Leader>R  :<C-u>QuickRun >: -mode v<CR>


let g:loaded_quicklaunch = 1
let g:quickrun_config = {
\  '*': {
\    'split': '{'.s:SID_PREFIX().'vertically() ? "vertical" : "" }',
\  },
\  'java': {
\    'exec': ['javac %s', '%c %s:t:r %a'],
\    'tempfile': '{fnamemodify(tempname(), ":p:h")}/{expand("%:t")}',
\  },
\  'javaapplet': {
\    'exec': ['echo "<applet code=%s:t:r width=500 height=500></applet>" > %s:p:r.html',
\             'appletviewer %s:p:r.html'],
\    'tempfile': '{fnamemodify(tempname(), ":p:h")}/{expand("%:t")}',
\  },
\  'markdown': {
\    'exec': ['markdown.pl %s | tee %s:p:r.html'],
\  },
\  'tex': {
\    'exec': ['platex --kanji=utf8 -shell-escape -file-line-error -output-directory=%s:p:h %s',
\             'pxdvi %s:p:r'],
\    'runmode': 'simple',
\  },
\  'xdefaults': {
\    'exec': ['xrdb -remove', 'xrdb -merge %s:p:r', 'xrdb -query'],
\  },
\ }




" ref  "{{{2

autocmd MyAutoCmd FileType ref
\   nmap <buffer> <silent> <C-]>  <Plug>(ref-keyword)
\ | nmap <buffer> <silent> <C-j>  <Plug>(ref-forward)
\ | nmap <buffer> <silent> <C-k>  <Plug>(ref-back)
\ | nnoremap <buffer> q  <C-w>c

nnoremap <silent> <Leader>t  :<C-u>Ref alc <C-r><C-w><CR>

AlterCommand ref  Ref


let g:ref_no_default_key_mappings = 1
let g:ref_cache_dir = expand('~/.vim/info/ref')
let g:ref_open = 'Split'




" scratch  "{{{2

autocmd MyAutoCmd User PluginScratchInitializeAfter
\   map <buffer> <CR>  <Plug>(scratch-evaluate)
\ | map <buffer> <C-m>  <Plug>(scratch-evaluate)
\ | map <buffer> q  <Plug>(scratch-close)

nmap <Leader>s  <Plug>(scratch-open)


if s:win_p
  let g:scratch_buffer_name = '[Scratch]'
endif
let g:scratch_show_command = 'Split | hide buffer'



" smartword  "{{{2

map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)




" submode  "{{{2

call submode#enter_with('scroll', 'n', '', '[Space]j')
call submode#map('scroll', 'n', 'e', 'j', 'line(".") != line("$") ? "<C-d>" : ""')
call submode#map('scroll', 'n', 'e', 'k', 'line(".") != 1         ? "<C-u>" : ""')

call submode#enter_with('window', 'n', '', '[Space]w')
call submode#map('window', 'n', '', '<Space>', '<C-w>=')
call submode#map('window', 'n', '', 'H', '<C-w>H')
call submode#map('window', 'n', '', 'J', '<C-w>J')
call submode#map('window', 'n', '', 'K', '<C-w>K')
call submode#map('window', 'n', '', 'L', '<C-w>L')
call submode#map('window', 'n', '', 'h', '<C-w><lt>')
call submode#map('window', 'n', '', 'j', '<C-w>+')
call submode#map('window', 'n', '', 'k', '<C-w>-')
call submode#map('window', 'n', '', 'l', '<C-w>>')


let g:submode_timeout = 0




" surround  "{{{2

" The default mapping ys for <Plug>Ysurround is not consistent with
" the default mappings of vi -- y is for yank.
nmap s  <Plug>Ysurround
nmap ss  <Plug>Yssurround




" vcsi  "{{{2

let g:vcsi_diff_in_commit_buffer_p = 1
let g:vcsi_open_command = 'Split | enew'
let g:vcsi_use_native_message_p = 1




" vimfiler  "{{{2

let g:vimfiler_as_default_explorer = 1
let g:vimfiler_edit_command = 'tabedit'
let g:vimfiler_split_command = 'Split'
let g:vimfiler_trashbox_directory = expand('~/.trash')

if executable('feh')
  call vimfiler#set_execute_file('bmp,gif,jpeg,jpg,png', 'feh')
endif
if executable('acroread')
  call vimfiler#set_execute_file('pdf', 'acroread')
endif
if executable('oobase')
  call vimfiler#set_execute_file('doc,odt,ott', 'oowriter')
  call vimfiler#set_execute_file('ods,xls', 'oocalc')
  call vimfiler#set_execute_file('odp,ppt', 'oodraw')
endif




" vimshell  "{{{2

autocmd MyAutoCmd FileType vimshell
\   call vimshell#altercmd#define('l', 'll')
\ | call vimshell#altercmd#define('la', 'ls -a')
\ | call vimshell#altercmd#define('ll', 'ls -l')
\ | call vimshell#altercmd#define('lla', 'ls -la')


let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
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
