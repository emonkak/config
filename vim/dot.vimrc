" My .vimrc
" Basic  {{{1
" Absolute  {{{2

if has('vim_starting')
  if has('win32') || has('win64')
    set runtimepath^=~\\vimfiles\\bundle\\*
    set runtimepath+=~\\vimfiles\\bundle\\*\\after
  else
    set runtimepath^=~/.vim/bundle/*
    set runtimepath+=~/.vim/bundle/*/after
  endif
endif

filetype plugin indent on

let s:SID = expand('<SID>')




" Encoding  {{{2

set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,cp932,ucs-bom

if has('win32') || has('win64')
  set fileformat=unix
  set termencoding=cp932
endif




" Color and Font  {{{2

if (1 < &t_Co || has('gui')) && has('syntax') && !exists('g:syntax_on')
  syntax enable
  if !exists('g:colors_name')
    colorscheme basic256
  endif
endif

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




" File and Directory  {{{2

set directory& directory-=. directory^=~/.cache/vim
set history=1000
set undofile
set undodir=~/.cache/vim/undo
set nowritebackup
set viminfo^='1000 viminfo-='100




" User Interface  {{{2

set ambiwidth=double
set backspace=indent,eol,start
if exists('+clipboard')
  set clipboard& clipboard-=autoselect clipboard+=autoselectml
  if has('unnamedplus')
    set clipboard+=unnamedplus
  endif
endif
set cmdheight=1
set complete& complete-=i complete-=t
set completeopt=menuone,longest
if has('conceal')
  set concealcursor=nc
  set conceallevel=0
endif
set confirm
set diffopt=filler,vertical
set display=lastline
set noequalalways
set foldmethod=marker
set fileformats=unix,dos,mac
set hidden
if exists('+langremap')
  set nolangremap
endif
set laststatus=2
set linebreak
set list
let &listchars = "tab:\u00bb ,trail:-,extends:>,precedes:<,conceal:|,nbsp:."
set keywordprg=:help
set nrformats-=octal
set pumheight=20
set scrolloff=5
if exists('+shellslash')
  set shellslash
endif
set showcmd
set splitbelow
set splitright
set synmaxcol=1000
if exists('+tagcase')
  set tagcase=match
endif
set title
set titlestring=Vim:\ %f\ %h%r%m
set ttimeoutlen=50
set updatetime=1000
set virtualedit=block
set nowrapscan
set wildmenu
set wildmode=full




" Indent  {{{2

set autoindent
if exists('+breakindent')
  set breakindent
endif
set cinoptions=:1s,l1,g0,t0,(0,W1s
set formatoptions=roqnlmM1
set formatlistpat&
if &formatlistpat != ''
  let &formatlistpat .= '\|^\s*[*+-]\s*'
endif
set ignorecase
set incsearch
set shiftround
set smartcase
set smartindent




" Status-line  {{{2

let &statusline = '%<%f %h%m%r%w'
\               . '%='
\               . '['
\               .   '%{&l:fileencoding == "" ? &encoding : &l:fileencoding}'
\               .   '%{&l:bomb ? "/BOM" : ""}'
\               . ']'
\               . '[%{&l:fileformat}]'
\               . '   %-14.(%l,%c%V%) %P'




" Tab-line  {{{2

function! s:my_tabline() abort
  let s = ''

  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let curbufnr = bufnrs[tabpagewinnr(i) - 1]

    let no = (i <= 10 ? i - 1 : '#')
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

  let s .= '%#TabLineFill#%T'
  let s .= '%=%#TabLine#'
  let s .= '| %<'
  let s .= fnamemodify(getcwd(), ':t')
  return s
endfunction

set showtabline=1

let &tabline = '%!' . s:SID . 'my_tabline()'




" Misc.  {{{2

augroup MyAutoCmd
  autocmd!
augroup END

call altercmd#load()
call arpeggio#load()
call metarw#define_wrapper_commands(0)

let g:mapleader = ','
let g:maplocalleader = '.'




" Commands  {{{1
" :grep wrappers  {{{2

command! -complete=file -nargs=+ Grep
\ call s:grep('grep', 'cwindow', [<f-args>])
command! -complete=file -nargs=+ Lgrep
\ call s:grep('lgrep', 'lwindow', [<f-args>])

function! s:grep(command, window_command, args) abort
  let target = join(a:args[:-2], ' ')

  if &grepprg ==# 'internal'
    execute a:command '/' . escape(a:args[-1], '|/ ') . '/j' target
  else
    execute a:command . '!' escape(shellescape(a:args[-1]), '|') target
  endif

  execute a:window_command
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  LGrep

let s:GREPPRGS = [
\   'git grep -n',
\   executable('rg') ? 'rg --vimgrep --no-heading --no-column' :
\   executable('grep') ? 'grep -nHE' :
\   'internal',
\ ]

function! s:toggle_grepprg() abort
  let i = (index(s:GREPPRGS, &grepprg) + 1) % len(s:GREPPRGS)
  let &grepprg = s:GREPPRGS[i]
  echo &grepprg
endfunction

autocmd MyAutoCmd BufReadPost *
\ let &l:grepprg = finddir('.git', expand('%:p:h') . ';') != ''
\   ? s:GREPPRGS[0]
\   : s:GREPPRGS[1]




" :make wrappers  {{{2

command! -bar -complete=file -nargs=* Make
\ call s:make('make', 'cwindow', [<f-args>])
command! -bar -complete=file -nargs=* Lmake
\ call s:make('lmake', 'lwindow', [<f-args>])

function! s:make(command, window_command, args) abort
  let old_winnr = winnr()

  execute a:command . '!' join(a:args)

  execute a:window_command

  execute old_winnr 'wincmd w'
endfunction

AlterCommand mak[e]  Make
AlterCommand lmak[e]  Lmake




" :edit with specified 'fileencoding'  {{{2

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




" BufferCleaner  {{{2

command! -bang -nargs=0 BufferClean  call s:cmd_BufferClean(<bang>0)

function! s:cmd_BufferClean(is_banged) abort
  let bufnrs = range(1, bufnr('$'))
  call filter(bufnrs, 'bufexists(v:val)
  \                 && buflisted(v:val)
  \                 && (bufname(v:val) == "" || !filereadable(bufname(v:val)))
  \                 && (a:is_banged || !getbufvar(v:val, "&modified"))')
  for bufnr in bufnrs
    silent execute bufnr 'bdelete' . (a:is_banged ? '!' : '')
  endfor
  if len(bufnrs) == 0
    echo "No buffer is deleted"
  elseif (bufnrs) == 1
    echo '1 buffer is deleted'
  else
    echo len(bufnrs) 'buffers are deleted'
  endif
endfunction




" Bundle  {{{2

let s:BUNDLE_DIR = $HOME . '/.vim/bundle'

command! BundleList  call s:cmd_BundleList()
command! -nargs=1 BundleInstall  call s:cmd_BundleInstall(<f-args>)
command! -bang BundleUpdate  call s:cmd_BundleUpdate(<bang>0)

function! s:cmd_BundleList() abort
  let bundles = split(glob(s:BUNDLE_DIR . '/*'), "\n")

  for bundle in bundles
    let head = bundle . '/.git/HEAD'
    if filereadable(head)
      echo readfile(head, '', 1)[0] fnamemodify(bundle, ':t')
    else
      echo fnamemodify(bundle, ':t')
    endif
  endfor

  echohl Title
  echo len(packages) 'plugins available'
  echohl None
endfunction

function! s:cmd_BundleInstall(package) abort
  let matches = matchlist(a:package,
  \                       '^\(\%(git\|https\?\)://[^/]\+\)\?/*\(.\+\)')

  if !empty(matches)
    let repository = (matches[1] == '' ? 'git://github.com' : matches[1])
    \              . '/'
    \              . substitute(matches[2], '\%(\.git\)\?$', '.git', '')
    let bundle = fnamemodify(s:BUNDLE_DIR . '/' . substitute(matches[2], '/', '_', ''), ':r')
    let command = join([
    \   'git',
    \   'clone',
    \   repository,
    \   shellescape(bundle)
    \ ])

    if isdirectory(bundle)
      echohl ErrorMsg
      echo 'The package is already installed'
      echohl None
    else
      echo system(command)
      silent! execute 'source' bundle . '/plugin/*.vim'
    endif
  else
    echohl ErrorMsg
    echo 'Invalid package name:' string(a:package)
    echohl None
  endif
endfunction

function! s:cmd_BundleUpdate(is_banged) abort
  for bundle in split(glob(s:BUNDLE_DIR . '/*'), "\n")
    if !a:is_banged
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

    let output = system(git . ' pull --rebase')
    if v:shell_error == 0
      echo output
    else
      echohl ErrorMsg
      echo 'An error was occurred occurred executing:'
      \    string(git . ' pull --rebase')
      echo output
      echohl None
    endif
  endfor
endfunction




" CD  {{{2

command! -nargs=* -complete=customlist,s:complete_cdpath CD
\ call s:cmd_CD(<q-args>)

function! s:complete_cdpath(arglead, cmdline, cursorpos) abort
  let paths = globpath(&cdpath,
  \                    join(split(a:cmdline, '\s', !0)[1:], ' ') . '*/',
  \                    0,
  \                    !0)
  return map(uniq(sort(paths)), 'v:val[:-2]')
endfunction

function! s:cmd_CD(path) abort
  if a:path != ''
    cd `=a:path`
  else
    let git_root = finddir('.git', expand('%:p:h') . ';')
    if git_root != ''
      cd `=fnamemodify(git_root, ':p:h:h')`
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




" FoldDebug  {{{2

command! -range=% FoldDebug
\ <line1>,<line2>global/^/echo printf(
\   "%*d [%2s] %s",
\   len(line('$')),
\   line('.'),
\   eval(substitute(&l:foldexpr, 'v:lnum', line('.'), '')),
\   getline('.')
\ )




" Note  {{{2

command! -bar -nargs=? Note
\ execute printf(
\   'edit %s/Sync/Documents/Notes/%s%s.md',
\   $HOME,
\   strftime('%Y-%m-%d'),
\   <q-args> != '' ? '_' . <q-args> : ''
\ )




" Rename  {{{2

command! -complete=file -nargs=1 Rename  call s:cmd_Rename(<q-args>)

function! s:cmd_Rename(path) abort
  let current = expand('%')
  if &l:readonly || (filereadable(current) && !filewritable(current))
    echohl ErrorMsg
    echo 'The buffer is read-only'
    echohl None
  elseif filereadable(a:path)
    echohl ErrorMsg
    echo 'A file already exists at the destination' string(a:path)
    echohl None
  else
    let directory = fnamemodify(a:path, ':p:h')
    if !isdirectory(directory)
      call mkdir(directory, 'p')
    endif
    file `=a:path`
    write
    call delete(current)
    redraw
    echo 'Renamed:' current '->' a:path
  endif
endfunction




" Reverse  {{{2

command! -bar -range=% Reverse  <line1>,<line2>g/^/m<line1>-1 | nohlsearch




" Source  {{{2

command! -bar -complete=file -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>

AlterCommand so[urce]  Source




" SuspendWithAutomticCD  {{{2

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
    silent execute printf(
    \  '!tmux %s \; send-keys -t %s C-u " cd \"%s\"" C-m',
    \   select_command,
    \   target_pane,
    \   getcwd()
    \ )
    redraw!
  else
    suspend
  endif
endfunction




" SyntaxCheck {{{2

command! -bar -nargs=0 SyntaxCheck
\ echo join(<SID>syntax_stack(line('.'), col('.')), '/')

function! s:syntax_stack(line, col) abort
  let stack = []

  for syn_id in synstack(a:line, a:col)
    let syn_trans_id = synIDtrans(syn_id)
    let name = synIDattr(syn_id, 'name')

    if syn_id != syn_trans_id
      let name .= '<' . synIDattr(syn_trans_id, 'name') . '>'
    endif

    call add(stack, name)
  endfor

  return stack
endfunction




" Sum  {{{2

command! -bang -range -nargs=* Sum
\ call append(<line2>,
\             string(s:fold_lines(s:SID . 'sum', 0, range(<line1>, <line2>))))

function! s:fold_lines(f, acc, range) abort
  let acc = a:acc

  for lnum in a:range
    let line = getline(lnum)
    let acc = call(a:f, [acc, line])
  endfor

  return acc
endfunction

function! s:sum_line(acc, line) abort
  let n = matchstr(a:line, '\d\+\%(\.\d\+\)\?')
  let n = n != '' ? str2nr(number, 10) : 0
  return a:acc + n
endfunction




" TabpageTitle  {{{2

command! -bar -nargs=* TabpageTitle
\   if <q-args> == ''
\ |   let t:title = input("Set tabpage's title to: ", '')
\ | else
\ |   let t:title = <q-args>
\ | endif
\ | redraw!




" Window helpers  {{{2

command! -bar -complete=file -nargs=* Split
\ Vertical split <args>
command! -bar -complete=file -nargs=* SplitTop
\ Vertical topleft split <args>
command! -bar -complete=file -nargs=* SplitBottom
\ Vertical botright split <args>
command! -bar -complete=help -nargs=* Help
\ Vertical help <args>
command! -bar -complete=file -nargs=* New
\ Vertical new <args>

command! -bar -nargs=* Vertical
\ execute (s:should_vertical() ? 'vertical' : '') <q-args>

AlterCommand sp[lit]  Split
AlterCommand h[elp]  Help
AlterCommand new  New

function! s:should_vertical() abort
  return winwidth(0) > winheight(0) * 3
endfunction




" Mappings  {{{1
" QuickFix  {{{2

" The prefix
noremap q  <Nop>

" Alternative for the original action
nnoremap Q  q


" For quickfix list
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


" For location list
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




" Tab pages  {{{2

" The prefix
noremap <C-t>  <Nop>


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


" Moving around tab pages
nnoremap <silent> <C-t>j
\ :<C-u>execute 'tabnext' 1 + (tabpagenr() + v:count1 - 1) % tabpagenr('$')<CR>
nnoremap <silent> <C-t>k
\ :<C-u>execute 'tabprevious' v:count1 % tabpagenr('$')<CR>
nnoremap <silent> <C-t>K  :<C-u>tabfirst<CR>
nnoremap <silent> <C-t>J  :<C-u>tablast<CR>

nmap <C-t><C-j>  <C-t>j
nmap <C-t><C-k>  <C-t>k
nmap <C-t><C-t>  <C-t>j


" Move specific tag page
for s:i in range(10)
  execute 'nnoremap <silent>' ('<C-t>'.(s:i))  ((s:i+1).'gt')
endfor
unlet s:i


" Moving tabpages them selves
nnoremap <silent> <C-t>l  :<C-u>execute 'tabmove' '+'.v:count1<CR>
nnoremap <silent> <C-t>h  :<C-u>execute 'tabmove' '-'.v:count1<CR>
nnoremap <silent> <C-t>L  :<C-u>tabmove<CR>
nnoremap <silent> <C-t>H  :<C-u>tabmove 0<CR>

nmap <C-t><C-l>  <C-t>l
nmap <C-t><C-h>  <C-t>h




" Argument list  {{{2

" The prefix
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




" Command-line editing  {{{2

" GNU Emacs like mappings
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-a>  <Home>
cnoremap <C-d>  <Delete>
cnoremap <C-y>  <C-r>"
cnoremap <C-k>
\ <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>
cnoremap <expr> <C-u>  getcmdline() == '' ? "\<C-c>" : "\<C-u>"
cnoremap <expr> <C-w>  getcmdline() == '' ? "\<C-c>" : "\<C-w>"


" Swap <C-p> and <C-n> to <Up> and <Down>
cnoremap <C-p>  <Up>
cnoremap <C-n>  <Down>
cnoremap <Up>  <C-p>
cnoremap <Down>  <C-n>


" Alternative key for the action to open the command-line history
cnoremap <C-o>  <C-f>


" Escape the slash character when command-line type is 'search'
cnoremap <expr> /  index(['/', '?'], getcmdtype()) >= 0 ? '\/' : '/'




" Command-line window  {{{2

autocmd MyAutoCmd CmdwinEnter *
\ call s:on_CmdwinEnter()

function! s:on_CmdwinEnter() abort
  " Close the command-line window
  nnoremap <buffer> <Esc><Esc>  <Esc><C-w>q
  inoremap <buffer> <Esc><Esc>  <Esc><C-w>q

  " Close the command-line window if the current line is empty
  inoremap <buffer> <expr> <BS>
         \ getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<BS>"
  inoremap <buffer> <expr> <C-w>
         \ getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<C-w>"
  inoremap <buffer> <expr> <C-u>
         \ getline('.') == '' ? "\<C-c>\<C-c>" : col('.') == 1 ? '' : "\<C-u>"

  imap <buffer> <C-h>  <BS>

  startinsert!
endfunction




" Insert mode  {{{2

" GNU Emacs like mappings
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


" Alternatives for original actions
inoremap <C-\>  <C-a>
inoremap <C-q>  <C-d>


" To be able to undo these types of deletion
inoremap <C-w>  <C-g>u<C-w>
inoremap <C-u>  <C-g>u<C-u>


" Complete or indent
inoremap <expr> <Tab>  pumvisible()
                   \ ? "\<C-n>"
                   \ : <SID>should_indent_rather_than_complete()
                   \ ? "\<C-i>"
                   \ : <SID>keys_to_complete()
inoremap <expr> <S-Tab>  pumvisible()
                     \ ? "\<C-p>"
                     \ : <SID>should_indent_rather_than_complete()
                     \ ? "\<C-i>"
                     \ : <SID>keys_to_complete()

function! s:should_indent_rather_than_complete() abort  "{{{
  return getline('.')[col('.') - 2] !~ '^\S'
endfunction  "}}}

function! s:keys_to_complete() abort  "{{{
  if &l:filetype ==# 'vim'
    return "\<C-x>\<C-v>"
  elseif &l:omnifunc != ''
    return "\<C-x>\<C-o>"
  elseif &l:completefunc != ''
    return "\<C-x>\<C-u>"
  else
    return "\<C-n>"
  endif
endfunction  "}}}




" The <Space>  {{{2

" The prefix
map <Space>  [Space]
noremap [Space]  <Nop>


" The shortcut for :write
nnoremap <silent> [Space]w  :<C-u>w<CR>
nnoremap <silent> [Space]W  :<C-u>w sudo:/%<CR>


" Close temporary windows such as 'quickfix', 'help'
nnoremap <silent> [Space]c  :<C-u>call <SID>close_temporary_windows()<CR>

function! s:close_temporary_windows() abort  "{{{
  let all_winnrs = range(1, winnr('$'))
  if len(all_winnrs) < 2
    return
  endif
  let current_winnr = winnr()
  for winnr in reverse(all_winnrs)
    let bufnr = winbufnr(winnr)
    if !buflisted(bufnr) && getbufvar(bufnr, '&buftype') != ''
      execute winnr . 'wincmd w'
      wincmd c
    endif
  endfor
endfunction  "}}}


" Toggle options
nnoremap [Space]o  <Nop>
nnoremap <silent> [Space]oc  :<C-u>call <SID>toggle_colorcolumn()<CR>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]og  :<C-u>call <SID>toggle_grepprg()<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]op  :<C-u>call <SID>toggle_option('paste')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>
nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>

function! s:toggle_option(option_name) abort  "{{{
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
endfunction  "}}}

function! s:toggle_colorcolumn() abort  "{{{
  if exists('b:textwidth')
    let &l:textwidth = b:textwidth
    unlet b:textwidth
    setlocal colorcolumn& colorcolumn?
  else
    let b:textwidth = &l:textwidth
    if b:textwidth == 0
      setlocal textwidth=80
    endif
    setlocal colorcolumn=+1 colorcolumn?
  endif
endfunction  "}}}


" Helpers for file options
nnoremap [Space]f  <Nop>
nnoremap [Space]fe  :<C-u>setlocal fileencoding=
nnoremap [Space]ff  :<C-u>setlocal fileformat=
nnoremap [Space]fs  :<C-u>setlocal filetype? fileencoding? fileformat?<CR>
nnoremap [Space]ft  :<C-u>setfiletype<Space>


" :Rename helper
nnoremap [Space]r  :<C-\>esetcmdpos(8 + strlen(expand('%:p:r'))) ? '' : 'Rename '.expand("%:p")<CR>


" Fold actions
nnoremap [Space]l  zo
nnoremap [Space]h  zc
nnoremap [Space]v  zMzv


" Paste the clipboard content
nnoremap [Space]p  "+p


" Enter the command-line window
nnoremap [Space]:  q:
xnoremap [Space]:  q:


" Reload vimrc
nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>




" Windows  {{{2

nnoremap <Tab>  <C-w>w
nnoremap <S-Tab>  <C-w>W


nnoremap <C-w>*  :<C-u>Split \| normal! *<CR>
nnoremap <C-w>#  :<C-u>Split \| normal! #<CR>


nnoremap <C-w>f  :<C-u>Split wincmd f<CR>
nnoremap <C-w><C-f>  :<C-u>Split wincmd f<CR>


nnoremap <silent> <C-w>t
\ :call <SID>move_window_into_tabpage(<SID>ask_tabpage_number())<CR>

nmap <C-w><C-t>  <C-w>t

function! s:ask_tabpage_number() abort  "{{{
  echon 'Which tabpage to move this window into? '

  let c = nr2char(getchar())
  if c =~# '[0-9]'
    return 1 + char2nr(c) - char2nr('0')
  else
    return 0
  endif
endfunction  "}}}

function! s:move_window_into_tabpage(target_tabpagenr) abort  "{{{
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
endfunction  "}}}


nnoremap <C-w>Q  :<C-u>quit!<CR>




" Text objects  {{{2

" Synonym for <>
onoremap aa  a>
vnoremap aa  a>
onoremap ia  i>
vnoremap ia  i>


" Synonym for []
onoremap ar  a]
vnoremap ar  a]
onoremap ir  i]
vnoremap ir  i]


" Select the last changed text
nnoremap <Plug>(textobj-last-changed-text)  `[v`]h
onoremap <silent> <Plug>(textobj-last-changed-text)  :<C-u>normal! gc<CR>
vnoremap <silent> <Plug>(textobj-last-changed-text)  :<C-u>normal! gc<CR>

map gc  <Plug>(textobj-last-changed-text)


" Select the last selected text
onoremap <silent> gv  :<C-u>normal! gv<CR>




" Operators  {{{2
" operator-increment/decrement  {{{3

call operator#user#define('increment',
\                         s:SID . 'operator_increment')
call operator#user#define('decrement',
\                         s:SID . 'operator_decrement')

function! s:operator_increment(motion_wiseness) abort
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]g'."\<C-a>"
endfunction

function! s:operator_decrement(motion_wiseness) abort
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]g'."\<C-x>"
endfunction

map g<C-a>  <Plug>(operator-increment)
map g<C-x>  <Plug>(operator-decrement)


" operator-search-forward/backward  {{{3

call operator#user#define('search-forward',
\                         s:SID . 'operator_search',
\                         'let v:searchforward = 1')
call operator#user#define('search-backward',
\                         s:SID . 'operator_search',
\                         'let v:searchforward = 0')

function! s:operator_search(motion_wiseness) abort
  let reg_0 = [@0, getregtype('0')]

  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]"0y'

  let @/ = '\V' . substitute(escape(@0, '\'), '\n', '\\n', 'g')
  call histadd('/', @/)
  execute 'normal!' v:searchforward ? 'n' : 'N'

  call setreg('0', reg_0[0], reg_0[1])
endfunction

vmap *  <Plug>(operator-search-forward)
vmap #  <Plug>(operator-search-backward)


" operator-translate  {{{3

call operator#user#define('translate', s:SID . 'operator_translate')

function! s:operator_translate(motion_wiseness) abort
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

Arpeggio map ot  <Plug>(operator-translate)


" operator-yank-clipboard  {{{3

call operator#user#define('yank-clipboard',
\                         s:SID . 'operator_yank_clipboard')

function! s:operator_yank_clipboard(motion_wiseness) abort
  let visual_command =
  \ operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' '`['.visual_command.'`]"+y'
endfunction

Arpeggio map oy  <Plug>(operator-yank-clipboard)




" Misc.  {{{2

" Command-line helpers
nnoremap <C-h>  :<C-u>Help<Space>
nnoremap <C-o>  :<C-u>edit<Space>
nnoremap <C-w>.  :<C-u>edit .<CR>


" x/X without register
nnoremap X  "_X
nnoremap x  "_x


" To yank from the cursor to the end of line
nnoremap Y  y$


" Jump list navigations
nnoremap <C-j>  <C-i>
nnoremap <C-k>  <C-o>


" Disable dangerous keys
nnoremap ZZ  <Nop>
nnoremap ZQ  <Nop>

" The shortcut for :update
nnoremap <silent> <Leader><Leader>  :<C-u>update<CR>


" Like o/O, but insert additional [count] blank lines
nnoremap <expr> <Plug>(arpeggio-default:o)
\        <SID>start_insert_mode_with_blank_lines('o')
nnoremap <expr> O
\        <SID>start_insert_mode_with_blank_lines('O')

function! s:start_insert_mode_with_blank_lines(command) abort  "{{{
  if v:count != v:count1
    return a:command
  endif

  if a:command ==# 'o'
    return "\<Esc>o" . repeat("\<CR>", v:count - 1)
  else
    return "\<Esc>O" . repeat("\<CR>\<Up>", v:count - 1) . "\<Esc>S"
  endif
endfunction  "}}}


" Make searching directions consistent
nnoremap <expr> n  v:searchforward ? 'nzv' : 'Nzv'
nnoremap <expr> N  v:searchforward ? 'Nzv' : 'nzv'
vnoremap <expr> n  v:searchforward ? 'nzv' : 'Nzv'
vnoremap <expr> N  v:searchforward ? 'Nzv' : 'nzv'
onoremap <expr> n  v:searchforward ? 'n' : 'N'
onoremap <expr> N  v:searchforward ? 'N' : 'n'


" Jump to the definition of the keyword under the cursor
nnoremap <expr> <CR>  &l:filetype ==# 'qf' ? "\<CR>" : "\<C-]>"
vnoremap <expr> <CR>  &l:filetype ==# 'qf' ? "\<CR>" : "\<C->"


noremap <C-z>  <Nop>
nnoremap <C-z>  :<C-u>SuspendWithAutomticCD<CR>


noremap <Leader>  <Nop>
noremap <LocalLeader>  <Nop>




" Filetypes  {{{1
" All filetypes   {{{2

autocmd MyAutoCmd FileType *
\ call s:on_FileType_any()

function! s:on_FileType_any() abort
  if &l:completefunc == ''
    setlocal completefunc=autoprogramming#complete
  endif

  if &l:omnifunc == ''
    setlocal omnifunc=syntaxcomplete#Complete
  endif

  " Disable the auto-wrapping
  setlocal formatoptions-=t formatoptions-=c
endfunction


" Optimize the loading for large files
autocmd MyAutoCmd BufReadPre *
\   if getfsize(expand("<afile>")) > 1024 * 1024 * 8
\ |   set eventignore+=FileType
\ |   setlocal noswapfile
\ | else
\ |   set eventignore-=FileType
\ | endif


" Unset 'paste' automatically
autocmd MyAutoCmd InsertLeave *  set nopaste


" Visualize ideographic spaces
autocmd MyAutoCmd VimEnter,WinEnter *
\ match Underlined /[\u3000\ufff9-\ufffc]/


" When editing a file, always jump to the last known cursor position
autocmd MyAutoCmd BufReadPost *
\   if line("'\"") >= 1 && line("'\"") <= line("$")
\ |   execute "normal! g`\""
\ | endif


" Avoid duplicated `filetype` settings
let g:ft_ignore_pat = '^\(ebuild\|eclass\)$'




" PHP  {{{2

let g:PHP_vintage_case_default_indent = 1
let g:PHP_noArrowMatching = 1




" Python  {{{2

let g:python_highlight_all = 1




" Rust  {{{2

let g:rust_recommended_style = 1

let g:cargo_makeprg_params = 'build --all-targets'




" Shell Script  {{{2

let g:is_bash = 1




" Vim Script  {{{2

let g:vim_indent = {
\   "line_continuation": 0,
\ }




" Plugins  {{{1
" accelerate  {{{2

call accelerate#map('nv', 'e', '<C-u>', 'repeat("\<C-u>", v:count1)')
call accelerate#map('nv', 'e', '<C-d>', 'repeat("\<C-d>", v:count1)')

call accelerate#map('nv', 'e', 'j', 'v:count == 0 ? "gj" : "j"')
call accelerate#map('nv', 'e', 'k', 'v:count == 0 ? "gk" : "k"')
call accelerate#map('nv', '', 'h', 'h')
call accelerate#map('nv', 'e', 'l', 'foldclosed(line(".")) != -1 ? "zo" : "l"')




" exjumplist  {{{2

nmap <Esc><C-j>  <Plug>(exjumplist-next-buffer)
nmap <Esc><C-k>  <Plug>(exjumplist-previous-buffer)
nmap <M-C-j>  <Plug>(exjumplist-next-buffer)
nmap <M-C-k>  <Plug>(exjumplist-previous-buffer)




" grex  {{{2

nmap gy  <Plug>(operator-grex-yank)<Plug>(textobj-entire-a)
vmap gy  <Plug>(operator-grex-yank)
nmap gd  <Plug>(operator-grex-delete)<Plug>(textobj-entire-a)
vmap gd  <Plug>(operator-grex-delete)




" ku  {{{2

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


call ku#custom_action('common', 'cd', s:SID . 'ku_common_action_my_cd')
call ku#custom_action('common', 'Yank',
\                     s:SID . 'ku_common_action_Yank')
call ku#custom_action('common', 'yank',
\                     s:SID . 'ku_common_action_yank')
call ku#custom_action('file', 'open-sudo',
\                     s:SID . 'ku_file_action_open_sudo')
call ku#custom_action('file/current', 'open-sudo',
\                     s:SID . 'ku_file_action_open_sudo')

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
  edit `='sudo:/' . fnamemodify(a:item.word, ':p')`
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

let g:ku_file_mru_file = expand('~/.vim/info/ku/mru')
let g:ku_file_mru_ignore_pattern = '/$\|/\.git/\|^/\(/\|mnt\|tmp\)'
let g:ku_file_mru_limit = 1000




" lsp  {{{2

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
    \        ['Setup.hs', 'stack.yml']
    \      )
    \   )},
    \   'allowlist': ['haskell'],
    \ })
  elseif executable('rls')
    call lsp#register_server({
    \   'name': 'rust-analyzer',
    \   'cmd': {server_info -> ['rust-analyzer']},
    \   'root_uri': {server_info -> lsp#utils#path_to_uri(
    \      lsp#utils#find_nearest_parent_file_directory(
    \        lsp#utils#get_buffer_path(),
    \        ['Cargo.toml']
    \      )
    \   )},
    \   'allowlist': ['rust'],
    \ })
  elseif executable('rust-analyzer')
    call lsp#register_server({
    \   'name': 'rust-analyzer',
    \   'cmd': {server_info -> [
    \     'rust-analyzer',
    \     lsp#utils#uri_to_path(server_info['root_uri'](server_info)),
    \   ]},
    \   'root_uri': {server_info -> lsp#utils#path_to_uri(
    \      lsp#utils#find_nearest_parent_file_directory(
    \        lsp#utils#get_buffer_path(),
    \        ['Cargo.toml']
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




" metarw  {{{2

let g:metarw_gist_safe_write = 1




" operator-camelize  {{{2

map <Leader>c  <Plug>(operator-camelize)
map <Leader>C  <Plug>(operator-decamelize)




" operator-comment  {{{2

Arpeggio map oc  <Plug>(operator-comment)
Arpeggio map od  <Plug>(operator-uncomment)




" operator-replece  {{{2

Arpeggio map or  <Plug>(operator-replace)




" operator-sort  {{{2

nmap [Space]S  <Plug>(operator-sort)$
vmap [Space]S  <Plug>(operator-sort)$
nmap [Space]s  <Plug>(operator-sort)
vmap [Space]s  <Plug>(operator-sort)

nmap [Space]N  <Plug>(operator-sort)$
vmap [Space]N  <Plug>(operator-sort)$
nmap [Space]n  <Plug>(operator-sort-numeric)
vmap [Space]n  <Plug>(operator-sort-numeric)




" quickrun  {{{2

command! -complete=command -nargs=+ Capture  QuickRun vim -src <q-args>

let g:quickrun_config = {
\   '_': {
\     'outputter/buffer/opener': '%{' . s:SID . 'should_vertical() ? "vsplit" : "split"}',
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
\     'type': 'cpp/g++',
\     'cmdopt': '-std=c++2b',
\   },
\   'dot': {
\     'exec': ['%c -Tsvg -o %s:p:r.svg %s']
\   },
\   'haskell': {
\     'type': 'haskell/runghc',
\     'cmdopt': '--ghc-arg=-Wall -Wno-unused-top-binds',
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
\     'cmdopt': '-A dead_code --edition 2021',
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





" repeat  {{{2

nmap \  <Plug>(repeat-.)




" scratch  {{{2

nmap <Leader>s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\ call s:on_User_plugin_scratch_initialize_after()

function! s:on_User_plugin_scratch_initialize_after() abort
  map <buffer> <CR>  <Plug>(scratch-evaluate!)
endfunction

let g:scratch_show_command = 'SplitTop | hide buffer'




" skeleton  {{{2

autocmd MyAutoCmd User plugin-skeleton-detect
\ call s:on_User_plugin_skeleton_detect()

function! s:on_User_plugin_skeleton_detect() abort
  let segments = split(expand('%:p'), '/')
  if len(segments) == 0
    return
  endif

  let filename = segments[-1]
  let directories = segments[:-2]

  if filename =~# 'LICENSE'
    SkeletonLoad license-mit
  elseif filename =~# '\.user\.js$'
    SkeletonLoad userjs
  elseif filename =~# 'Cargo\.toml$'
    SkeletonLoad cargo
  elseif filename =~# '\.vim$'
    if get(directories, -2) ==# "after" && get(directories, -1) ==# "ftplugin"
      execute 'SkeletonLoad' 'vim-after-'.directories[-1]
    elseif get(directories, -1) ==# '^\v(compiler|ftdetect|ftplugin|indent|plugin|syntax)$'
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




" smartword  {{{2

map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)




" smartinput  {{{2

let g:smartinput_no_default_key_mappings = 1

if exists('g:loaded_smartinput')
  call smartinput#clear_rules()
  call smartinput#define_default_rules()
endif

call smartinput#define_rule({
\   'at': '/\*\*\%#',
\   'char': '<CR>',
\   'input': '<CR><CR><Space>*/<Up><Space>*<Space>',
\ })

" for Vim Script  {{{
call smartinput#define_rule({
\   'at': '\%#',
\   'char': '"',
\   'input': '"',
\   'filetype': ['vim'],
\   'syntax': ['Comment']
\ })
call smartinput#define_rule({
\   'at': '\%#',
\   'char': '{',
\   'input': '{',
\   'filetype': ['vim'],
\   'syntax': ['Comment']
\ })
call smartinput#define_rule({
\   'at': '^[ \t:]*".*\%#$',
\   'char': '"',
\   'input': '"',
\   'filetype': ['vim'],
\ })
call smartinput#define_rule({
\   'at': '^[ \t:]*".*\%#$',
\   'char': '{',
\   'input': '{',
\   'filetype': ['vim'],
\ })  " }}}

" for PHP  {{{
call smartinput#define_rule({
\   'at': '\%#',
\   'char': '@',
\   'input': '$this->',
\   'filetype': ['php']
\ })
call smartinput#define_rule({
\   'at': '\%#[$A-Za-z]',
\   'char': '@',
\   'input': '@',
\   'filetype': ['php']
\ })
call smartinput#define_rule({
\   'at': '\%#',
\   'char': '@',
\   'input': '@',
\   'filetype': ['php'],
\   'syntax': ['Comment', 'Constant', 'None']
\ })  " }}}

" for Rust  {{{
call smartinput#define_rule({
\   'at': '\%#',
\   'char': "'",
\   'input': "'",
\   'filetype': ['rust'],
\ })  " }}}

call smartinput#map_trigger_keys()




" submode  {{{2

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
\                       ':<C-u>call ' . s:SID . 'submode_winsize()<CR>')
call submode#enter_with('winsize', 'n', '', '<C-w><C-@>',
\                       ':<C-u>call ' . s:SID . 'submode_winsize()<CR>')

function! s:submode_winsize() abort
  let current = winnr()
  wincmd k | let above = winnr() | execute current 'wincmd w'
  wincmd j | let below = winnr() | execute current 'wincmd w'
  wincmd h | let left = winnr() | execute current 'wincmd w'
  wincmd l | let right = winnr() | execute current 'wincmd w'

  execute printf('call submode#map("winsize", "n", "r", "j", "<C-w>%s")',
  \              above != below && current == below ? "-" : "+")
  execute printf('call submode#map("winsize", "n", "r", "k", "<C-w>%s")',
  \              above != below && current == below ? "+" : "-")
  execute printf('call submode#map("winsize", "n", "r", "h", "<C-w>%s")',
  \              left != right && current == right ? ">" : "<")
  execute printf('call submode#map("winsize", "n", "r", "l", "<C-w>%s")',
  \              left != right && current == right ? "<" : ">")
endfunction


let g:submode_timeout = 0




" surround  {{{2

map s  <Plug>(operator-surround)
map S  <Plug>(operator-surround)$

call surround#define_default_mappings()




" table-mode  {{{2

nnoremap <silent> [Space]t  :<C-u>TableModeToggle<CR>




" Fin.  {{{1

set secure




" __END__  {{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
