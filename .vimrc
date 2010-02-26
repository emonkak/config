" My vimrc
" Basic  "{{{1
" Absolute  "{{{2

if !exists('s:loaded_my_vimrc')
  " Don't reset twice on reloading - 'compatible' has so many side effects.
  set nocompatible  " to use many extensions of Vim.
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

if (1 < &t_Co || has('gui')) && has('syntax')
  if &term ==# '256color'
    set t_Co=256
  endif
  syntax on
  if !exists('g:colors_name')
    colorscheme basic
  endif
endif

filetype plugin indent on


set ambiwidth=double
set backspace=indent,eol,start
if has('clipboard')
  set clipboard&
  set clipboard+=unnamed
endif
set diffopt=filler,vertical
set directory=$HOME/.vim
set grepprg=internal
set hidden
set history=100
set nobackup

set cmdheight=1
set completeopt=longest,menu
set display=lastline
set laststatus=2
set linebreak
set list
set listchars=tab:>-,extends:<,trail:-
set noequalalways
set nohlsearch
set nonumber
set nowrapscan
set pumheight=20
set showcmd
set title
set titlestring=Vim:\ %f\ %h%r%m
set ttimeoutlen=50
set wildmenu

set autoindent
set cinoptions=:0,t0,(0,W1s
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

let &statusline = ''
let &statusline .= '(%{(&ft!=""?&ft:"none")}) %<%f %h%m%r%w'
let &statusline .= '[%{(&fenc!=""?&fenc:&enc).":".&ff}]'
let &statusline .= '%=%-16.(%l,%c [0x%B]%) %P'

function! s:my_tabline()  "{{{
  let s = ''

  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let curbufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears

    let no = (i <= 10 ? i-1 : '#')  " display 0-origin tabpagenr.
    let mod = len(filter(bufnrs, 'getbufvar(v:val, "&modified")')) ? '+' : ' '
    let title = fnamemodify(bufname(curbufnr),':t')
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
  let s .= '%999X'
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




" Syntax  {{{1
" Stuffs  "{{{2

let s:FALSE = 0
let s:TRUE = !s:FALSE




" Rename - rename the current file  "{{{2

command! -nargs=1 -complete=file Rename
\ call s:rename(<q-args>)

function! s:rename(newfile)
  let current = expand('%:p')
  if !filereadable(a:newfile) && filewritable(current)
    execute 'file' a:newfile
    write
    call delete(current)
  endif
endfunction




" Qexecute - variant of :execute with some extensions  "{{{2

command! -complete=command -nargs=* -range Qexecute
\ execute <q-args> s:count()

function! s:count(...)
  if v:count == v:count1  " is count given?
    return v:count
  else  " count isn't given.  (the default '' is useful for special value)
    return a:0 == 0 ? '' : a:1
  endif
endfunction




" Source - wrapper of :source with echo.  "{{{2

command! -bar -nargs=1 Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | source <args>




" SuspendWithAutomticCD  "{{{2

if !exists('s:TMUX_AVAILABLE_P')
  let s:TMUX_AVAILABLE_P = len($TMUX) != 0
endif


command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCD()

function! s:cmd_SuspendWithAutomticCD()
  if s:TMUX_AVAILABLE_P
    let _ = split(system('tmux list-windows'), ':\s\|\s\S\+\n')
    let i = index(_, 'zsh')
    silent execute '!tmux'
    \              (i > -1 ? 'select-window -t '._[i-1] : 'new-window') '\;'
    \              'send-keys C-u "cd' fnameescape(getcwd()) '" C-m'
    redraw!
    let s:TMUX_AVAILABLE_P = (v:shell_error == 0)
  else
    suspend
  endif
endfunction




" CD - wrapper of :cd to keep cwd for each tabpage  "{{{2

command! -complete=file -nargs=? CD
\ call s:tabpage_cd(<q-args>)

function! s:tabpage_cd(dir)
  if strlen(a:dir)
    execute 'cd' fnameescape(a:dir)
  else
    execute 'cd' (strlen(expand('%')) ? fnameescape(expand('%:p:h')) : '')
  endif
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

command! -bang -bar -complete=file -nargs=? Jis  Iso2022jp<bang> <args>
command! -bang -bar -complete=file -nargs=? Sjis  Cp932<bang> <args>




" Utilities  "{{{1
" :grep wrappers  "{{{2

command! -bar -complete=file -nargs=+ Grep  call s:grep('grep', [<f-args>])
command! -bar -complete=file -nargs=+ Lgrep  call s:grep('lgrep', [<f-args>])

function! s:grep(command, args)
  let pattern = a:args[0]
  let file = join(a:args[1:])
  try
    execute a:command '/'.pattern.'/j' file
  catch /^Vim(l\?grep):E303:/
    redraw
    echo 'No matches found:' file
  catch /^Vim(l\?grep):E480:/
    redraw
    echo 'Not match:' pattern
  endtry
  execute a:command == 'lgrep' ? 'lwin' : 'cwin'
endfunction

AlterCommand grep  Grep
AlterCommand lgrep  Lgrep




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




function! s:toggle_option(option_name)  "{{{2
  execute 'setlocal' a:option_name.'!'
  execute 'setlocal' a:option_name.'?'
endfunction




function! s:set_short_indent()  "{{{2
  setlocal expandtab softtabstop=2 shiftwidth=2
endfunction




" Mappings  "{{{1
" Quickfix  "{{{2

" The prefix key.
nnoremap q  <Nop>

" Alternative key for the original action.
nnoremap Q  q


" For quickfix list.
nnoremap <silent> qj  :cnext<CR>
nnoremap <silent> qk  :cprevious<CR>
nnoremap <silent> qr  :Qexecute crewind<CR>
nnoremap <silent> qK  :Qexecute cfirst<CR>
nnoremap <silent> qJ  :Qexecute clast<CR>
nnoremap <silent> qfj  :Qexecute cnfile<CR>
nnoremap <silent> qfk  :Qexecute cpfile<CR>
nnoremap <silent> ql  :<C-u>clist<CR>
nnoremap <silent> qq  :Qexecute cc<CR>
nnoremap <silent> qo  :Qexecute copen<CR>
nnoremap <silent> qc  :<C-u>cclose<CR>
nnoremap <silent> qp  :Qexecute colder<CR>
nnoremap <silent> qn  :Qexecute cnewer<CR>
nnoremap <silent> qm  :<C-u>make<CR>
nnoremap qM  :<C-u>make<Space>
nnoremap q<Space>  :<C-u>make<Space>
nnoremap qg  :<C-u>Grep<Space>


" For location list.
nnoremap <silent> qwj  :lnext<CR>
nnoremap <silent> qwk  :lprevious<CR>
nnoremap <silent> qwr  :Qexecute lrewind<CR>
nnoremap <silent> qwK  :Qexecute lfirst<CR>
nnoremap <silent> qwJ  :Qexecute llast<CR>
nnoremap <silent> qwfj  :Qexecute lnfile<CR>
nnoremap <silent> qwfk  :Qexecute lpfile<CR>
nnoremap <silent> qwl  :<C-u>llist<CR>
nnoremap <silent> qwq  :Qexecute ll<CR>
nnoremap <silent> qwo  :Qexecute lopen<CR>
nnoremap <silent> qwc  :<C-u>close<CR>
nnoremap <silent> qwp  :Qexecute lolder<CR>
nnoremap <silent> qwn  :Qexecute lnewer<CR>
nnoremap <silent> qwm  :<C-u>lmake<CR>
nnoremap qwM  :<C-u>lmake<Space>
nnoremap qw<Space>  :<C-u>lmake<Space>
nnoremap qwg  :<C-u>Lgrep<Space>




" Tab pages  "{{{2

" the prefix key.
nnoremap <C-t>  <Nop>


" Move new tabpage at the last.
nnoremap <silent> <C-t>n  :<C-u>tabnew \| :tabmove<CR>
nnoremap <silent> <C-t>c  :<C-u>tabclose<CR>
nnoremap <silent> <C-t>o  :<C-u>tabonly<CR>
nnoremap <silent> <C-t>i  :<C-u>tabs<CR>

nmap <C-t><C-n>  <C-t>n
nmap <C-t><C-c>  <C-t>c
nmap <C-t><C-o>  <C-t>o
nmap <C-t><C-i>  <C-t>i


" Moving around tabs.
nnoremap <silent> <C-t>j  gt
nnoremap <silent> <C-t>k  gT
nnoremap <silent> <C-t>K  :<C-u>tabfirst<CR>
nnoremap <silent> <C-t>J  :<C-u>tablast<CR>

nmap <C-t><C-j>  <C-t>j
nmap <C-t><C-t>  <C-t>j
nmap <C-t><C-k>  <C-t>k

" GNU screen like mappings.
" Note that the numbers in {lhs}s are 0-origin.  See also 'tabline'.
for i in range(10)
  execute 'nnoremap <silent>' ('<C-t>'.(i))  ((i+1).'gt')
endfor
unlet i


" Moving tabs themselves.
nnoremap <silent> <C-t>l
\ :<C-u>execute 'tabmove' min([tabpagenr() + v:count1 - 1, tabpagenr('$')])<CR>
nnoremap <silent> <C-t>h
\ :<C-u>execute 'tabmove' max([tabpagenr() - v:count1 - 1, 0])<CR>
nnoremap <silent> <C-t>L  :<C-u>tabmove<CR>
nnoremap <silent> <C-t>H  :<C-u>tabmove 0<CR>

nmap <C-t><C-l>  <C-t>l
nmap <C-t><C-h>  <C-t>h




" Command-line editting  "{{{2

cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-a>  <Home>
cnoremap <C-d>  <Delete>

cnoremap <C-p>  <Up>
cnoremap <C-n>  <Down>
cnoremap <Up>  <C-p>
cnoremap <Down>  <C-n>

" Emacs like kill-line.
cnoremap <C-k>  <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>




" Insert mode  "{{{2

inoremap <C-b>  <Left>
inoremap <C-f>  <Right>
inoremap <C-a>  <Home>
inoremap <C-e>  <End>
inoremap <C-d>  <Delete>


" Alternative key for the original action.
inoremap <C-q>  <C-d>
inoremap <C-\>  <C-a>


" Emacs like kill-line.
inoremap <expr> <C-k>  (col('.') == col('$') ? '<C-o>gJ' : '<C-o>D')


" To be able to undo these types of deletion.
inoremap <C-w>  <C-g>u<C-w>
inoremap <C-u>  <C-g>u<C-u>




" Input: datetime  "{{{2

inoremap <Leader>dF  <C-r>=strftime('%Y-%m-%dT%H:%M:%S+09:00')<CR>
inoremap <Leader>df  <C-r>=strftime('%Y-%m-%dT%H:%M:%S')<CR>
inoremap <Leader>dd  <C-r>=strftime('%Y-%m-%d')<CR>
inoremap <Leader>dT  <C-r>=strftime('%H:%M:%S')<CR>
inoremap <Leader>dt  <C-r>=strftime('%H:%M')<CR>




" The <Space>  "{{{2

" to show <Space> in the bottom line.
map <Space> [Space]

" fallback
noremap [Space] <Nop>

nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>
nnoremap <silent> [Space]b  :<C-u>ls<CR>
nnoremap <silent> [Space]m  :<C-u>marks<CR>
nnoremap <silent> [Space]q  :<C-u>help quickref<CR>
nnoremap <silent> [Space]r  :<C-u>registers<CR>

nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]op  :<C-u>call <SID>toggle_option('paste')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>


" Close fold.
nnoremap [Space]h  zc
nnoremap [Space]H  zM

" Open fold.
nnoremap [Space]l  zo
nnoremap [Space]L  zR

" Close all folds but including the cursor.
nnoremap [Space]v  zMzv


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




" Text objects  "{{{2

" Angle
vnoremap aa  a>
onoremap aa  a>
vnoremap ia  i>
onoremap ia  i>

" Rectangle
vnoremap ar  a]
onoremap ar  a]
vnoremap ir  i]
onoremap ir  i]

" Quote
vnoremap aq  a'
onoremap aq  a'
vnoremap iq  i'
onoremap iq  i'

" Double quote
vnoremap ad  a"
onoremap ad  a"
vnoremap id  i"
onoremap id  i"




" Operators  "{{{2

" User key mappings will be defined later - see [Space].
call operator#user#define_ex_command('my-sort', 'sort')




" Misc.  "{{{2

nnoremap <Leader><Leader>  :<C-u>update<CR>
nnoremap <C-h>  :<C-u>help<Space>
nnoremap <C-o>  :<C-u>edit<Space>
nnoremap <C-w>.  :<C-u>edit .<CR>
nnoremap <C-z>  :<C-u>SuspendWithAutomticCD<CR>


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


" Complete or indent.
inoremap <expr> <C-i>  (<SID>should_indent_rather_than_complete_p()
                      \ ? '<C-i>'
                      \ : <SID>keys_to_complete())

function! s:should_indent_rather_than_complete_p()
  let m = match(getline('.'), '\S')
  return m == -1 || col('.')-1 <= m
endfunction

function! s:keys_to_complete()
  if &l:filetype ==# 'vim'
    return "\<C-x>\<C-v>"
  elseif strlen(&l:dictionary)
    return "\<C-x>\<C-k>"
  elseif strlen(&l:omnifunc)
    return "\<C-x>\<C-o>"
  else
    return "\<C-n>"
  endif
endfunction


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
    return "\<Esc>o" . repeat("\<Return>", v:count)
  else  " a:command ==# 'O'
    return "\<Esc>OX\<Esc>m'o" . repeat("\<Return>", v:count-1) . "\<Esc>''S"
  endif
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
  " Load the dictionary for filetype now.
  let dict = expand('$HOME/.vim/dict/').&l:filetype.'.dict'
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


" Unset 'paste' automatically.  It's often hard to do so because of most
" mappings are disabled in Paste mode.
autocmd MyAutoCmd InsertLeave *
\ set nopaste




" c,cpp  "{{{2

if has('win32unix')
  autocmd MyAutoCmd FileType c,cpp
  \ setlocal dictionary+=$HOME/.vim/dict/winapi.dict
endif




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




" go  "{{{2

autocmd MyAutoCmd BufNewFile,BufRead *.go
\ set filetype=go




" java  "{{{2

autocmd MyAutoCmd FileType java
\ call s:on_FileType_java()

function! s:on_FileType_java()
  setlocal makeprg=javac\ -Xlint:deprecation\ %
  setlocal errorformat=%E%f:%l:\ %m,%C%\\S%\\+:\ %.%#\ %m,%Z%p^,%C%.%#
  setlocal cinoptions=:0,t0,(0,W1s

  nnoremap <silent> <LocalLeader>a  :<C-u>QuickRun java-applet -mode n<CR>
  vnoremap <silent> <LocalLeader>a  :<C-u>QuickRun java-applet -mode v<CR>
endfunction




" lua  "{{{2

autocmd MyAutoCmd FileType lua
\ call s:set_short_indent()




" markdown  "{{{2

autocmd MyAutoCmd BufRead,BufNewFile *.mkd
\   setfiletype mkd
\ | call s:set_short_indent()




" perl  "{{{2

autocmd MyAutoCmd FileType perl
\ call s:set_short_indent()




" registry  "{{{2

autocmd MyAutoCmd FileType registry
\ call s:on_FileType_registry()

function! s:on_FileType_registry()
  " Fix the default syntax to properly highlight
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




" tex  "{{{2

autocmd MyAutoCmd FileType tex,plaintex
\   call s:set_short_indent()
\ | setlocal makeprg=platex\ --kanji=utf8\ -shell-escape\ -file-line-error\ -interaction=nonstopmode\ %
\ | setlocal errorformat=%f:%l:\ %m




" vim  "{{{2

autocmd MyAutoCmd FileType vim
\ call s:on_FileType_vim()

function! s:on_FileType_vim()
  call s:set_short_indent()
  nnoremap <buffer> <silent> K  :<C-u>help <C-r><C-w><CR>

  " Fix the default syntax to properly highlight.
  syntax clear vimFunc
  syntax match vimFunc
  \ "\%([sS]:\|<[sS][iI][dD]>\|\<\%(\I\i*[#.]\)\+\)\=\I\i*\ze\s*("
  \ contains=vimFuncName,vimUserFunc,vimCommand,vimNotFunc,vimExecute

  syntax clear vimUserFunc
  syntax match vimUserFunc contained
  \ "\%([sS]:\|<[sS][iI][dD]>\|\<\%(\I\i*[#.]\)\+\)\i\+\|\<\u\i*\>\|\<if\>"
  \ contains=vimNotation,vimCommand

  syntax clear vimCmplxRepeat
  syntax cluster vimFuncList
  \ remove=vimFunctionError
endfunction

let g:vim_indent_cont = 0




" xml  "{{{2

autocmd MyAutoCmd FileType html,xhtml,xml,xslt
\ call s:on_FileType_xml()

function! s:on_FileType_xml()
  call s:set_short_indent()
  inoremap <buffer> </  </<C-x><C-o>
endfunction




" Plugins  "{{{1
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

nnoremap <silent> [Space]ka  :<C-u>Ku args<CR>
nnoremap <silent> [Space]kb  :<C-u>Ku buffer<CR>
nnoremap <silent> [Space]kf  :<C-u>Ku file<CR>
nnoremap <silent> [Space]kg  :<C-u>Ku metarw/git<CR>
nnoremap <silent> [Space]kh  :<C-u>Ku history<CR>
nnoremap <silent> [Space]kq  :<C-u>Ku quickfix<CR>
nnoremap <silent> [Space]ks  :<C-u>Ku source<CR>

nnoremap <silent> [Space]km  :<C-u>Ku file_mru<CR>
nnoremap <silent> [Space]k:  :<C-u>Ku cmd_mru/cmd<CR>
nnoremap <silent> [Space]k/  :<C-u>Ku cmd_mru/search<CR>


autocmd MyAutoCmd FileType ku
\   call ku#default_key_mappings(s:TRUE)
\ | call s:ku_my_key_mappings()

function! s:ku_my_key_mappings()
  imap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
  nmap <buffer> <silent> <Esc><Esc>  <Plug>(ku-cancel)
endfunction


function! s:ku_common_action_yank(item)
  call setreg('"', a:item.word, 'c')
endfunction
function! s:ku_common_action_Yank(item)
  call setreg('"', a:item.word, 'l')
endfunction

call ku#custom_action('common', 'yank', s:SID_PREFIX().'ku_common_action_yank')
call ku#custom_action('common', 'Yank', s:SID_PREFIX().'ku_common_action_Yank')

call ku#custom_key('common', 'y', 'yank')
call ku#custom_key('common', 'Y', 'Yank')
call ku#custom_key('buffer', 'd', 'delete')

call ku#custom_prefix('common', '~', $HOME)
call ku#custom_prefix('common', '.vim', $HOME.'/.vim')
call ku#custom_prefix('common', 'VIM', $VIMRUNTIME)


let g:ku_file_mru_file = expand('$HOME/.vim/info/ku/mru')
let g:ku_file_mru_limit = 256
let g:ku_file_mru_ignore_pattern = '/$\|^/usr/portage/\|^/cygdrive/'




" narrow  "{{{2

noremap [Space]xn  :Narrow<CR>
noremap [Space]xw  :<C-u>Widen<CR>




" operator-replece  "{{{2

map _  <Plug>(operator-replace)




" quickrun  "{{{2

nnoremap <silent> <Leader>R  :<C-u>QuickRun >! -runmode simple -mode n<CR>
vnoremap <silent> <Leader>R  :<C-u>QuickRun >! -runmode simple -mode v<CR>


let g:quickrun_config = {
\  '*': {
\    'output_encode': '',
\    'split': '{winwidth(0) * 2 < winheight(0) * 5 ? "botright" : "botright vertical"}',
\  },
\  'c': {
\    'command': 'gcc',
\    'exec': ['%c %s -o %s:p:r', '%s:p:r %a'],
\    'tempfile': '{tempname()}.c',
\  },
\  'cpp': {
\    'command': 'g++',
\    'exec': ['%c %s -o %s:p:r', '%s:p:r %a'],
\    'tempfile': '{tempname()}.cpp',
\  },
\  'go': {
\    'command': '8g',
\    'exec': ['%c %s', '8l -o %s:p:r %s:p:r.8', '%s:p:r %a'],
\  },
\  'java': {
\    'exec': ['javac -Xlint:deprecation %s', '%c %s:t:r %a'],
\    'tempfile': '{fnamemodify(tempname(), ":p:h")}/{expand("%:t")}',
\  },
\  'java-applet': {
\    'exec': ['echo "<applet code=%s:t:r width=500 height=500></applet>" > %s:p:r.html',
\             'appletviewer %s:p:r.html',
\             'rm -f %s:p:r.html'],
\    'tempfile': '{fnamemodify(tempname(), ":p:h")}/{expand("%:t")}',
\  },
\  'mkd': {
\    'exec': ['markdown.pl %s | tee %s:p:r.html'],
\    'tempfile': '{fnamemodify(tempname(), ":p:h")}/{expand("%:t")}',
\  },
\  'tex': {
\    'exec': ['platex --kanji=utf8 -shell-escape -file-line-error -output-directory=%s:p:h %s',
\             'pxdvi %s:p:r'],
\  },
\  'vim': {
\    'runmode': 'simple',
\  },
\  'xdefaults': {
\    'runmode': 'simple',
\    'exec': ['xrdb -remove',
\             'xrdb -merge %s:p:r'],
\  },
\}


" Use async processing if possible.
autocmd MyAutoCmd VimEnter *
\   if has('clientserver') && strlen(v:servername) && exists('*vimproc#popen2')
\ |   let g:quickrun_config['*']['runmode'] = 'async:remote:vimproc'
\ | endif




" ref  "{{{2

map K <Plug>(ref-keyword)

autocmd MyAutoCmd FileType ref
\   map <buffer> <C-]> <Plug>(ref-keyword)
\ | map <buffer> <C-j> <Plug>(ref-forward)
\ | map <buffer> <C-k> <Plug>(ref-back)


let g:ref_no_default_key_mappings = 1
let g:ref_cache_dir = expand('$HOME/.vim/info/ref')
let g:ref_open = 'botright vsplit'




" scratch  "{{{2

nmap <Leader>s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\   map <buffer> <CR>  <Plug>(scratch-evaluate)
\ | map <buffer> <C-m>  <Plug>(scratch-evaluate)
\ | map <buffer> <C-j>  <Plug>(scratch-evaluate)




" smartword  "{{{2

map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)




" surround  "{{{2

" The default mapping ys for <Plug>Ysurround is not consistent with
" the default mappings of vi -- y is for yank.
nmap s  <Plug>Ysurround
nmap ss  <Plug>Yssurround




" wwwsearch  "{{{2

nnoremap [Space]*  :<C-u>Wwwsearch -default <C-r><C-w><CR>


let g:wwwsearch_command_to_open_uri = 'w3m {uri}'

call wwwsearch#add('default', 'http://www.google.com/search?hl=ja&lr=&q={keyword}')
call wwwsearch#add('google', 'http://www.google.com/search?hl=ja&lr=&q={keyword}')
call wwwsearch#add('wikipedia', 'http://ja.wikipedia.org/wiki/{keyword}')
call wwwsearch#add('dictionary', 'http://dictionary.goo.ne.jp/srch/all/{keyword}/m0u/')




" Fin.  "{{{1

if !exists('s:loaded_my_vimrc')
  let s:loaded_my_vimrc = 1
endif


" must be written at the last.  see :help 'secure'.
set secure




" __END__  "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
