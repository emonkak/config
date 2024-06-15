" My init.vim
" Basic  {{{1
" Absolute  {{{2

if has('vim_starting')
  if has('nvim') && isdirectory('/usr/share/vim/vimfiles')
    set runtimepath^=/usr/share/vim/vimfiles
    set runtimepath+=/usr/share/vim/vimfiles/after
  endif
endif

set ambiwidth=single
set fileencodings=ucs-bom,utf-8,iso-2022-jp,euc-jp,cp932
set fileformats=unix,dos,mac

filetype plugin indent on

function! s:SID() abort
  return matchstr(get(function('s:SID'), 'name'), '^<SNR>\zs\d\+')
endfunction
let s:SID_PREFIX = '<SNR>' . s:SID() . '_'

" Syntax  {{{2

let g:ansi_colors = {
\   'background': '#23262e',
\   'foreground': '#d5dae9',
\   'black': '#38425a',
\   'red': '#cf6950',
\   'green': '#1c9969',
\   'yellow': '#aa7f2c',
\   'blue': '#8075f5',
\   'magenta': '#d95a88',
\   'cyan': '#3790b3',
\   'white': '#7486b2',
\   'bright-black': '#465476',
\   'bright-red': '#e9b5a8',
\   'bright-green': '#8dcfad',
\   'bright-yellow': '#d9bd95',
\   'bright-blue': '#c5b8fb',
\   'bright-magenta': '#f1afc2',
\   'bright-cyan': '#9cc7dd',
\   'bright-white': '#b8c0da',
\ }

if (1 < &t_Co || has('gui')) && has('syntax') && !exists('g:syntax_on')
  syntax enable
  if !exists('g:colors_name')
    colorscheme ansi_colors
  endif
endif

" Options  {{{2

if has('gui_running')
  if has('gui_gtk')
    set guifont=Monospace\ 10
    set linespace=0
  elseif has('gui_macvim')
    set guifont=Monaco:h12
    set linespace=1
    set macmeta
    set visualbell
  elseif has('gui_win32')
    set guifont=Consolas:h10.5
    set guifontwide=Meiryo:h10.5
    set renderoptions=type:directx,renmode:5
    set linespace=1
  endif
  set guicursor=a:blinkwait4000-blinkon1500-blinkoff500
  set guioptions=AcgM
endif

set autoindent
set backspace=indent,eol,start
if exists('+breakindent')
  set breakindent
endif
set cinoptions=l1,g0,t0,W1s
if exists('+clipboard')
  if &clipboard =~# '\<autoselect\>'
    set clipboard& clipboard-=autoselect clipboard+=autoselectml
  endif
endif
set cmdheight=1
set complete& complete-=i complete-=t
set completeopt=menuone,longest
set concealcursor=nc
set conceallevel=0
set confirm
set diffopt=filler,vertical
if &directory =~# '^\.'
  set directory-=. directory=~/.local/state/vim/swap
endif
set display=lastline
set noequalalways
set foldmethod=marker
set formatlistpat&
if &formatlistpat != ''
  let &formatlistpat .= '\|^\s*[*+-]\s*'
endif
set formatoptions=roqnlmM1
set grepformat^=%f:%l:%c:%m
set hidden
set history=1000
set ignorecase
set incsearch
set keywordprg=:help
if exists('+langremap')
  set nolangremap
endif
set laststatus=2
set linebreak
set list
set listchars=tab:>\\xb7,trail:\\xb7,extends:>,precedes:<,conceal:\\xa6,nbsp:+
set mouse=
set nrformats-=octal
set pumheight=12
set scrolloff=5
if exists('+shellslash')
  set shellslash
endif
set shiftround
set showcmd
set smartcase
set smartindent
set splitbelow
set splitright
set synmaxcol=1000
if exists('+tagcase')
  set tagcase=match
endif
if exists('+termguicolors')
  set notermguicolors
endif
set title
set titlestring=%{v:progname}:\ %f%(\ %h%r%m%)
set ttimeoutlen=50
if &undodir ==# '.'
  set undodir=~/.local/state/vim/undo
endif
set undofile
set updatetime=1000
set viminfo^='1000 viminfo-='100
set virtualedit=block
set wildmenu
set wildmode=full
set nowrapscan
set nowritebackup

" Status-line  {{{2

let &statusline = '%<%f %h%m%r%w'
\               . '%='
\               . '['
\               . '%{&l:fileencoding == "" ? &encoding : &l:fileencoding}'
\               . '%{&l:bomb ? "/BOM" : ""}'
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
let &tabline = '%!' . s:SID_PREFIX . 'my_tabline()'

" Misc.  {{{2

augroup MyAutoCmd
  autocmd!
augroup END

let g:mapleader = ','
let g:maplocalleader = '.'

if !has('nvim')
  if has('patch-8.0.1398')
    " In this version, :packadd look in the "start" directory.
    packadd vim-altercmd
    packadd vim-arpeggio
  endif
  if !has('patch-8.2.4275')
    " Add all "start" packages to 'runtimepath' explicitly, because Vim of
    " this version does not autoload sources from 'packpath'.
    let &runtimepath .= ','
    \                . join(globpath(&packpath, 'pack/*/start/*', 0, 1), ',')
  endif
endif

call altercmd#load()
call arpeggio#load()

" Utilities  "{{{1
function! s:complete_cdpath(arglead, cmdline, cursorpos) abort  "{{{2
  return map(globpath(substitute(&cdpath, '^,', '', ''),
  \                   join(split(a:cmdline, '\s', 1)[1:], ' ') . '*/',
  \                   0,
  \                   1),
  \          'v:val[:-2]')
endfunction

function! s:complete_file(arglead, cmdline, cursorpos) abort  "{{{2
  return glob(join(split(a:cmdline, '\s', 1)[1:], ' ') . '*',
  \           0,
  \           1)
endfunction

function! s:readdir(dir) abort  "{{{2
  if exists('*readdir')
    try
      return readdir(a:dir)
    catch /\<E484:/
      return []
    endtry
  else
    let paths = []
    call extend(paths, globpath(a:dir, '.*', 1, 1))
    call extend(paths, globpath(a:dir, '*', 1, 1))
    call map(paths, 'fnamemodify(v:val, ":t")')
    call filter(paths, 'v:val !~# "^\\.\\{1,2}$"')
    return paths
  endif
endfunction

function! s:sandbox_eval(expr) abort  "{{{2
  sandbox let result = eval(a:expr)
  return result
endfunction

function! s:trim(text) abort  "{{{2
  if exists('*trim')
    return trim(a:text)
  else
    return substitute(a:text, '^\s*\|\s*$', '', 'g')
  endif
endfunction

" Commands  {{{1
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

" :grep wrappers  {{{2

command! -complete=customlist,s:complete_file -nargs=+ Grep
\ call s:grep('grep', 'cwindow', [<f-args>])
command! -complete=customlist,s:complete_file -nargs=+ Lgrep
\ call s:grep('lgrep', 'lwindow', [<f-args>])

function! s:grep(command, window_command, args) abort
  let pattern = escape(shellescape(a:args[-1]), '#%|')
  let target = len(a:args) > 1
  \          ? escape(shellescape(join(a:args[:-2], ' ')), '|')
  \          : ''
  execute (a:command . '!') pattern target
  execute a:window_command
endfunction

AlterCommand gr[ep]  Grep
AlterCommand lgr[ep]  LGrep

let s:GREPPRGS = [
\   executable('rg') ? 'rg --vimgrep --no-heading' : 'grep -nHE',
\   'git grep --line-number --column',
\ ]

function! s:toggle_grepprg() abort
  let i = (index(s:GREPPRGS, &l:grepprg) + 1) % len(s:GREPPRGS)
  let &l:grepprg = s:GREPPRGS[i]
  echo &l:grepprg
endfunction

function! s:detect_optimal_greprg() abort
  let git_dir = finddir('.git', expand('%:p:h') . ';')
  return s:GREPPRGS[git_dir != '' ? 1 : 0]
endfunction

autocmd MyAutoCmd BufEnter *
\   if &l:grepprg == ''
\ |   let &l:grepprg = s:detect_optimal_greprg()
\ | endif

" :make wrappers  {{{2

command! -complete=file -nargs=* Make
\ call s:make('make', 'cwindow', [<f-args>])
command! -complete=file -nargs=* Lmake
\ call s:make('lmake', 'lwindow', [<f-args>])

function! s:make(command, window_command, args) abort
  let original_winnr = winnr()

  execute a:command . '!' join(a:args)
  execute a:window_command

  execute original_winnr 'wincmd w'
endfunction

AlterCommand mak[e]  Make
AlterCommand lmak[e]  Lmake

" BufSweap  {{{2

command! -bang -bar -nargs=0 BufSweap  call s:cmd_BufSweap(<bang>0)

function! s:cmd_BufSweap(banged) abort
  let bufnrs = range(1, bufnr('$'))
  call filter(bufnrs, 'bufexists(v:val)
  \                 && buflisted(v:val)
  \                 && (bufname(v:val) == "" || !filereadable(bufname(v:val)))
  \                 && (a:banged || !getbufvar(v:val, "&modified"))')
  for bufnr in bufnrs
    silent execute bufnr 'bdelete' . (a:banged ? '!' : '')
  endfor
  echo len(bufnrs) 'buffer(s) are deleted'
endfunction

" CD  {{{2

command! -bang -bar -nargs=* -complete=customlist,s:complete_cdpath CD
\ call s:cmd_CD(exists(':tcd') ? 'tcd' : 'lcd', <q-args>)

function! s:cmd_CD(cd_command, path) abort
  if a:path != ''
    execute a:cd_command fnameescape(a:path)
  else
    let buffer_dir = expand('%:p:h')
    if isdirectory(buffer_dir)
      let git_dir = finddir('.git', buffer_dir . ';')
      let new_cwd = git_dir != ''
      \ ? fnamemodify(git_dir, ':p:h:h')
      \ : buffer_dir
      execute a:cd_command fnameescape(new_cwd)
    else
      execute a:cd_command
    endif
  endif
  let t:cwd = getcwd(-1)
  pwd
endfunction

AlterCommand cd  CD

if exists(':tcd')
  autocmd MyAutoCmd BufEnter ?*
  \   if !exists('t:cwd') && getbufvar('%', '&buftype') == ''
  \ |   silent CD
  \ | endif
endif

" FoldDump  {{{2

command! -bar -range=% FoldDump
\ <line1>,<line2>global/^/echo
\ printf("%*d (%d) [%2s] %s",
\   len(line('$')),
\   line('.'),
\   foldlevel('.'),
\   eval(substitute(&l:foldexpr, '\<v:lnum\>', line('.'), '')),
\   getline('.')
\ ) | nohlsearch

" Indent styles  {{{2

command! -bar -nargs=1 SpaceIndent
\ setlocal expandtab shiftwidth=<args> softtabstop=<args> tabstop<

command! -bar -nargs=1 TabIndent
\ setlocal noexpandtab shiftwidth< softtabstop< tabstop=<args>

" Jsonify  {{{2

command! -nargs=? -range Jsonify
\   if <q-args> != ''
\ |   echo s:pretty_print(json_decode(s:trim(<q-args>)), 0, 2, function('json_encode'))
\ | else
\ |   call s:prettify(visualmode(), 2, function('json_decode'), function('json_encode'))
\ | endif

function! s:prettify(visual_command, indent_width, Decode, Encode) abort
  let reg_value = getreg('"')
  let reg_type = getregtype('"')
  try
    execute 'normal!' ('`<' . a:visual_command . '`>""y')
    let @" = s:pretty_print(a:Decode(s:trim(@")), 0, a:indent_width, a:Encode)
    execute 'normal!' ('`<' . a:visual_command . '`>""p')
  finally
    call setreg('"', reg_value, reg_type)
  endtry
endfunction

function! s:pretty_print(value, depth, indent_width, Encode) abort
  let type = type(a:value)
  if type is v:t_list
    if empty(a:value)
      return '[]'
    endif
    let current_indent = repeat(' ', a:depth * a:indent_width)
    let next_indent = repeat(' ', (a:depth + 1) * a:indent_width)
    let len = len(a:value)
    let values = map(
    \   a:value,
    \   { i, value -> next_indent
    \                 . s:pretty_print(value, a:depth + 1, a:indent_width, a:Encode)
    \                 . (i + 1 == len ? '' : ',')
    \   }
    \ )
    return "[\n"
    \      . join(values, "\n") . "\n"
    \      . current_indent . ']'
  elseif type is v:t_dict
    if empty(a:value)
      return '{}'
    endif
    let current_indent = repeat(' ', a:depth * a:indent_width)
    let next_indent = repeat(' ', (a:depth + 1) * a:indent_width)
    let len = len(a:value)
    let values = map(
    \   items(a:value),
    \   { i, val ->
    \     next_indent . a:Encode(val[0]) . ': '
    \                 . s:pretty_print(val[1], a:depth + 1, a:indent_width, a:Encode)
    \                 . (i + 1 == len ? '' : ',')
    \   }
    \ )
    return "{\n"
    \      . join(values, "\n") . "\n"
    \      . current_indent . '}'
  else
    return a:Encode(a:value)
  endif
endfunction

" Note  {{{2

command! -bar -nargs=? Note
\ execute printf(
\   'edit %s/Sync/Documents/Notes/%s%s.md',
\   $HOME,
\   strftime('%Y-%m-%d'),
\   <q-args> != '' ? '_' . <q-args> : ''
\ )

" Remote commands  {{{2

command! -bar -nargs=1 -complete=customlist,s:complete_server_name
\ RemotePut read! ssh <args> pbpaste

command! -bar -nargs=1 -range -complete=customlist,s:complete_server_name
\ RemoteYank
\ call system(['ssh', <q-args>, 'pbcopy'], getline(<line1>, <line2>))

function! s:complete_server_name(arglead, cmdline, cursorpos) abort
  let path = expand('$HOME/.ssh/known_hosts')
  try
    let server_names = readfile(path, '')
  catch /\<E484:/
    return []
  endtry
  call map(server_names, { _, line -> matchstr(line, '^\S\+') })
  call sort(server_names)
  call uniq(server_names)
  call filter(server_names, { _, name -> stridx(name, a:arglead) == 0 })
  return server_names
endfunction

" Rename  {{{2

command! -bar -complete=file -nargs=1 Rename  call s:cmd_Rename(<q-args>)

function! s:cmd_Rename(new_path) abort
  let old_path = expand('%')
  if &l:readonly || (filereadable(old_path) && !filewritable(old_path))
    echohl ErrorMsg
    echo 'This buffer is read-only'
    echohl None
  elseif filereadable(a:new_path)
    echohl ErrorMsg
    echo 'A file already exists at the destination' string(a:new_path)
    echohl None
  else
    let new_dir = fnamemodify(a:new_path, ':p:h')
    if !isdirectory(new_dir)
      call mkdir(new_dir, 'p')
    endif
    file `=fnamemodify(a:new_path, ':.')`
    silent write
    call delete(old_path)
    let old_dir = fnamemodify(old_path, ':p:h')
    if empty(s:readdir(old_dir))
      call delete(old_dir, 'd')
    endif
    echo 'Renamed:' old_path '->' a:new_path
  endif
endfunction

" Reverse  {{{2

command! -bar -range=% Reverse
\   <line1>,<line2>global/^/m<line1>-1
\ | nohlsearch

" Source  {{{2

command! -bar -complete=file -nargs=? -range Source
\   echo 'Sourcing ...' expand(<q-args>)
\ | if <q-args> != ''
\ |   source <args>
\ | else
\ |   <line1>,<line2> source
\ | endif

AlterCommand so[urce]  Source

" Sum  {{{2

command! -bar -range Sum
\ <line2>put =string(s:sum_lines(<line1>, <line2>))

function! s:sum_lines(start_lnum, end_lnum) abort
  let total = 0
  let ns = []

  for lnum in range(a:start_lnum, a:end_lnum)
    let n = s:extract_number(getline(lnum))
    let total += n
    call add(ns, n)
  endfor

  echo join(ns, ' + ') '=' total

  return total
endfunction

function! s:extract_number(line) abort
  let n = matchstr(a:line,
  \                '\c\(0\(x\x\+\|b[01]\+\|o\?[0-7]\+\)\)\|\d\+\(\.\d\+\)\?')
  if n == ''
    return 0
  endif
  if stridx(n, '.') >= 0
    return str2float(n)
  endif
  if n[0:1] == '0x'
    let base = 16
  elseif n[0:1] == '0b'
    let base = 2
  elseif n[0] == '0'
    let base = 8
  else
    let base = 10
  endif
  return str2nr(n, base)
endfunction

" SuspendWithAutomticCD  {{{2

command! -bar -nargs=0 SuspendWithAutomticCD
\ call s:cmd_SuspendWithAutomticCD()

function! s:cmd_SuspendWithAutomticCD() abort
  if exists('$TMUX')
    let shell = split(&shell, '/')[-1]
    let select_command = 'new-window'
    let target_pane = ':$'
    for window in split(system('tmux list-windows'), '\n')
      let matches = matchlist(window, '^\(\d\+\):\s\(\w\+\)')
      if !empty(matches) && matches[2] ==# shell
        let select_command = 'select-window -t ' . matches[1]
        let target_pane = ':' . matches[1]
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

" SyntaxStack {{{2

command! -bar -nargs=0 SyntaxStack
\ echo join(<SID>syntax_stack(line('.'), col('.')), '/')

function! s:syntax_stack(line, col) abort
  let stack = []

  for syn_id in synstack(a:line, a:col)
    let name = synIDattr(syn_id, 'name')
    let syn_trans_id = synIDtrans(syn_id)

    if syn_id != syn_trans_id
      let name .= '<' . synIDattr(syn_trans_id, 'name') . '>'
    endif

    call add(stack, name)
  endfor

  return stack
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
\ execute (winwidth(0) > winheight(0) * 4 ? 'vertical' : '') <q-args>

AlterCommand sp[lit]  Split
AlterCommand h[elp]  Help
AlterCommand new  New

" Mappings  {{{1
" QuickFix  {{{2

noremap q  <Nop>
nnoremap Q  q

" For quickfix list.
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

" For location list.
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

" Moving around tab pages.
nnoremap <silent> <C-t>j
\ :<C-u>execute 'tabnext' 1 + (tabpagenr() + v:count1 - 1) % tabpagenr('$')<CR>
nnoremap <silent> <C-t>k
\ :<C-u>execute 'tabprevious' v:count1 % tabpagenr('$')<CR>
nnoremap <silent> <C-t>K  :<C-u>tabfirst<CR>
nnoremap <silent> <C-t>J  :<C-u>tablast<CR>

nmap <C-t><C-j>  <C-t>j
nmap <C-t><C-k>  <C-t>k
nmap <C-t><C-t>  <C-t>j

" Moveing the specific tag page.
for s:i in range(10)
  execute 'nnoremap <silent>' ('<C-t>'.(s:i))  ((s:i+1).'gt')
endfor
unlet s:i

" Moving tabpages them selves.
nnoremap <silent> <C-t>l  :<C-u>execute 'tabmove' '+'.v:count1<CR>
nnoremap <silent> <C-t>h  :<C-u>execute 'tabmove' '-'.v:count1<CR>
nnoremap <silent> <C-t>L  :<C-u>tabmove<CR>
nnoremap <silent> <C-t>H  :<C-u>tabmove 0<CR>

nmap <C-t><C-l>  <C-t>l
nmap <C-t><C-h>  <C-t>h

" Argument list  {{{2

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

" Basic mappings like GNU Emacs.
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-a>  <Home>
cnoremap <C-d>  <Delete>
cnoremap <expr> <C-y>  pumvisible() ? "\<C-y>" : "\<C-r>\""
cnoremap <C-k>
\ <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>
cnoremap <expr> <C-u>  getcmdline() == '' ? "\<C-c>" : "\<C-u>"
cnoremap <expr> <C-w>  getcmdline() == '' ? "\<C-c>" : "\<C-w>"

" Swap <C-p>, <C-n> to <Up>, <Down>.
cnoremap <C-p>  <Up>
cnoremap <C-n>  <Down>
cnoremap <Up>  <C-p>
cnoremap <Down>  <C-n>

" An alternative key to open the command-line history.
cnoremap <C-o>  <C-f>

" Escape the slash character when command-line type is 'search'.
cnoremap <expr> /  index(['/', '?'], getcmdtype()) >= 0 ? '\/' : '/'

" When you type "s<Space>", it's expanded to "%s//|/g".
cnoreabbrev <expr> s
\ [getcmdtype(), getcmdline()] ==# [':', 's']
\ ? '%s///g<Left><Left>' . getchar()[1:0]
\ : 's'

" Command-line window  {{{2

autocmd MyAutoCmd CmdwinEnter *
\ call s:on_CmdwinEnter()

function! s:on_CmdwinEnter() abort
  " Close the command-line window.
  nnoremap <buffer> <Esc>  <Esc><C-w>q

  " Close the command-line window if the current line is empty.
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

" Basic mappings like GNU Emacs.
inoremap <C-b>  <Left>
inoremap <C-f>  <Right>
inoremap <C-d>  <Delete>
inoremap <expr> <C-a>  indent('.') + 1 < virtcol('.') ? "\<C-o>^" : "\<Home>"
inoremap <expr> <C-e>  indent('.') < virtcol('.') ? "\<End>" : "\<C-o>^"
inoremap <expr> <C-k>  col('.') == col('$') ? "\<Delete>" : "\<C-o>D"
inoremap <expr> <C-y>  pumvisible() ? "\<C-y>" : "\<C-r>\""

" Alternative keys for original actions.
inoremap <C-\>  <C-a>
inoremap <C-q>  <C-d>

" Enable undo for those deletion mappings.
inoremap <C-w>  <C-g>u<C-w>
inoremap <C-u>  <C-g>u<C-u>

" Do complete or indent.
inoremap <expr> <Tab>  pumvisible()
                     \ ? "\<C-n>"
                     \ : <SID>should_complete_rather_than_indent()
                     \ ? <SID>keys_to_complete()
                     \ : "\<C-i>"
inoremap <expr> <S-Tab>  pumvisible()
                       \ ? "\<C-p>"
                       \ : <SID>should_complete_rather_than_indent()
                       \ ? <SID>keys_to_complete()
                       \ : "\<C-i>"

function! s:should_complete_rather_than_indent() abort
  return search('\S\%#', 'Wbn') > 0
endfunction

function! s:keys_to_complete() abort
  if &l:omnifunc != ''
    return "\<C-x>\<C-o>"
  elseif &l:completefunc != ''
    return "\<C-x>\<C-u>"
  else
    return "\<C-n>"
  endif
endfunction

" The <Space>  {{{2

map <Space>  [Space]
noremap [Space]  <Nop>

nnoremap <silent> [Space]w  :<C-u>w<CR>
nnoremap <silent> [Space]W  :<C-u>SudaWrite<CR>

" Close temporary windows such as 'quickfix' or 'help'.
nnoremap <silent> [Space]c  :<C-u>call <SID>close_temporary_windows()<CR>

function! s:close_temporary_windows() abort
  let last_winnr = winnr('$')
  if last_winnr <= 1
    return
  endif
  for winnr in range(last_winnr, 1, -1)
    let bufnr = winbufnr(winnr)
    if win_gettype(winnr) != '' ||
    \ (!buflisted(bufnr) && getbufvar(bufnr, '&buftype') != '')
      execute winnr 'wincmd w'
      wincmd c
    endif
  endfor
endfunction

nnoremap [Space]o  <Nop>
nnoremap <silent> [Space]oc  :<C-u>call <SID>toggle_colorcolumn()<CR>
nnoremap <silent> [Space]ol  :<C-u>call <SID>toggle_option('cursorline')<CR>
nnoremap <silent> [Space]og  :<C-u>call <SID>toggle_grepprg()<CR>
nnoremap <silent> [Space]on  :<C-u>call <SID>toggle_option('number')<CR>
nnoremap <silent> [Space]op  :<C-u>call <SID>toggle_option('paste')<CR>
nnoremap <silent> [Space]os  :<C-u>call <SID>toggle_option('spell')<CR>
nnoremap <silent> [Space]ow  :<C-u>call <SID>toggle_option('wrap')<CR>
nnoremap <silent> [Space]/  :<C-u>call <SID>toggle_option('hlsearch')<CR>

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
      setlocal textwidth=80
    endif
    setlocal colorcolumn=+1 colorcolumn?
  endif
endfunction

nnoremap [Space]f  <Nop>
nnoremap [Space]fe  :<C-u>setlocal fileencoding=
nnoremap [Space]ff  :<C-u>setlocal fileformat=
nnoremap [Space]fi  :<C-u>setlocal expandtab? shiftwidth? softtabstop?<CR>
nnoremap [Space]fr  :<C-\>esetcmdpos(8 + strlen(expand('%:p:r'))) ? '' : 'Rename ' . expand("%:p")<CR>
nnoremap [Space]fs  :<C-u>setlocal filetype? fileencoding? fileformat?<CR>
nnoremap [Space]ft  :<C-u>setfiletype<Space>

nnoremap [Space]l  zo
nnoremap [Space]h  zc
nnoremap [Space]H  zM
nnoremap [Space]v  zMzv

nnoremap <silent> [Space].  :<C-u>Source $MYVIMRC<CR>
nnoremap <silent> [Space]%  :<C-u>Source %<CR>

" Put the clipboard content.
nnoremap [Space]p  "+p

" Enter the command-line window.
nnoremap [Space]:  q:
xnoremap [Space]:  q:

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

function! s:ask_tabpage_number() abort
  echon 'Which tabpage to move this window into? '

  let c = nr2char(getchar())
  if c =~# '[0-9]'
    return 1 + char2nr(c) - char2nr('0')
  else
    return 0
  endif
endfunction

function! s:move_window_into_tabpage(target_tabpagenr) abort
  " Move the current window into a:target_tabpagenr.
  if a:target_tabpagenr <= 0  " ignore invalid number
    return
  endif
  let original_tabnr = tabpagenr()
  let target_bufnr = bufnr('')
  let window_view = winsaveview()

  if a:target_tabpagenr > tabpagenr('$')
    tabnew
    tabmove  " Move new tabpage at the last
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

nnoremap <C-w>Q  :<C-u>quit!<CR>

" Marks  {{{2

let s:MARK_CHARACTERS = [
\    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
\    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
\ ]

nnoremap <expr> [Space]<Space>  <SID>push_into_markring()
nnoremap <expr> <C-Space>  <SID>pop_from_markring()

function! s:push_into_markring() abort
  if !exists('b:markring_position')
    let b:markring_position = 0
  else
    let b:markring_position = (b:markring_position + 1)
    \                         % len(s:MARK_CHARACTERS)
  endif
  let mark = s:MARK_CHARACTERS[b:markring_position]
  return 'm' . mark
endfunction

function! s:pop_from_markring() abort
  let available_marks = join(map(getmarklist(bufnr('%')), 'v:val.mark[1:]'), '')
  let initial_position = get(b:, 'markring_position', 0)
  let position = initial_position
  while 1
    let mark = s:MARK_CHARACTERS[position]
    let position = (len(s:MARK_CHARACTERS) + position - 1)
    \              % len(s:MARK_CHARACTERS)
    if stridx(available_marks, mark) >= 0
      let b:markring_position = position
      return "'" . mark
    endif
    if position == initial_position
      return ''
    endif
  endwhile
endfunction

" Text objects  {{{2

" Synonym for <>.
onoremap aa  a>
vnoremap aa  a>
onoremap ia  i>
vnoremap ia  i>

" Synonym for [].
onoremap ar  a]
vnoremap ar  a]
onoremap ir  i]
vnoremap ir  i]

" Select the last changed text.
nnoremap gc  `[v`]h
onoremap gc  :<C-u>normal! gc<CR>
vnoremap gc  :<C-u>normal! gc<CR>

" Select the last selected text.
onoremap <silent> gv  :<C-u>normal! gv<CR>

" Operators  {{{2
" operator-eval  {{{3

call operator#user#define('eval',
\                         s:SID_PREFIX . 'operator_eval')

function! s:operator_eval(motion_wiseness) abort
  let reg_value = getreg('"')
  let reg_type = getregtype('0')
  try
    let visual_command =
    \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
    execute 'normal!' ('`[' . visual_command . '`]""y')
    let result = join(map(split(@", "\n"), "s:sandbox_eval(s:trim(v:val))"), "\n")
    call setreg('"', result, visual_command)
    execute 'normal!' ('`[' . visual_command . '`]""p')
  finally
    call setreg('"', reg_value, reg_type)
  endtry
endfunction

map g=  <Plug>(operator-eval)
nmap g==  <Plug>(operator-eval)<Plug>(operator-eval)

" operator-increment/decrement  {{{3

call operator#user#define('increment',
\                         s:SID_PREFIX . 'operator_increment')
call operator#user#define('decrement',
\                         s:SID_PREFIX . 'operator_decrement')

function! s:operator_increment(motion_wiseness) abort
  let visual_command =
  \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' ('`[' . visual_command . '`]g' . "\<C-a>")
endfunction

function! s:operator_decrement(motion_wiseness) abort
  let visual_command =
  \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' ('`[' . visual_command . '`]g' . "\<C-x>")
endfunction

map g<C-a>  <Plug>(operator-increment)
map g<C-x>  <Plug>(operator-decrement)

" operator-multiply  {{{3

call operator#user#define('multiply',
\                         s:SID_PREFIX . 'operator_multiply')

function! s:operator_multiply(motion_wiseness) abort
  let reg_value = getreg('"')
  let reg_type = getregtype('0')
  try
    let visual_command =
    \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
    execute 'normal!' ('`[' . visual_command . '`]""yP')
  finally
    call setreg('"', reg_value, reg_type)
  endtry
endfunction

map gm  <Plug>(operator-multiply)
nmap gmm  <Plug>(operator-multiply)<Plug>(operator-multiply)

" operator-search-forward/backward  {{{3

call operator#user#define('search-forward',
\                         s:SID_PREFIX . 'operator_search',
\                         'let v:searchforward = 1')
call operator#user#define('search-backward',
\                         s:SID_PREFIX . 'operator_search',
\                         'let v:searchforward = 0')

function! s:operator_search(motion_wiseness) abort
  let reg_value = getreg('"')
  let reg_type = getregtype('0')

  try
    let visual_command =
    \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
    execute 'normal!' ('`[' . visual_command . '`]""y')

    let pattern = '\V' . substitute(escape(@", '\'), '\n', '\\n', 'g')
    call setreg('/', pattern)
    call histadd('/', pattern)

    execute 'normal!' (v:searchforward ? 'n' : 'N')
  finally
    call setreg('"', reg_value, reg_type)
  endtry
endfunction

vmap *  <Plug>(operator-search-forward)
vmap #  <Plug>(operator-search-backward)

" operator-source  {{{3

call operator#user#define('source', s:SID_PREFIX . 'operator_source')

function! s:operator_source(motion_wiseness) abort
  let start = getpos("'[")
  let end = getpos("']")
  let range = start[1] . ',' . end[1]
  execute range 'Source'
endfunction

map g.  <Plug>(operator-source)

" operator-translate  {{{3

call operator#user#define('translate', s:SID_PREFIX . 'operator_translate')

function! s:operator_translate(motion_wiseness) abort
  let visual_command =
  \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
  silent execute 'normal!' ('`[' . visual_command . '`]""y')

  let text = @"
  let text = substitute(text, '\n\@<!\n\n\@!', ' ', 'g')
  let text = substitute(text, '\n\+', '\n', 'g')

  if text =~ '[\u3000-\u30ff\u3400-\u4dbf\u4e00-\u9fff\uf900-\ufaff]'
    let translation = google_translate#translate('ja', 'en', text)
  else
    let translation = google_translate#translate('en', 'ja', text)
  endif

  let @" = translation

  echon s:trim(translation) "\n"
endfunction

Arpeggio map ot  <Plug>(operator-translate)

" operator-yank-clipboard  {{{3

call operator#user#define('yank-clipboard',
\                         s:SID_PREFIX . 'operator_yank_clipboard')

function! s:operator_yank_clipboard(motion_wiseness) abort
  let visual_command =
  \   operator#user#visual_command_from_wise_name(a:motion_wiseness)
  execute 'normal!' ('`[' . visual_command . '`]"+y')
endfunction

Arpeggio noremap oy  <Plug>(operator-yank-clipboard)

" Misc.  {{{2

" Command-line helpers.
nnoremap <C-h>  :<C-u>Help<Space>
nnoremap <C-o>  :<C-u>edit<Space>
nnoremap <C-w>.  :<C-u>edit .<CR>

" x/X without register.
nnoremap X  "_X
vnoremap X  "_X
nnoremap x  "_x
vnoremap x  "_x

" Yank from the cursor to the end of the line.
nnoremap Y  y$

" Jump list navigations.
nnoremap <C-j>  <C-i>
nnoremap <C-k>  <C-o>

" Disable dangerous keys.
nnoremap ZZ  <Nop>
nnoremap ZQ  <Nop>

" The shortcut for :update.
nnoremap <silent> <Leader><Leader>  :<C-u>update<CR>

" Like o/O, but insert additional [count] blank lines.
nnoremap <expr> <Plug>(arpeggio-default:o)
\        <SID>start_insert_mode_with_blank_lines('o')
nnoremap <expr> O
\        <SID>start_insert_mode_with_blank_lines('O')

function! s:start_insert_mode_with_blank_lines(command) abort
  if v:count != v:count1
    return a:command
  endif

  if a:command ==# 'o'
    return "\<Esc>o" . repeat("\<CR>", v:count - 1)
  else
    return "\<Esc>O" . repeat("\<CR>\<Up>", v:count - 1) . "\<Esc>S"
  endif
endfunction

" Make search directions consistent.
nnoremap <expr> n  v:searchforward ? 'nzv' : 'Nzv'
nnoremap <expr> N  v:searchforward ? 'Nzv' : 'nzv'
vnoremap <expr> n  v:searchforward ? 'nzv' : 'Nzv'
vnoremap <expr> N  v:searchforward ? 'Nzv' : 'nzv'
onoremap <expr> n  v:searchforward ? 'n' : 'N'
onoremap <expr> N  v:searchforward ? 'N' : 'n'

" Jump to the definition of the keyword under the cursor.
nnoremap <expr> <CR>  &l:filetype ==# 'qf' ? "\<CR>" : "\<C-]>"
vnoremap <expr> <CR>  &l:filetype ==# 'qf' ? "\<CR>" : "\<C-]>"

" A fallback for repeat.
nnoremap <silent> \  .

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
    setlocal completefunc=git_complete#completefunc
  endif

  if &l:omnifunc == ''
    setlocal omnifunc=syntaxcomplete#Complete
  endif

  " Disable auto-wrapping.
  setlocal formatoptions-=t formatoptions-=c
endfunction

" Optimize the loading for large files.
autocmd MyAutoCmd BufReadPre *
\   if getfsize(expand('<afile>')) > 1024 * 1024 * 8
\ |   set eventignore+=FileType
\ |   setlocal noswapfile
\ | else
\ |   set eventignore-=FileType
\ | endif

" Unset 'paste' automatically.
autocmd MyAutoCmd InsertLeave *  set nopaste

" Highlight special spaces.
autocmd MyAutoCmd VimEnter,WinEnter *
\ match Underlined /[\u1680\u2000-\u200a\u202f\u205f\u3000]/

" Jump to the last known cursor position.
autocmd MyAutoCmd BufReadPost *
\   if line("'\"") >= 1 && line("'\"") <= line('$')
\ |   execute "normal! g`\""
\ |   silent! foldopen!
\ | endif

" Avoid to set `filetype` twice.
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

" The indent for a continuation line
let g:vim_indent_cont = 0

" Disable Lua embedding, since this is buggy.
let g:vimsyn_embed = ''

" Plugins  {{{1
" accelerate  {{{2

call accelerate#map('nv', 'e', 'j', 'v:count == 0 ? "gj" : "j"')
call accelerate#map('nv', 'e', 'k', 'v:count == 0 ? "gk" : "k"')
call accelerate#map('nv', '', 'h', 'h')
call accelerate#map(
\   'nv',
\   'e',
\   'l',
\   'v:count == 0 && foldclosed(line(".")) >= 0 ? "zo" : "l"'
\ )

" exjumplist  {{{2

nmap <M-C-j>  <Plug>(exjumplist-next-buffer)
nmap <M-C-k>  <Plug>(exjumplist-previous-buffer)

" grex  {{{2

nmap gy  <Plug>(operator-grex-yank)<Plug>(textobj-entire-a)
vmap gy  <Plug>(operator-grex-yank)
nmap gd  <Plug>(operator-grex-delete)<Plug>(textobj-entire-a)
vmap gd  <Plug>(operator-grex-delete)

" luis  {{{2
" Mappings  {{{3

nnoremap [Space]k  <Nop>
nnoremap <silent> [Space]k/  :<C-u>call <SID>luis_start(luis#source#history#new('search'))<CR>
nnoremap <silent> [Space]k:  :<C-u>call <SID>luis_start(luis#source#history#new('cmd'))<CR>
nnoremap <silent> [Space]kM  :<C-u>call <SID>luis_start(luis#source#mark#new())<CR>
nnoremap <silent> [Space]kT  :<C-u>call <SID>luis_start(luis#source#tagstack#new(win_getid()))<CR>
nnoremap <silent> [Space]k\  :<C-u>call <SID>luis_start(luis#source#history#new('expr'))<CR>
nnoremap <silent> [Space]ka  :<C-u>call <SID>luis_start(luis#source#arglist#new())<CR>
nnoremap <silent> [Space]kb  :<C-u>call <SID>luis_start(luis#source#buffer#new())<CR>
nnoremap <silent> [Space]kc  :<C-u>call <SID>luis_start_with_path(luis#source#file#new(), expand('%:h'))<CR>
nnoremap <silent> [Space]kd  :<C-u>call <SID>luis_start(luis#source#lsp_document_symbol#new(bufnr('%')))<CR>
nnoremap <silent> [Space]kf  :<C-u>call <SID>luis_start(luis#source#file#new())<CR>
nnoremap <silent> [Space]kg  :<C-u>call <SID>luis_start(luis#source#metarw#new('git'))<CR>
nnoremap <silent> [Space]kh  :<C-u>call <SID>luis_start(luis#source#help#import())<CR>
nnoremap <silent> [Space]ki  :<C-u>call <SID>luis_start(luis#source#history#new('input'))<CR>
nnoremap <silent> [Space]kj  :<C-u>call <SID>luis_start(luis#source#jumplist#new(win_getid()))<CR>
nnoremap <silent> [Space]kk  :<C-u>call <SID>luis_resume()<CR>
nnoremap <silent> [Space]kl  :<C-u>call <SID>luis_start_async_files()<CR>
nnoremap <silent> [Space]km  :<C-u>call <SID>luis_start(luis#source#local_mark#new(bufnr('%')))<CR>
nnoremap <silent> [Space]kn  :<C-u>call <SID>luis_start_file_in_path(expand("~/Sync/Documents/Notes"))<CR>
nnoremap <silent> [Space]ko  :<C-u>call <SID>luis_start(luis#source#oldfiles#new())<CR>
nnoremap <silent> [Space]kq  :<C-u>call <SID>luis_start(luis#source#quickfix#new())<CR>
nnoremap <silent> [Space]kr  :<C-u>call <SID>luis_start(luis#source#register#new())<CR>
nnoremap <silent> [Space]ks  :<C-u>call <SID>luis_start(luis#source#spell#import())<CR>
nnoremap <silent> [Space]kt  :<C-u>call <SID>luis_start_async_tags()<CR>
nnoremap <silent> [Space]kw  :<C-u>call <SID>luis_start_project(expand('~/Sync/works'))<CR>
nnoremap <silent> [Space]kz  :<C-u>call <SID>luis_start(luis#source#fold#new(win_getid()))<CR>

let s:luis_last_session = 0

function! s:luis_project_callback(path)  " {{{3
  tcd `=a:path`
  call s:luis_start_async_files()
endfunction

function! s:luis_resume() abort  " {{{3
  if s:luis_last_session isnot 0
    call luis#start(s:luis_last_session)
  endif
endfunction

function! s:luis_start(source, ...) abort  " {{{3
  let options = get(a:000, 0, {})
  let session = luis#new_session(a:source, options)
  if luis#start(session)
    let s:luis_last_session = session
  endif
endfunction

function! s:luis_start_async_files() abort  " {{{3
  if isdirectory('.git')
    let command = [
    \   'fzf-filter', '-f', '1', '-p', '2', '-d', ' ', '-l', '1000',
    \   '--',
    \   'git', 'ls-files', '--exclude-standard', '-v', '-c', '-o',
    \ ]
    let options = {
    \   'to_candidate': function('s:to_git_file_candidate'),
    \ }
    let source = luis#source#async#new(
    \   'git_files',
    \   luis#kind#file#import(),
    \   command,
    \   options
    \ )
  else
    let command = [
    \   'fzf-filter', '-l', '1000',
    \   '--',
    \   'fd', '--type', 'f', '--strip-cwd-prefix', '-c', 'never', '--max-results', '10000',
    \ ]
    let options = {
    \   'to_candidate': { line -> {
    \     'word': line,
    \     'user_data': { 'preview_path': line },
    \   }},
    \ }
    let source = luis#source#async#new(
    \   'fd',
    \   luis#kind#file#import(),
    \   command,
    \   options
    \ )
  endif

  call s:luis_start(source)
endfunction

function! s:luis_start_async_tags() abort  " {{{3
  let name = 'tags'
  let kind = luis#kind#tag#import()
  let command = [
  \   'fzf-filter', '-l', '1000', '-f', '0',
  \   '--',
  \   'sh', '-c', 'echo "$@" | xargs -n 1 -P 4 cat', '',
  \ ]
  call extend(command, tagfiles())
  let options = {
  \   'to_candidate': function('s:make_tag_candidate'),
  \ }
  let source = luis#source#async#new(name, kind, command, options)
  return s:luis_start(source)
endfunction

function! s:luis_start_file_in_path(path) abort  " {{{3
  let source = luis#source#file#new()
  let hook = { 'path': a:path }

  function! hook.on_source_enter(context) abort dict
    lcd `=self.path`
  endfunction

  function! hook.on_source_leave(context) abort dict
    lcd -
  endfunction

  call s:luis_start(source, { 'hook': hook })
endfunction

function! s:luis_start_project(path) abort  " {{{3
  let Callback = function('s:luis_project_callback')
  let source = luis#source#project#new(a:path, Callback)
  call s:luis_start(source)
endfunction

function! s:luis_start_with_path(source, path) abort  " {{{3
  let initial_pattern = a:path != '' && a:path != '.'
  \                   ? a:path . '/'
  \                   : ''
  call s:luis_start(a:source, {
  \   'initial_pattern': initial_pattern,
  \ })
endfunction

function! s:to_git_file_candidate(line) abort  " {{{3
  let components = split(a:line, '^\S\zs\s')
  let type = get(components, 0, '?')
  let path = get(components, 1, '')
  return {
  \   'word': path,
  \   'kind': type,
  \   'user_data': {
  \     'preview_path': path,
  \   },
  \ }
endfunction

function! s:make_tag_candidate(line) abort  " {{{3
  let components = split(a:line, '\t')
  return {
  \   'word': get(components, 0, a:line),
  \   'menu': get(components, 1, ''),
  \   'dup': 1,
  \ }
endfunction

" operator-camelize  {{{2

map <Leader>c  <Plug>(operator-camelize)
map <Leader>C  <Plug>(operator-decamelize)

" operator-comment  {{{2

Arpeggio map oc  <Plug>(operator-comment)
Arpeggio map od  <Plug>(operator-uncomment)

" operator-replece  {{{2

Arpeggio map or  <Plug>(operator-replace)

" operator-sort  {{{2

nmap <Leader>s  <Plug>(operator-sort-numeric)
vmap <Leader>s  <Plug>(operator-sort-numeric)
nmap <Leader>S  <Plug>(operator-sort-numeric)$
vmap <Leader>S  <Plug>(operator-sort-numeric)$

" quickrun  {{{2

command! -complete=command -nargs=+ Capture  QuickRun vim/source -src <q-args>

let g:quickrun_config = {
\   '_': {
\     'outputter/buffer/opener': 'Split',
\   },
\   'c': {
\     'type': 'c/clang',
\   },
\   'objc': {
\     'type': 'objc/clang',
\   },
\   'objc/clang': {
\     'cmdopt': '-framework Foundation -framework AppKit',
\     'command': 'clang',
\     'exec': ['%c %o -o %s:p:r %s', '%s:p:r %a'],
\     'hook/sweep/files': '%S:p:r',
\     'tempfile': '%{fnamemodify(tempname(), ":r")}.m',
\   },
\   'cpp': {
\     'type': 'cpp/g++',
\     'cmdopt': '-std=c++2b',
\   },
\   'dot': {
\     'exec': ['%c -Tsvg -o %s:p:r.svg %s'],
\   },
\   'haskell': {
\     'type': 'haskell/runghc',
\   },
\   'haskell/runghc': {
\     'cmdopt': '--ghc-arg=-Wall -Wno-unused-top-binds',
\     'command': 'runghc',
\     'hook/eval/template': 'main = print \$ %s',
\     'tempfile': '%{tempname()}.hs',
\   },
\   'javascript': {
\     'type': 'javascript/nodejs',
\   },
\   'javascript/nodejs': {
\     'cmdopt': '--experimental-default-type=module',
\     'command': 'node',
\     'hook/eval/template': 'console.log(%s)',
\     'tempfile': '%{tempname()}.mjs',
\   },
\   'javascript/browserjs': {
\     'command': 'browserjs',
\     'hook/eval/template': 'console.log(%s)',
\   },
\   'json': {
\     'cmdopt': '.',
\     'command': 'jq',
\   },
\   'lua': {
\     'type': executable('nvim') ? 'lua/neovim' : 'lua/vim',
\   },
\   'lua/neovim': {
\     'cmdopt': '-u NONE -i NONE -N -n -E',
\     'command': 'nvim',
\     'exec': ['%c %o -l %s'],
\     'hook/eval/template': 'print(vim.inspect(%s))',
\     'tempfile': '%{tempname()}.lua',
\   },
\   'rust': {
\     'command': 'rustc',
\     'cmdopt': '-A dead_code --edition 2024',
\     'exec': ['%c %o %s -o %s:p:r', 'RUST_BACKTRACE=1 %s:p:r %a'],
\     'tempfile': '%{fnamemodify(tempname(), ":r")}.rs',
\     'hook/shebang/enable': 0,
\     'hook/sweep/files': '%S:p:r',
\   },
\   'sql': {
\     'type': 'sql/mysql',
\   },
\   'sql/mysql': {
\     'command': 'mysql',
\     'exec': ['%c --host 127.0.0.1 --user root %a < %s'],
\   },
\   'typescript': {
\     'type': executable('tsx') ? 'typescript/tsx' : 'typescript/ts-node',
\   },
\   'typescript/ts-node': {
\     'command': 'tsm',
\     'cmdopt': '--esm',
\     'exec': ['TS_NODE_TRANSPILE_ONLY=true %c %o %s %a'],
\   },
\   'typescript/tsx': {
\     'command': 'tsx',
\     'exec': ['%c %o %s %a'],
\   },
\   'vim': {
\     'type': 'vim/sandbox',
\   },
\   'vim/sandbox': {
\     'cmdopt': '-u NONE -i NONE -N -n -E -s',
\     'command': v:progpath,
\     'exec': ['%c %o --cmd "verbose source %S" --cmd "qall!"'],
\     'hook/eval/template': 'echo %s',
\     'tempfile': '%{tempname()}.vim',
\   },
\   'vim/source': {
\     'command': ':source',
\     'exec': '%C %S',
\     'hook/eval/template': 'echo %s',
\     'runner': 'vimscript',
\   },
\   'xdefaults': {
\     'command': 'xrdb',
\     'exec': ['%c -remove', '%c -merge %s', '%c -query'],
\   },
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

let g:ref_no_default_key_mappings = 1
let g:ref_open = 'Split'
let g:ref_perldoc_complete_head = 1
let g:ref_phpmanual_path = '/usr/share/php-docs/en/php-chunked-xhtml'

" scratch  {{{2

nmap [Space]s  <Plug>(scratch-open)

autocmd MyAutoCmd User PluginScratchInitializeAfter
\ call s:on_User_plugin_scratch_initialize_after()

function! s:on_User_plugin_scratch_initialize_after() abort
  setlocal nobuflisted
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

" smartinput  {{{2
" Initialize  {{{3

let g:smartinput_no_default_key_mappings = 1

if exists('g:loaded_smartinput')
  call smartinput#clear_rules()
  call smartinput#define_default_rules()
endif

" Common {{{3

call smartinput#define_rule({
\   'at': '/\*\*\%#',
\   'char': '<CR>',
\   'input': '<CR><CR>/<Up><Space>',
\ })

" for Vim  {{{3

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
\ })

" for PHP  {{{3

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
\ })

" for Rust  {{{3

call smartinput#define_rule({
\   'at': '\%#',
\   'char': "'",
\   'input': "'",
\   'filetype': ['rust'],
\ })

" Fin.  {{{3

call smartinput#map_trigger_keys()

" smartword  {{{2

map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)

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

call submode#enter_with(
\   'winsize', 'n', '', '<C-w><Space>',
\   ':<C-u>call ' . s:SID_PREFIX . 'submode_winsize()<CR>'
\ )
call submode#enter_with(
\   'winsize', 'n', '', '<C-w><C-@>',
\   ':<C-u>call ' . s:SID_PREFIX . 'submode_winsize()<CR>'
\ )

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

" surround-obj  {{{2

map s  <Plug>(surround-obj-add)
map S  <Plug>(surround-obj-add)$

nmap cs  <Plug>(surround-obj-change)
nmap ds  <Plug>(surround-obj-delete)

" By default, a text object for a quote string contains trailing white spaces.
" These mappings aren't included it.
omap a'  <Plug>(surround-obj-a:')
vmap a'  <Plug>(surround-obj-a:')
omap a"  <Plug>(surround-obj-a:")
vmap a"  <Plug>(surround-obj-a:")
omap a`  <Plug>(surround-obj-a:`)
vmap a`  <Plug>(surround-obj-a:`)
omap i'  <Plug>(surround-obj-i:')
vmap i'  <Plug>(surround-obj-i:')
omap i"  <Plug>(surround-obj-i:")
vmap i"  <Plug>(surround-obj-i:")
omap i`  <Plug>(surround-obj-i:`)
vmap i`  <Plug>(surround-obj-i:`)

let g:surround_obj_config = {
\   'a': { 'type': 'block', 'delimiter': ['<', '>'] },
\   'e': { 'type': 'inline', 'delimiter': '_' },
\   'r': { 'type': 'block', 'delimiter': ['[', ']'] },
\   's': { 'type': 'inline', 'delimiter': '**' },
\   'jA': {'type': 'block', 'delimiter': ['≪', '≫']},
\   'ja': {'type': 'block', 'delimiter': ['＜', '＞']},
\   'jb': {'type': 'block', 'delimiter': ['（', '）']},
\   'jB': {'type': 'block', 'delimiter': ['｛', '｝']},
\   'jk': {'type': 'block', 'delimiter': ['「', '」']},
\   'jK': {'type': 'block', 'delimiter': ['『', '』']},
\   'jr': {'type': 'block', 'delimiter': ['［', '］']},
\   'js': {'type': 'block', 'delimiter': ['【', '】']},
\   'jt': {'type': 'block', 'delimiter': ['〔', '〕']},
\   'jy': {'type': 'block', 'delimiter': ['〈', '〉']},
\   'jY': {'type': 'block', 'delimiter': ['《', '》']},
\ }

" table-mode  {{{2

nnoremap <silent> [Space]ot  :<C-u>TableModeToggle<CR>

" Fin.  {{{1

set secure

" __END__  {{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
