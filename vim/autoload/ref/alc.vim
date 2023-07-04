" ============================================================================
" File:         autoload/ref/alc.vim
" Author:       mojako <moja.ojj@gmail.com>
" URL:          https://github.com/mojako/ref-sources.vim
" Last Change:  2011-09-16
" ============================================================================

scriptencoding utf-8

" s:cpo_save {{{1
let s:cpo_save = &cpo
set cpo&vim
"}}}

" options {{{1
if !exists('g:ref_alc_intelligent_search')
    let g:ref_alc_intelligent_search = 1
endif

if !exists('g:ref_alc_remove_kana')
    let g:ref_alc_remove_kana = 1
endif

if !exists('g:ref_alc_use_cache')
    let g:ref_alc_use_cache = exists('g:ref_use_cache') ? g:ref_use_cache : 0
endif
"}}}

let s:source = {'name': 'alc'}

" s:source.available() {{{1
" ====================
function! s:source.available() abort
    return executable('curl')
endfunction

" s:source.complete( <query> ) {{{1
" ============================
function! s:source.complete(query) abort
    let result = webapi#http#get('https://eow.alc.co.jp/eow/sg/', {
      \ 'q': iconv(a:query, &enc, 'utf-8'),
      \ })

    return map(split(result.content, '<word>')[1:],
      \ 'iconv(webapi#http#decodeURI(matchstr(v:val, ''.\{-}\ze</word>'')), ''utf-8'', &enc)')
endfunction

" s:source.get_body( <query> ) {{{1
" ============================
function! s:source.get_body(query) abort
    let query = iconv(a:query, &enc, 'utf-8')

    " キャッシュが無効な場合、<query>を検索して返す {{{2
    if !g:ref_alc_use_cache
        return iconv(s:get_body(query), 'utf-8', &enc)
    endif

    " <query>のキャッシュが存在する場合、それを返す {{{2
    let cache_name = tolower(query)
    let cache = ref#cache(s:source.name, cache_name)
    if type(cache) == type([])
        return iconv(cache, 'utf-8', &enc)
    endif

    " 存在しない場合、<query>を検索してキャッシュする {{{2
    let body = s:get_body(query)
    if body != ''
        call ref#cache(s:source.name, cache_name, [body], 1)
    endif
    "}}}

    return iconv(body, 'utf-8', &enc)
endfunction

" s:source.get_keyword() {{{1
" ======================
function! s:source.get_keyword() abort
    let url = ref#get_text_on_cursor('https\?://\S\+')
    if url != ''
        if globpath(&rtp, 'autoload/openbrowser.vim') != ''
            call openbrowser#open(url)
        endif
        return ''
    endif

    let isk_save = &l:isk
    setlocal isk=45,48-57,65-90,97-122  " -0-9A-Za-z
    let kwd = expand('<cword>')
    let &l:isk = isk_save
    return kwd
endfunction

" s:source.opened( <query> ) {{{1
" ==========================
function! s:source.opened(query) abort
    setlocal conceallevel=2 syntax=ref-alc
endfunction
"}}}

function! ref#alc#define() abort
    return copy(s:source)
endfunction

" s:get_body( <query> ) {{{1
" =====================
function! s:get_body(query) abort
    " <query>の検索結果を取得 {{{2
    let url = 'https://eow.alc.co.jp/search'
    let result = webapi#http#get(url, { 'q': a:query })
    if result.status != '200'
        return ''
    endif

    let body = result.content

    " 一致する結果がないとき、空の文字列を返す {{{2
    if body =~# '<div id="unmatchMsg"'
        return ''
    endif

    " 「全文へのリンク」の為の下準備 {{{2
    let dk = matchstr(body, 'name="dk" type="hidden" value="\zs\w*\ze"')

    " 改行とタブを削除 {{{2
    let body = substitute(body, '[\n\r\t]', '', 'g')

    " 検索結果部分を抽出 {{{2
    let body = matchstr(body,
      \ '<!-- ▼ 検索結果本体 ▼ -->\zs.*\ze<!-- ▲ 検索結果本体 ▲ -->')

    " <br>, <li>タグを改行に変換 {{{2
    let body = substitute(body, '<\%(br\|li\)\%(\s[^>]*\)\?>', '\n', 'g')

    " ふりがなを削除 {{{2
    if g:ref_alc_remove_kana
        let body = substitute(body, '<span class="kana">.\{-}</span>', '', 'g')
    endif

    " 見出し {{{2
    let body = substitute(body, '<span class="redtext">\(.\{-}\)</span>',
      \ '\1', 'g')
    let body = substitute(body, '<span class="midashi">\(.\{-}\)</span>',
      \ '\n*\1* ', 'g')
    let body = substitute(body, '[\.,"?!]\*\* \zs<div>\ze\%(<\)\@!',
      \ '\n-- ', 'g')

    " wordclassの前に改行を挿入 {{{2
    let body = substitute(body, '<span class="wordclass">', '\n', 'g')
    
    " 検索結果の前に改行を挿入 {{{2
    let body = substitute(body, '<div class="search-sentence-ttl"', '\n\n&', 'g')

    " 全文へのリンク {{{2
    let body = substitute(body,
      \ "<span class='exp'>" . '"\([^"]*\)", "\(\d*\)"</span>',
      \ '\n[全文] ' . url . '\?ref=ex\&amp;exp=\1\&amp;dn=\2\&amp;dk='
      \ . dk . '\n', 'g')

    " すべてのタグを削除 {{{2
    let body = substitute(body, '<script[^>]*>.\{-}</script>', '', 'g')
    let body = substitute(body, '<style[^>]*>.\{-}</style>', '', 'g')
    let body = substitute(body, '<div class="aboutword">.\{-}</div>', '', 'g')
    let body = substitute(body, '<[^>]*>', '', 'g')

    " 文字参照を置換 {{{2
    let body = webapi#html#decodeEntityReference(body)

    " 空行を詰める {{{2
    let body = substitute(body, '\s\+\n', '\n', 'g')
    let body = substitute(body, '^\n\+', '', 'g')
    let body = substitute(body, '\n\+\s*$', '', 'g')
    let body = substitute(body, '\n\{3,}', '\n\n', 'g')

    " 変換されたデータを返す {{{2
    return body
    "}}}
endfunction
"}}}

" s:cpo_save {{{1
let &cpo = s:cpo_save
unlet s:cpo_save
"}}}

" vim: set et sts=4 sw=4 wrap:
