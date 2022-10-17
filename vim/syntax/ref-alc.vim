" ============================================================================
" Language:     ref-alc
" File:         syntax/ref-alc.vim
" Author:       mojako <moja.ojj@gmail.com>
" URL:          https://github.com/mojako/ref-sources.vim
" Last Change:  2011-09-16
" ============================================================================

" s:cpo_save {{{1
let s:cpo_save = &cpo
set cpo&vim
"}}}

if exists('b:current_syntax')
    finish
endif

syntax region refAlcMidashi oneline concealends
\ matchgroup=refAlcConceal start='^\*' end='\*'

syntax match refAlcKeyword '◆'

syntax match refAlcLabel       '【.\{-}】'
syntax match refAlcAnnotation  '〔.\{-}〕'
syntax match refAlcAnnotation  '（.\{-}）'
syntax match refAlcAlt         '［.\{-}］'
syntax match refAlcAlt         '\[.\{-}\]'
syntax match refAlcKana        '｛.\{-}｝'
syntax match refAlcType        '《.\{-}》'
syntax match refAlcType        '〈.\{-}〉'
syntax match refAlcURL         'https\?://\S\+'

" Highlight Group Link
highlight def link refAlcKeyword     Statement
highlight def link refAlcMidashi     Title
highlight def link refAlcLabel       Constant
highlight def link refAlcAnnotation  Comment
highlight def link refAlcAlt         Comment
highlight def link refAlcType        Type
highlight def link refAlcKana        Comment
highlight def link refAlcURL         Underlined

let b:current_syntax = 'ref-alc'

" s:cpo_save {{{1
let &cpo = s:cpo_save
unlet s:cpo_save
"}}}

" vim: set et sts=4 sw=4 wrap:
