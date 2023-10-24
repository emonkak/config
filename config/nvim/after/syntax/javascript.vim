if exists('g:main_syntax')
  finish
endif

function! s:include_html_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax
  let g:main_syntax = 'java'
  let g:java_css = 1

  syntax include @jsHtmlSyntax syntax/html.vim

  unlet g:java_css
  unlet g:main_syntax
  let b:current_syntax = original_current_syntax
endfunction

call s:include_html_syntax()

syntax region jsLitHtmlTemplate
\ keepend
\ extend
\ start='\<html`'
\ skip='\\`'
\ end='`'
\ contains=@jsHtmlSyntax,jsTemplateExpressionHtmlText,jsTemplateExpressionHtmlAttribute
\ containedin=jsTaggedTemplate

syntax region jsTemplateExpressionHtmlText
\ matchgroup=Special
\ keepend
\ start='${'
\ end='}'
\ contains=jsBlock,jsTemplateExpression
\ contained
\ containedin=htmlString,htmlComment

syntax region jsTemplateExpressionHtmlAttribute
\ matchgroup=Special
\ start='=\s*["'']\?${'hs=s+1
\ end='}'
\ contains=jsBlock,jsTemplateExpression
\ contained
\ containedin=htmlTag

syntax cluster jsExpression  add=jsLitHtmlTemplate
