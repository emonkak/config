if exists('g:main_syntax')
  finish
endif

function! s:include_html_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax
  let g:main_syntax = 'java'
  let g:java_css = 1

  syntax include @jsHTMLSyntax syntax/html.vim

  unlet g:java_css
  unlet g:main_syntax
  let b:current_syntax = original_current_syntax
endfunction

call s:include_html_syntax()

syntax region jsHTMLTemplate
\ matchgroup=String
\ start='\<html\s*`'hs=e
\ skip='\\`'
\ end='`'
\ contains=@jsHTMLSyntax,jsHTMLTemplateHole
\ containedin=jsTaggedTemplate

syntax region jsHTMLTemplateAttribute
\ keepend
\ start='=\s*\${'
\ end='}'
\ contains=jsHTMLTemplateHole
\ contained
\ containedin=htmlTag

syntax region jsHTMLTemplateHole
\ matchgroup=Constant
\ start='\${'
\ end='}'
\ contains=@jsExpression
\ contained
\ containedin=@jsHTMLDecorations,htmlComment,htmlString,htmlTag

syntax cluster jsHTMLDecorations
\ contains=htmlBold,htmlBoldItalic,htmlBoldItalicUnderline,htmlBoldUnderline,htmlBoldUnderlineItalic,htmlH1,htmlH2,htmlH3,htmlH4,htmlH5,htmlH6,htmlHead,htmlItalic,htmlItalicBold,htmlItalicBoldUnderline,htmlItalicUnderline,htmlItalicUnderlineBold,htmlLink,htmlStrike,htmlTitle,htmlUnderline,htmlUnderlineBold,htmlUnderlineBoldItalic,htmlUnderlineItalic,htmlUnderlineItalicBold,htmlStrike

syntax cluster jsExpression add=jsHTMLTemplate
