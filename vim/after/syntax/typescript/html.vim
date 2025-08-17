if exists('g:main_syntax')
  finish
endif

function! s:include_html_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax
  let g:main_syntax = 'java'
  let g:java_css = 1

  syntax include @typescriptHTMLSyntax syntax/html.vim

  unlet g:java_css
  unlet g:main_syntax
  let b:current_syntax = original_current_syntax
endfunction

call s:include_html_syntax()

syntax region typescriptHTMLTemplate
\ matchgroup=String
\ start='\%([\K$]\k*\.\)\?html\s*`'hs=e
\ skip='\\`'
\ end='`'
\ contains=@typescriptHTMLSyntax,typescriptHTMLTemplateHole

syntax region typescriptHTMLTemplateAttribute
\ keepend
\ start='=\s*\${'
\ end='}'
\ contains=typescriptHTMLTemplateHole
\ contained
\ containedin=@typescriptHTMLDecorations,htmlTag

syntax region typescriptHTMLTemplateHole
\ matchgroup=Special
\ start='\${'
\ end='}'
\ contains=@typescriptValue
\ contained
\ containedin=@typescriptHTMLDecorations,htmlString,htmlComment,htmlTag

syntax cluster typescriptHTMLDecorations
\ contains=htmlBold,htmlBoldItalic,htmlBoldItalicUnderline,htmlBoldUnderline,htmlBoldUnderlineItalic,htmlH1,htmlH2,htmlH3,htmlH4,htmlH5,htmlH6,htmlHead,htmlItalic,htmlItalicBold,htmlItalicBoldUnderline,htmlItalicUnderline,htmlItalicUnderlineBold,htmlLink,htmlStrike,htmlTitle,htmlUnderline,htmlUnderlineBold,htmlUnderlineBoldItalic,htmlUnderlineItalic,htmlUnderlineItalicBold,htmlStrike

syntax cluster typescriptTopExpression add=typescriptHTMLTemplate
