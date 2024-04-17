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

function! s:include_svg_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax

  syntax include @jsSVGSyntax syntax/svg.vim

  let b:current_syntax = original_current_syntax
endfunction

" Allow spaces before tag name.
syntax clear jsTaggedTemplate
syntax match jsTaggedTemplate '\<\K\k*\s*\ze`' nextgroup=jsTemplateString

call s:include_html_syntax()
call s:include_svg_syntax()

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

syntax region jsSVGTemplate
\ matchgroup=String
\ start='\%(\K\k*\.\|\<\)svg\s*`'hs=e
\ skip='\\`'
\ end='`'
\ contains=@jsSVGSyntax,jsSVGTemplateHole

syntax region jsSVGTemplateAttribute
\ keepend
\ start='=\s*\${'
\ end='}'
\ contains=jsSVGTemplateHole
\ contained
\ containedin=xmlTag

syntax region jsSVGTemplateHole
\ matchgroup=Special
\ start='\${'
\ end='}'
\ contains=@jsExpression
\ contained
\ containedin=xmlComment,xmlString,xmlTag

syntax cluster jsExpression add=jsHTMLTemplate,jsSVGTemplate
