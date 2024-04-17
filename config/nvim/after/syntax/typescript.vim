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

function! s:include_svg_syntax()
  let original_current_syntax = b:current_syntax

  unlet b:current_syntax

  syntax include @typescriptSVGSyntax syntax/svg.vim

  let b:current_syntax = original_current_syntax
endfunction

call s:include_html_syntax()
call s:include_svg_syntax()

syntax region typescriptHTMLTemplate
\ matchgroup=String
\ start='\%(\K\k*\.\|\<\)html\s*`'hs=e
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

syntax region typescriptSVGTemplate
\ matchgroup=String
\ start='\%(\K\k*\.\|\<\)svg\s*`'hs=e
\ skip='\\`'
\ end='`'
\ contains=@typescriptSVGSyntax,typescriptSVGTemplateHole

syntax region typescriptSVGTemplateAttribute
\ keepend
\ start='=\s*\${'
\ end='}'
\ contains=typescriptSVGTemplateHole
\ contained
\ containedin=xmlTag

syntax region typescriptSVGTemplateHole
\ matchgroup=Special
\ start='\${'
\ end='}'
\ contains=@typescriptValue
\ contained
\ containedin=xmlComment,xmlString,xmlTag

syntax cluster typescriptTopExpression
\ add=typescriptHTMLTemplate,typescriptSVGTemplate

" Add matchgroup to typescriptClassTypeParameter
syntax clear typescriptClassTypeParameter
syntax region typescriptClassTypeParameter
\ matchgroup=typescriptTypeBrackets
\ start='<'
\ end='>'
\ contains=@typescriptTypeParameterCluster
\ nextgroup=typescriptClassBlock,typescriptClassExtends
\ contained
\ skipwhite
\ skipnl

" Fix index closing tag highlight
syntax clear typescriptIndexExpr
syntax region typescriptIndexExpr
\ matchgroup=typescriptProperty
\ start=/\[/rs=s+1
\ end=/]/re=e-1
\ contains=@typescriptValue
\ nextgroup=@typescriptSymbols,typescriptDotNotation,typescriptFuncCallArg
\ contained
\ skipwhite
\ skipempty

highlight link typescriptBinaryOp Operator
highlight link typescriptBraces Noise
highlight link typescriptHTMLTemplateQuotedString htmlString
highlight link typescriptParens Noise
highlight link typescriptProperty Noise
highlight link typescriptTypeAnnotation Noise
highlight link typescriptTypeBracket Noise
highlight link typescriptTypeBrackets Noise
