" More strict Ecmascript import/export syntax
syntax clear typescriptImport
syntax clear typescriptImportType
syntax clear typescriptExport
syntax clear typescriptExportType

syntax keyword typescriptImport import
\ nextgroup=typescriptImportType,@typescriptImportClause,typescriptString
\ skipwhite
\ skipempty
syntax keyword typescriptImportType type
\ contained
\ nextgroup=@typescriptImportClause
\ skipwhite
\ skipempty
syntax match typescriptImportDefaultBinding /\K\k*\%(\s*,\)\?/
\ contains=@typescriptBinding
\ contained
\ nextgroup=typescriptImportNamespace,typescriptImportNamedImports,typescriptFromClause
\ skipwhite
\ skipempty
syntax match typescriptImportNamespace /\*\s\+as\s\+\K\k*/he=s+1
\ contains=typescriptBindingAlias
\ contained
\ nextgroup=typescriptFromClause
\ skipwhite
\ skipempty
syntax region typescriptImportNamedImports
\ matchgroup=typescriptBraces
\ start=/{/
\ end=/}/
\ contains=typescriptComment,typescriptString,typescriptBindingAlias,@typescriptBinding
\ contained
\ nextgroup=typescriptFromClause
\ skipwhite
\ skipempty
syntax cluster typescriptImportClause
\ contains=typescriptImportDefaultBinding,typescriptImportNamespace,typescriptImportNamedImports

syntax keyword typescriptExport export
\ nextgroup=typescriptExportType,typescriptExportNamespace,typescriptExportNamedExports
\ skipwhite
\ skipempty
syntax keyword typescriptExportType type
\ contained
\ nextgroup=typescriptExportNamespace,typescriptExportNamedExports
\ skipwhite
\ skipempty
syntax match typescriptExportNamespace /\*\%(\s\+as\s\+\K\k*\)\?/he=s+1
\ contains=typescriptBindingAlias
\ contained
\ nextgroup=typescriptFromClause
\ skipwhite
\ skipempty
syntax region typescriptExportNamedExports
\ matchgroup=typescriptBraces
\ start=/{/
\ end=/}/
\ contains=typescriptComment,typescriptString,typescriptBindingAlias,@typescriptBinding
\ contained
\ nextgroup=typescriptFromClause
\ skipwhite
\ skipempty

syntax keyword typescriptFromClause from
\ contained
\ nextgroup=typescriptString
\ skipwhite
\ skipempty

syntax cluster typescriptBinding
\ contains=typescriptBindingValue,typescriptBindingType
syntax match typescriptBindingValue /\K\k*/
\ contained
syntax keyword typescriptBindingType type
\ contained
\ nextgroup=typescriptBindingValue
\ skipwhite
\ skipempty
syntax keyword typescriptBindingAlias as
\ contained
\ nextgroup=typescriptBindingValue
\ skipwhite
\ skipempty

highlight link typescriptBindingAlias Special
highlight link typescriptBindingType Special
highlight link typescriptFromClause Special
highlight link typescriptImportNamespace Keyword
highlight link typescriptExportNamespace Keyword

" Add const type
syntax keyword typescriptConstType const
\ contained
syntax cluster typescriptType
\ add=typescriptConstType
highlight link typescriptConstType Keyword

" Allow TOP highlight groups inside blocks
syntax clear typescriptBlock
syntax region typescriptBlock
\ matchgroup=typescriptBraces
\ start=/{/
\ end=/}/
\ contains=TOP
\ fold

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

" Fix highlighting for void type
syntax clear typescriptOperator
syntax keyword typescriptOperator delete new typeof void

" Better highlighting
highlight link typescriptBinaryOp Operator
highlight link typescriptBraces Noise
highlight link typescriptHTMLTemplateQuotedString htmlString
highlight link typescriptParens Noise
highlight link typescriptProperty Noise
highlight link typescriptTypeAnnotation Noise
highlight link typescriptTypeBracket Noise
highlight link typescriptTypeBrackets Noise
highlight link typescriptVariable Keyword
