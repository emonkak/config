syntax clear markdownCodeBlock
syntax clear markdownCode

syntax region markdownCode matchgroup=markdownCodeDelimiter start="``" end="`" keepend contains=markdownLineStart
syntax region markdownCodeBlock matchgroup=markdownCodeDelimiter start="^\z(>\?\s*\)\zs\z(`\{3,\}\|\~\{3,\}\).*$" end="^\z1\zs\z2\ze\s*$" keepend

syntax region markdownMath matchgroup=markdownMathDelimiter start="\$" end="\$" keepend contains=markdownLineStart
syntax region markdownMathBlock matchgroup=markdownMathDelimiter start="^\z(>\?\s*\)\zs\z(\$\{2,\}\).*$" end="^\z1\zs\z2\ze\s*$" keepend

syntax cluster markdownInline add=markdownCode,markdownMath

highlight default link markdownMathDelimiter Delimiter
