" Allow spaces before tagged function.
syntax clear jsTaggedTemplate
syntax match jsTaggedTemplate '\<\K\k*\s*\ze`' nextgroup=jsTemplateString
