" Allow "-" as an annotation name in doc comments
syntax clear phpDocCustomTags
syntax match phpDocCustomTags "@[a-zA-Z-]*\(\s\+\|\n\|\r\)" containedin=phpComment
