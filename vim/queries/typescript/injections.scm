; extends
(call_expression
  function: (member_expression
    property: (property_identifier) @_name
    (#any-of? @_name "html" "svg"))
  arguments: [
    (arguments
      (template_string) @injection.content)
    (template_string) @injection.content
  ]
  (#offset! @injection.content 0 1 0 -1)
  (#set! injection.include-children)
  (#set! injection.language "html"))
