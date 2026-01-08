; extends

; Fix double indentation when function expressions are used as arguments.
; Before:
;   f(function x(
;   ) {
;       return 1;
;     });
; After:
;   f(function x(
;   ) {
;     return 1;
;   });
((function_expression
  parameters: (formal_parameters) @params
  body: (statement_block) @body)
  (#set! "indent.start_at_same_line" @params))
