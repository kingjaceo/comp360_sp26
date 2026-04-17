#lang br/quicklang

(define-macro (particool-begin (simulation DEFN ...))
  ;  (with-pattern
  ;      ([((b-line NUM STMT ...) ...) #'(LINE ...)]
  ;       [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))])
  #'(#%module-begin
     'DEFN ...   ; we can put in a SHIMS ' to see what the expander is producing
     ))
(provide (rename-out [particool-begin #%module-begin]))

(define-macro (system-defn ID FEATURES ...)
  (with-pattern
      ([P-COUNT (find-property 'particles #'(FEATURES ...))]
       )
    (define (system (synatax->datum ID)
                    P-COUNT
                    0 0
                    ))))

(define-macro (sim-defn FEATURE ...))

; data structures
(struct point (x y))
(struct system (name particles center velocity))
    
(define (simulate) (displayln "this should start the simulatoin!"))