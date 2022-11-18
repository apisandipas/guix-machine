(define-module (cablecar utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (rde features)
  #:use-module (rde features fontutils)
  #:use-module (rde features predicates)
  #:use-module (guix channels)
  #:use-module (guix inferior))


;; Predicates
(define-public (dotfile? x)
  (and (string? (car x))
       (or (file-like? (cadr x)) (gexp? (cadr x)))))

(define-public (state-item? x)
  (and (string? (car x)) (string? (cdr x))))

(define-public (list-of-dotfiles? x)
  (every dotfile? x))

(define-public (list-of-state-items? x)
  (every state-item? x))
