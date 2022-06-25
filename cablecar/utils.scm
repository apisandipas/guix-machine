(define-module (cablecar utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (rde features)
  #:use-module (rde features fontutils)
  #:use-module (rde features predicates)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:export (pkgs pkgs-vanilla))

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define* (pkgs-vanilla #:rest lst)
  "Packages from guix channel."
  (define channel-guix
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git")
           (commit
            "2b6af630d61dd5b16424be55088de2b079e9fbaf"))))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

   (map get-inferior-pkg lst))

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
