(define-module (cablecar configs bryan)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features password-utils)
  #:use-module (rde features mail)
  #:use-module (gnu system keyboard)
  #:export (%user-features))

;;; User-specfics settings
(define* (mail-acc id user #:optional (type 'gmail))
  "Make a simple mail-account with gmail type by default."
  (mail-account
   (id   id)
   (fqda user)
   (type type)))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define %user-features
  (list (feature-user-info
         #:user-name "bryan"
         #:full-name "Bryan Paronto"
         #:email "bryan@cablecar.digital"
         #:user-initial-password-hash
         "$6$abc$JmjYWp0yTCBlvqR4m19zKTi2C3ErCHdW9xXCmNWbhmHhFqfDnMPPW17ieYSYd3sXPisEoG5JKEZ0c4xtP/y6g/"
         #:emacs-advanced-user? #t)
        (feature-gnupg
         #:gpg-primary-key "1447CBC3E2E68A6A")
        ;; TODO pull down passcrypt repo
        (feature-password-store
         #:remote-password-store-url "ssh://git@github.com:apisandipas/passcrypt.git")
        ;; TODO configure mbsync
        ;; TODO configure mu4e
        (feature-mail-settings
         #:mail-accounts (list
                          (mail-acc 'work       "bryan@cablecar.digital")
                          (mail-acc 'personal   "bparonto@gmail.com"))
         #:mailing-lists (list
                          (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))
        (feature-keyboard
         #:keyboard-layout
         (keyboard-layout "us"
                          #:options '("ctrl:nocaps" "acer_laptop")))))
