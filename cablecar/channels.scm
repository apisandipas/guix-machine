(define-module (cablecar channels)
  #:use-module (guix channels))

(list
 (channel
    (name 'rde)
    (url "https://github.com/abcdw/rde")
    (branch "master")
    (commit
        "0bb660a1039db52b0e4bcde07f07ee4bb534bc66")
    (introduction
        (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
            "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
  (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix"))
  (channel
    (name 'guix)
    (url "https://git.savannah.gnu.org/git/guix.git")
    (branch "master")
    (commit
        "f6afaf58b0a0b04e5023141c8f56c204f9779e19")
    (introduction
        (make-channel-introduction
        "9edb3f66fd807b096b48283debdcddccfea34bad"
        (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
