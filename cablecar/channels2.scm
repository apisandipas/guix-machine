(define-module (cablecar channels)
  #:use-module (guix channels))

(list
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'cablecar-config)
  (url "https://github.com/apiasandipas/guix-machine")
  (branch "main")
  (introduction
   (make-channel-introduction
<<<<<<< HEAD
    "904688444d6cba3e255a8703423159f6e587c947"
=======
    "1a486fd21e4d0b1a5494b4314326153beef57810"
>>>>>>> ac10737 (troubleshotting sigining commits)
    (openpgp-fingerprint
     "7ADC 2A21 674D A808 BF72  EC48 1447 CBC3 E2E6 8A6A")))))
