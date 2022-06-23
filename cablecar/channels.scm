(define-module (cablecar channels)
  #:use-module (guix channels))

(list
 (channel
  (name 'cablecar-config)
  (url "http://github.com/apisandipas/guix-machine.git")
  (branch "main")
  (commit "ce7db55fc8920e2c7d4d79b945b35fc678dd35ea")
  (introduction
   (make-channel-introduction
    "ce7db55fc8920e2c7d4d79b945b35fc678dd35ea"
    (openpgp-fingerprint
     "7ADC 2A21 674D A808 BF72  EC48 1447 CBC3 E2E6 8A6A"))))
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (commit
   "e9bd919270f7075b47014664a70b9e8558404355")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
