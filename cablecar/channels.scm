(define-module (cablecar channels)
  #:use-module (guix channels))

(list
  (channel
  (name 'rekahsoft)
  (url "https://git.rekahsoft.ca/rekahsoft/rekahsoft-guix")
  (branch "master")
  (commit
   "4dcfd8731e3dbcd40c1812c293726a30048108cb"))
 (channel
  (name 'engstrand-config)
  (url "https://github.com/engstrand-config/guix-dotfiles.git")
  (branch "main")
  (commit "5873684600188f99472f6292685877596711098d"))
 (channel
  (name 'channel-x)
  (url "https://github.com/daviwil/channel-x")
  (branch "master")
  (commit
   "0c442c2479fc86fed791e401663d964c999732bf"))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (branch "master")
  (commit
   "f82ea2e72a3e78e0ad6f3b71bba66dead11f66bb")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 (channel
  (name 'flat)
  (url "https://github.com/flatwhatson/guix-channel.git")
  (branch "master")
  (commit
   "657da22f0229b978b7bf4e4d476f59f17f6a175f")
  (introduction
   (make-channel-introduction
    "33f86a4b48205c0dc19d7c036c85393f0766f806"
    (openpgp-fingerprint
     "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit
   "1a122e06fe046caebf39395edc797515861acd3b")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
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
