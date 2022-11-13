(define-module (cablecar systems norrin)
  #:use-module (cablecar systems)
  #:use-module (rde features system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (
            %systems-swap
            %system-features))

(define-public %system-swap
  (swap-space
   (target (file-system-label "swap-partition"))))

(define-public %system-features
  (list
   (feature-host-info
    #:host-name "norrin"
    #:timezone  "America/Chicago")
   (feature-kernel
    #:kernel linux
    #:initrd microcode-initrd
    #:firmware (list linux-firmware))
   (feature-file-systems
    ;; #:mapped-devices norrin-mapped-devices
    #:file-systems %cablecar-generic-file-systems)))
