(define-module (sheepfold packages ime)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((gnu packages logging) #:select (glog-next))
  #:use-module ((gnu packages ibus) #:prefix gnu:))

(define-public librime
  (package/inherit gnu:librime
    (version "1.16.1")
    (source
     (origin
       (inherit (package-source gnu:librime))
       (uri (git-reference
              (inherit (origin-uri (package-source gnu:librime)))
              (commit version)))
       (file-name (git-file-name (package-name gnu:librime) version))
       (sha256
        (base32 "1x8sa4y996kdvvkbk1aqa6jb7qhm19ir2n0b4wclsxvzzd53mfi5"))))
    (inputs
     (modify-inputs (package-inputs gnu:librime)
       (replace "glog" glog-next)))))
