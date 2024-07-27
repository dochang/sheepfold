(define-module (sheepfold packages xdisorg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((gnu packages xdisorg) #:prefix gnu:))

;; (name "unclutter-xfixes")
(define-public unclutter-xfixes
  (package/inherit gnu:unclutter-xfixes
    (version "1.6")
    (source (origin
              (inherit (package-source gnu:unclutter-xfixes))
              (uri (git-reference
                    (inherit (origin-uri (package-source gnu:unclutter-xfixes)))
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name gnu:unclutter-xfixes) version))
              (sha256
               (base32 "1mqir7imiiyl7vrnnnid80kb14fh78acrkffcm3z1l3ah9madqmj"))))))
