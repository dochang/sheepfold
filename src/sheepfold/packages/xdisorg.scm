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

;; (name "xss-lock")
(define-public xss-lock
  (let ((version "0.3.0")
        (revision "2")
        (commit "0c562bfa5d9073c6a47060ee06946aa39eff2fae"))
    (package/inherit gnu:xss-lock
      (version (git-version version revision commit))
      (source (origin
                (inherit (package-source gnu:xss-lock))
                (uri (git-reference
                      (url "https://github.com/wavexx/xss-lock")
                      (commit commit)))
                (file-name (git-file-name (package-name gnu:xss-lock) version))
                (sha256
                 (base32 "14mm6ra1hgwixvfzkd26r9flqp4nrx25fmwnm2qjpg7m9zhg6663"))))
      (home-page "https://github.com/wavexx/xss-lock"))))
