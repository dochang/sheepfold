(define-module (sheepfold packages image-viewers)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp))

(define-public nsxiv-extra
  (let ((commit "eb9c24b8a89bd9c58bf3238ffae7d7f385984768")
        (revision "2"))
    (package
      (name "nsxiv-extra")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/nsxiv/nsxiv-extra.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "13zjspxlsz70h3cw7vl8q9m2ajlh4mda29fqwin5m13xvd3r7czv"))))
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             ;; No need to use `with-imported-modules' because `(srfi srfi-1)'
             ;; and `(ice-9 ftw)' are both Guile internal modules.
             #~(begin
                 (use-modules (srfi srfi-1))
                 (append-map
                  (lambda (script)
                    (let* ((src (string-append "scripts/" script "/"))
                           (stripped (strip-store-file-name #$output))
                           (doc (simple-format #f "share/doc/~A/~A/"
                                               stripped script)))
                      (list
                       (list (string-append src script) "bin/")
                       (list (string-append src "README.md") doc))))
                  '("nsxiv-anti-alias"
                    "nsxiv-env"
                    "nsxiv-fill"
                    "nsxiv-open"
                    "nsxiv-pipe"
                    "nsxiv-rifle"
                    "nsxiv-saver"
                    "nsxiv-thumb"
                    "nsxiv-url")))))
      (home-page "https://codeberg.org/nsxiv/nsxiv-extra")
      (synopsis "Extra scripts for nsxiv")
      (description "This package provides scripts which add new and commonly
requested functionality to @code{nsxiv}.")
      (license
       ((@@ (guix licenses) license)
        "No License"
        "https://choosealicense.com/no-permission/"
        (string-append "https://docs.github.com/en/repositories"
                       "/managing-your-repositorys-settings-and-features"
                       "/customizing-your-repository"
                       "/licensing-a-repository"
                       "#choosing-the-right-license"))))))
