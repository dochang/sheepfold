(define-module (sheepfold packages image-viewers)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy))

(define-public nsxiv-extra
  (let ((commit "ad39c6be5e8041f069f19838dc0ee481dd2e650e")
        (revision "1"))
    (package
      (name "nsxiv-extra")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nsxiv/nsxiv-extra")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "089i64a6d1sgwhij06vdlqc1aby7l8rkjr1i2w0dihi5s340lbfy"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(,@(apply append
                    (map (lambda (script)
                           (list
                            (list (simple-format #f "scripts/~A/~A" script script) "bin/")
                            (list (simple-format #f "scripts/~A/README.md" script)
                                  (simple-format #f "share/doc/nsxiv-extra/~A/" script))))
                         '("nsxiv-env" "nsxiv-pipe" "nsxiv-rifle" "nsxiv-saver" "nsxiv-thumb" "nsxiv-url"))))))
      (home-page "https://github.com/nsxiv/nsxiv-extra")
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
