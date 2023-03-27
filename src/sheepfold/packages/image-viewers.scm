(define-module (sheepfold packages image-viewers)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial))

(define-public nsxiv-extra
  (let ((commit "af05da591727f80d52525d987e1d9894eae8754e")
        (revision "0"))
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
                 (base32 "0kaxprbi86xl6j9rxwpg1ag9wk6rkvndwdk9237b87na5q224zlm"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils)
                        (ice-9 ftw))
           (let* ((scripts-dir (string-append (assoc-ref %build-inputs "source")
                                              "/scripts"))
                  (out (assoc-ref %outputs "out"))
                  (bin (string-append out "/bin"))
                  (doc-root (string-append out "/share/doc/nsxiv-extra")))
             (with-directory-excursion scripts-dir
               (for-each (lambda (d)
                           (with-directory-excursion d
                             (install-file d bin)
                             (let ((doc (string-append doc-root "/" d)))
                               (install-file "README.md" doc))))
                         (scandir "."
                                  (lambda (fname)
                                    (not (or (string= fname ".")
                                             (string= fname ".."))))))))
           #t)))
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
