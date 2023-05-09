(define-module (sheepfold packages documentation)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public c-tldr
  (package
    (name "c-tldr")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tldr-pages/tldr-c-client")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0mm02xwnz60cmf4f95ir77jndr4glmgqk8dsii6a3nvz3x4bjaf6"))))
    (build-system gnu-build-system)
    (inputs
     (list curl
           libzip))
    (native-inputs
     (list git
           pkg-config))
    (arguments
     (list
      #:tests? #f
      #:make-flags '(list (string-append "PREFIX=" %output))
      #:modules `(,@%gnu-build-system-modules
                  (ice-9 match))
      #:phases
      '(modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (ice-9 match))
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (match-lambda
                  ((ext dest)
                   (mkdir-p (dirname (string-append out dest)))
                   (copy-file (string-append "autocomplete/complete." ext)
                              (string-append out dest))))
                '(("bash" "/etc/bash_completion.d/tldr")
                  ("zsh" "/share/zsh/site-functions/_tldr")
                  ("fish" "/share/fish/vendor_completions.d/tldr.fish")))))))))
    (home-page "https://github.com/tldr-pages/tldr-c-client")
    (synopsis "C command-line client for tldr pages")
    (description "A command line client for @emph{tldr}, written in plain ISO
C90.")
    (license license:expat)))
