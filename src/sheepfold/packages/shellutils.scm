(define-module (sheepfold packages shellutils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public xenv
  (package
    (name "xenv")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://download.gnu.org.ua/release/xenv/xenv-"
                              version ".tar.gz"))
              (sha256
               (base32 "03ajnsgzvicjp49g24g9sv5iglk7iwjzzzrydb9v1v7faaaf9sx5"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org.ua/software/xenv/manual/")
    (synopsis "Specialized preprocessor that expands environment variables in its input")
    (description "Xenv reads input files and prints on the standard output
their contents, replacing references to the environment variables with their
values and shell commands with the output they produce.")
    (license license:gpl3)))
