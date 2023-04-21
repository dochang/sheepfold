(define-module (sheepfold packages fontutils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages perl)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public afdko-perl-scripts
  (package
    (name "afdko-perl-scripts")
    (version "20190530")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adobe-type-tools/perl-scripts")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "18ds4axajz8w9mi4s7hjjgyki66b0kkqf18jnnk22621y8yd3m8i"))))
    (build-system copy-build-system)
    (inputs
     (list perl))
    (arguments
     (list #:install-plan
           #~`(("." ,(simple-format #f "libexec/~A/" #$name)
                #:include-regexp ("\\.pl$")))))
    (home-page "https://github.com/adobe-type-tools/perl-scripts")
    (synopsis "Perl scripts that are useful for font development")
    (description "This project includes an assortment of command-line Perl
scripts that are useful for font development, and run in terminal apps, such
as Terminal on macOS, and Command Prompt on Windows.")
    (license license:expat)))
