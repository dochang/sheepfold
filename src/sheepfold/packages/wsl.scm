(define-module (sheepfold packages wsl)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages bash)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wslu
  (package
    (name "wslu")
    (version "4.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wslutilities/wslu")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0rvdxcsx14fh8awrnfpmgpjz7sickgdxgvml964881psrn3s06ya"))))
    (build-system gnu-build-system)
    (inputs
     (list bc
           psmisc
           imagemagick))
    (native-inputs
     (list gzip
           shellcheck
           bats))
    (arguments
     `(#:tests? #f
       ;; `make test' requires reading and writing the external file system.
       ;; I don't know how to do that.  Disable it for now.
       #:test-target "test"
       #:make-flags
       (list (string-append "DESTDIR=" %output) ;; For /etc
             "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let ((bash (or (and=> (assoc-ref (or native-inputs inputs) "bash")
                                    (lambda (in) (string-append in "/bin/bash")))
                             "/bin/sh")))
               (invoke bash "./configure.sh" "--build"))))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (simple-format #f "~A/share/doc/~A/"
                                        out (strip-store-file-name out))))
               (install-file "THIRD_PARTY_LICENSE" doc)))))))
    (home-page "https://wslutiliti.es/wslu/")
    (synopsis "Collection of utilities for Windows Subsystem for Linux")
    (description "A collection of utilities for the Linux Subsystem for
Windows (WSL), such as converting Linux paths to Windows paths or creating
Linux application shortcuts on the Windows Desktop.

@itemize
@item Requires at least Windows 10 Creators Update;
@item Some of the features require a higher version of Windows;
@item Supports WSL2;
@item Supports Windows 11.
@end itemize\n")
    (license (list license:gpl3
                   license:expat))))
