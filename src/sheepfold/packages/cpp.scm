(define-module (sheepfold packages cpp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (sheepfold packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cxx
  (package
    (name "cxx")
    (version "3.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xyproto/cxx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0yz8j2a87b45ifn6hq7pdylgzaz6xcpm0nrr82619rbr6vqim45q"))
              (patches
               (search-patches
                "cxx-make-scons-path-configurable.patch"))))
    (build-system gnu-build-system)
    (inputs
     ;; https://github.com/xyproto/cxx#requirements
     ;; https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=cxx
     ;; https://github.com/xyproto/cxx/blob/main/system/homebrew/cxx.rb
     (list scons
           gnu-make
           python-wrapper))
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list
         (string-append "PREFIX=" %output)
         (string-append "MAKE="  #$gnu-make "/bin/make")
         (string-append "SCONS="  #$scons "/bin/scons"))
      #:phases
      `(modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
              (string-append %output "/share/licenses"))
             ;; Why LICENSE is installed here?
             (let* ((out (assoc-ref outputs "out"))
                    (doc (simple-format #f "~A/share/doc/~A/"
                                        out (strip-store-file-name out)))
                    (examples (string-append doc "examples"))
                    (tests (string-append doc "tests")))
               (install-file "README.md" doc)
               (install-file "TODO.md" doc)
               (copy-recursively "examples" examples)
               (copy-recursively "tests" tests)))))))
    (home-page "https://github.com/xyproto/cxx")
    (synopsis "Utility for building, testing and packaging executables written in C++")
    (description "Configuration-free utility for building, testing and
packaging executables written in C++.  Can auto-detect compilation flags based
on includes, via the package system and pkg-config.")
    (license license:bsd-3)))
