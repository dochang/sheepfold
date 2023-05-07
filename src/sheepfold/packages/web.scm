(define-module (sheepfold packages web)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages tls)
  #:use-module (guix gexp)
  #:use-module (sheepfold packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wrk
  (package
    (name "wrk")
    (version "4.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wg/wrk")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0wvm6p8qdlsdgw39n39bvgva65avrb84dshx73blmvi9iviw09ww"))
              (patches
               (search-patches
                "wrk-fix-include-path.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list luajit
           openssl))
    (arguments
     (list
      #:tests? #f ;; No target `check'.
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "WITH_LUAJIT=" #$luajit)
              (string-append "WITH_OPENSSL=" #$openssl)
              (string-append "VER=" #$version))
      #:phases
      `(modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (simple-format #f "~A/share/doc/~A/"
                                        out (strip-store-file-name out)))
                    (scripts (string-append doc "/scripts")))
               (install-file "wrk" bin)
               (for-each (lambda (f)
                           (install-file f doc))
                         '("CHANGES" "NOTICE" "README.md" "SCRIPTING"))
               (copy-recursively "scripts" scripts)))))))
    (home-page "https://github.com/wg/wrk")
    (synopsis "Modern HTTP benchmarking tool")
    (description "wrk is a modern HTTP benchmarking tool capable of generating
significant load when run on a single multi-core CPU.  It combines a
multithreaded design with scalable event notification systems such as epoll
and kqueue.

An optional LuaJIT script can perform HTTP request generation, response
processing, and custom reporting.  Details are available in @file{SCRIPTING}
and several examples are located in @file{scripts/}.")
    (license (list (license:non-copyleft "file://LICENSE")
                   (license:non-copyleft "file://NOTICE")))))
