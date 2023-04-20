(define-module (sheepfold packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

;; https://osdn.net/dl/hanazono-font/hanazono-20170904.zip
;; https://osdn.net/projects/hanazono-font/downloads/68253/hanazono-20170904.zip/
;; https://osdn.net/frs/redir.php?m=rwthaachen&f=hanazono-font%2F68253%2Fhanazono-20170904.zip
(define-public font-hanazono
  (package
    (name "font-hanazono")
    (version "20170904")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (simple-format #f "~A?m=~A&f=hanazono-font%2F~A%2Fhanazono-~A.zip"
                                  "https://osdn.net/frs/redir.php"
                                  "rwthaachen" "68253" version))
              ;; "url-fetch" doesn't support redirection.  Use the real URL.
              (file-name (string-append name "-" version ".zip"))
              (sha256
               (base32 "0wlmkhdd95yhrnapln5hxd8a82h55i2c43v45mj0rnp7kahd872p"))))
    (build-system font-build-system)
    (home-page "http://fonts.jp/hanazono/")
    (synopsis "Japanese TrueType mincho font by KAGE system and FontForge")
    (description "This font is Japanese KANJI free font.  The KAGE system and
FontForge are used for the generation of this font.  Collected glyph uses all
the data registered in glyphwiki.")
    (license (list license:silofl1.1
                   (license:non-copyleft "file://LICENSE.txt")))))

(define (hanazono-afdko-package name version font-family-name ideographs base32sum)
  (package
    (name name)
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cjkvi/HanaMinAFDKO"
                                  "/releases/download/" version
                                  "/HanaMin" font-family-name ".otf"))
              (sha256
               (base32 base32sum))))
    (build-system font-build-system)
    (home-page "https://github.com/cjkvi/HanaMinAFDKO")
    (synopsis (string-append "Hanazono Mincho Font "
                             font-family-name
                             " created by AFDKO"))
    (description (string-append "This version of Hanazono Mincho is created
from GlyphWiki data.  This font is created by AFDKO, and covers entire CJK
Unified Ideographs, Compatibility Ideographs, Kanji and Kana characters, and
other related characters created by GlyphWiki system.

This font family mainly covers " ideographs ".  For all the families, install
@code{font-hanazono-afdko}."))
    (license (license:non-copyleft
              (string-append
               "https://glyphwiki.org/wiki/GlyphWiki:"
               "%e3%83%87%e3%83%bc%e3%82%bf%e3%83%bb%e8%a8%98%e4%ba%8b"
               "%e3%81%ae%e3%83%a9%e3%82%a4%e3%82%bb%e3%83%b3%e3%82%b9")))))

(define-syntax define-font-hanazono-afdko-package
  (syntax-rules ()
    ((_ name version font-family-name ideographs base32sum)
     (define-public name
       (hanazono-afdko-package (symbol->string 'name) version font-family-name ideographs base32sum)))))

(define font-hanazono-afdko-version "8.030")

;; (name "font-hanazono-afdko-a")
(define-font-hanazono-afdko-package font-hanazono-afdko-a
  font-hanazono-afdko-version "A"
  "BMP Ideographs"
  "1471mvzllldvgx4nj6rlrr309gmf72a3z0yw4f6q54g0yzjgns7h")

;; (name "font-hanazono-afdko-b")
(define-font-hanazono-afdko-package font-hanazono-afdko-b
  font-hanazono-afdko-version "B"
  "Extension B Ideographs"
  "0vnl9z7bb7482lasp8vxx5z33iiz7nq5dfgazfwlnnzifwajvv6g")

;; (name "font-hanazono-afdko-c")
(define-font-hanazono-afdko-package font-hanazono-afdko-c
  font-hanazono-afdko-version "C"
  "Extension C to up and SIP Compatibility Ideographs"
  "0nyfhcc07fnp8arm11qgc6dzcni2gvbc9yrfdw28rwan1qxdb547")

;; (name "font-hanazono-afdko-ex-a-1")
(define-font-hanazono-afdko-package font-hanazono-afdko-ex-a-1
  font-hanazono-afdko-version "ExA1"
  "URO Ideographs"
  "0rrwh891ambkl8g3icgzqgmps9nqq5viwa2dmrxa5d423lx42ngz")

;; (name "font-hanazono-afdko-ex-a-2")
(define-font-hanazono-afdko-package font-hanazono-afdko-ex-a-2
  font-hanazono-afdko-version "ExA2"
  "Ideographs Extension A and various other characters"
  "1sfpb7rifagj2hn4aa53dy7rccjcn158kflc9ljf1vrm7661ry7d")

;; (name "font-hanazono-afdko-ex-b")
(define-font-hanazono-afdko-package font-hanazono-afdko-ex-b
  font-hanazono-afdko-version "ExB"
  "Ideographs Extension B"
  "1sgh4qcjr5is07fp3lpaclhgyl174pg89w870a35abpmwm8ywnhc")

;; (name "font-hanazono-afdko-ex-c")
(define-font-hanazono-afdko-package font-hanazono-afdko-ex-c
  font-hanazono-afdko-version "ExC"
  "Extension C to up and SIP Compatibility Ideographs"
  "1wvdlfqr5hiaz44y760fhnyhbvmawww4nisdhxwawbidxcpj5s16")

;; (name "font-hanazono-afdko-i")
(define-font-hanazono-afdko-package font-hanazono-afdko-i
  font-hanazono-afdko-version "I"
  "IDS fglyphs (via GSUB @emph{ccmp} feature)"
  "0ihmgl755ayw6spsgw52xwq6k0sa80j3nclr45h5rdzbhygqdyxd")

(define-public font-hanazono-afdko
  (package
    (name "font-hanazono-afdko")
    (version font-hanazono-afdko-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cjkvi/HanaMinAFDKO")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "00kbb69fjcxmwjdlrpf05vnc13fa25pnr2zhd6m2fblyy9r5bnf6"))))
    (build-system copy-build-system)
    (propagated-inputs
     (list font-hanazono-afdko-a
           font-hanazono-afdko-b
           font-hanazono-afdko-c
           font-hanazono-afdko-ex-a-1
           font-hanazono-afdko-ex-a-2
           font-hanazono-afdko-ex-b
           font-hanazono-afdko-ex-c
           font-hanazono-afdko-i))
    (arguments
     (list #:install-plan
           #~`(("README.md" ,(simple-format #f "share/doc/~A-~A/"
                                            #$name #$version)))))
    (home-page "https://github.com/cjkvi/HanaMinAFDKO")
    (synopsis "Hanazono Mincho Font created by AFDKO")
    (description "This version of Hanazono Mincho is created from GlyphWiki
data.  This font is created by AFDKO, and covers entire CJK Unified
Ideographs, Compatibility Ideographs, Kanji and Kana characters, and other
related characters created by GlyphWiki system.

This package installs all the font families.")
    (license (license:non-copyleft
              (string-append
               "https://glyphwiki.org/wiki/GlyphWiki:"
               "%e3%83%87%e3%83%bc%e3%82%bf%e3%83%bb%e8%a8%98%e4%ba%8b"
               "%e3%81%ae%e3%83%a9%e3%82%a4%e3%82%bb%e3%83%b3%e3%82%b9")))))
