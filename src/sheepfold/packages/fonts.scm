(define-module (sheepfold packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
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
