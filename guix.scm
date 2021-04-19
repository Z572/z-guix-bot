(use-modules (guix)
             (guix git-download)
             (gnu packages guile-xyz)
             (guix packages)
             (gnu packages pkg-config))

(define z-guix-irc
  (package
    (inherit guile-irc)
    (name "z-guix-irc")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Z572/guile-irc")
             (commit "fe456fc853d1b27109076a99d666dfe8a60d6ccf")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bv668m2w7hk8hdnym1z8i6rcplyma5p3jfpyi6s7q2pm3vfs2mb"))))
    (arguments `(#:make-flags (list "GUILE_AUTO_COMPILE=0")
                 ,@(package-arguments guile-irc)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ,@(package-native-inputs guile-irc)))))
z-guix-irc
