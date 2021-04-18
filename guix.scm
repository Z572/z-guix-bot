(use-modules (guix)
             (guix git)
             (gnu packages guile-xyz)
             (gnu packages pkg-config))

(package
  (inherit guile-irc)
  (version "0.3.0")
  (source
   (git-checkout
    (url "https://github.com/Z572/guile-irc")))
  (arguments `(#:make-flags (list "GUILE_AUTO_COMPILE=0")
               ,@(package-arguments guile-irc)))
  (native-inputs
   `(("pkg-config" ,pkg-config)
     ,@(package-native-inputs guile-irc))))
