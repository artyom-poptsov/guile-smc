(use-modules (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(package
 (name "guile-smc")
 (version "0.2.0")
 (source (string-append "./" name "-" version ".tar.gz"))
 (build-system gnu-build-system)
 (native-inputs
  `(("autoconf" ,autoconf)
    ("automake" ,automake)
    ("pkg-config" ,pkg-config)
    ("texinfo" ,texinfo)))
 (inputs `(("guile" ,guile-2.2)))

 (arguments
  '(#:phases (modify-phases
              %standard-phases
              (add-after 'unpack 'autoreconf
                         (lambda _
                           (zero? (system* "autoreconf" "-vfi")))))))

  (home-page "https://github.com/artyom-poptsov/guile-smc")
  (synopsis "GNU Guile State Machine Compiler")
  (description
   "GNU Guile State Machine Compiler")
  (license gpl3+))
