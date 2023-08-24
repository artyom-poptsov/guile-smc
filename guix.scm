;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is part of Guile-SMC.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:


(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages admin)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tex)
             (gnu packages texlive)
             (gnu packages man))


(define %source-dir (dirname (current-filename)))


(define-public guile-smc
  (package
    (name "guile-smc")
    (version "git")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")     ;to prevent guild warnings
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'strip)
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (version (target-guile-effective-version))
                    (site-dir
                     (lambda (dir)
                       (string-append dir "/share/guile/site/" version)))
                    (go-dir
                     (lambda (dir)
                       (string-append dir "/lib/guile/" version "/site-ccache")))
                    (dirs (map cdr (append inputs outputs))))
               (wrap-program (string-append bin "/smc")
                 (list "GUILE_LOAD_PATH"
                       'prefix
                       (filter file-exists? (map site-dir dirs)))
                 (list "GUILE_LOAD_COMPILED_PATH"
                       'prefix
                       (filter file-exists? (map go-dir dirs))))))))))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           help2man
           texinfo
           which
           texlive
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib))
    (inputs
     (list bash-minimal
           guile-3.0
           guile-lib
           inetutils))
    (home-page "https://github.com/artyom-poptsov/guile-smc")
    (synopsis "GNU Guile state machine compiler")
    (description
     "Guile-SMC is a state machine compiler that allows users to describe
finite state machines (FSMs) in Scheme in terms of transition tables.  It is
capable to generate such transition tables from a @url{https://plantuml.com/,
PlantUML} state diagrams.

A transition table can be verified and checked for dead-ends and infinite
loops.  Also Guile-SMC FSMs gather statistics when they run.

Guile-SMC comes with a Scheme program called @command{smc} -- a state machine
compiler itself.  It produces a Scheme code for an FSM from the PlantUML
format.  This tool is meant to be called on a PlantUML file when a program
with a FSM is being built (for example, from a Makefile.)")
    (license gpl3)))

guile-smc
