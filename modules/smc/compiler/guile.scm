;;; guile.scm -- Guile-SMC state machine compiler procedures.

;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; The procedures in this module produce a Scheme code for GNU Guile.


;;; Code:

(define-module (smc compiler guile)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (smc compiler guile-common)
  #:use-module (smc core common)
  #:use-module (smc core state)
  #:use-module (smc core log)
  #:use-module (smc version)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc config)
  #:re-export (form-feed
               write-header
               write-parent-fsm-info)
  #:export (write-module
            write-use-modules
            write-transition-table
            write-define-class
            write-initialize
            copy-dependencies))



(define* (write-module module
                       #:key
                       extra-modules
                       class-name
                       port
                       (standalone-mode? #f))
  "Write a @code{define-module} part to a @var{port}. @var{class-name} is used
to export a FSM class in the @code{#:export} part. @var{extra-modules} allow
to specify a list of extra modules that required for the output FSM to work."
  (let loop ((lst `(define-module ,module
                     #:use-module (oop goops)
                     #:use-module ,(if standalone-mode?
                                       (append module '(smc fsm))
                                       '(smc fsm))))
             (em  extra-modules))
    (if (or (not em) (null? em))
        (begin
          (pretty-print (append lst `(#:re-export (fsm-run!)
                                      #:export    (,class-name)))
                        port)
          (newline port))
        (loop (append lst `(#:use-module ,(car em)))
              (cdr em)))))

(define (write-use-modules extra-modules port)
  "Write 'use-modules' section to the @var{port}."
  (let loop ((lst `(use-modules (smc fsm) (oop goops)))
             (em  extra-modules))
    (if (or (not em) (null? em))
        (begin
          (display lst port)
          (newline port))
        (loop (append lst (list (car em)))
              (cdr em)))))

(define-method-with-docs (write-transition-table (fsm <fsm>) (port <port>))
  "Write a @var{fsm} transition table to a @var{port}."
  (let ((table (fsm-transition-table fsm)))
    (pretty-print
     `(define %transition-table
        ,(list 'quasiquote
               (map state->list/serialized
                    (hash-table->transition-list table))))
     port)))

(define (write-define-class class-name port)
  "Write @code{define-class} for a @var{class-name} to a @var{port}."
  (pretty-print `(define-class ,class-name (<fsm>))
                port))

(define (write-initialize fsm class-name port)
  "Write the class constructor for @var{class-name} to the @var{port}."
  (pretty-print
   `(define-method (initialize (self ,class-name) initargs)
      (next-method)
      (fsm-description-set! self ,(fsm-description fsm))
      (fsm-event-source-set! self ,(and (fsm-event-source fsm)
                                        (procedure-name (fsm-event-source fsm))))
      (fsm-transition-table-set!
       self
       (transition-list->hash-table self %transition-table))
      (fsm-current-state-set! self
                              (fsm-state self
                                         (quote ,(state-name (fsm-current-state fsm))))))
   port))



(define (copy-file/substitute source destination substitutes)
  (let ((src (open-input-file source))
        (dst (open-output-file destination)))
    (let loop ((line (read-line src)))
      (if (eof-object? line)
          (begin
            (close-port src)
            (close-port dst))
          (let ((ln (let matcher-loop ((subs substitutes)
                                       (ln   line))
                      (if (null? subs)
                          ln
                          (let ((s (car subs)))
                            (matcher-loop (cdr subs)
                                          (regexp-substitute/global #f
                                                                    (car s)
                                                                    ln
                                                                    'pre
                                                                    (cdr s)
                                                                    'post)))))))
            (write-line ln dst)
            (loop (read-line src)))))))

(define (mkdir* path)
  "Create directories from a PATH recursively."
  (let loop ((dirparts (string-split path #\/))
             (dir      ""))
    (unless (null? dirparts)
      (let ((d (string-append dir (car dirparts) "/")))
        (unless (file-exists? d)
          (mkdir d))
        (loop (cdr dirparts) d)))))

(define (modules->paths load-path modules)
  "Locate each module from a MODULES list in the directories from a LOAD-PATH
list.  Return a list of pairs (module-file full-path)."
  (let mod-loop ((mods   modules)
                 (result '()))
    (if (null? mods)
        result
        (let* ((mod       (car mods))
               (mod-file  (string-append (string-join (map symbol->string mod)
                                                      "/")
                                         ".scm"))
               (full-path (let path-loop ((paths load-path))
                            (if (null? paths)
                                #f
                                (let ((path (string-append (car paths)
                                                           "/"
                                                           mod-file)))
                                  (if (file-exists? path)
                                      path
                                      (path-loop (cdr paths))))))))
          (if full-path
              (mod-loop (cdr mods)
                        (cons (cons mod-file full-path) result))
              (mod-loop (cdr mods)
                        result))))))

(define (copy-dependencies output-directory root-module-name extra-modules)
  "Copy dependencies to a sub-directory with ROOT-MODULE-NAME of an
OUTPUT-DIRECTORY."

  (define (copy src dst substitutes)
    (let ((dir (dirname dst)))

      (unless (file-exists? dir)
        (log-debug "  creating \"~a\" ..." dir)
        (mkdir* (dirname dst))
        (log-debug "  creating \"~a\" ... done" dir))
      (log-debug "  copying: ~a -> ~a ..." src dst)
      (copy-file/substitute src dst substitutes)
      (log-debug "  copying: ~a -> ~a ... done" src dst)))

  (let* ((target-dir (format #f
                             "~a/~a"
                             output-directory
                             (string-join (map symbol->string root-module-name)
                                          "/")))
         (files      (list "/fsm.scm"
                           "/core/common.scm"
                           "/core/config.scm"
                           "/core/log.scm"
                           "/core/state.scm"
                           "/core/stack.scm"
                           "/core/transition.scm"
                           "/context/port.scm"
                           "/context/char-context.scm"
                           "/context/binary.scm"
                           "/context/context.scm"))
         (substitutes (list (cons "\\(smc "
                                  (format #f
                                          "(~a smc "
                                          (string-join (map symbol->string
                                                            root-module-name)
                                                       " ")))))
         (substitutes-smc (cons (cons ";;; Commentary:"
                                      (string-append
                                       ";;; Commentary:\n\n"
                                       (format #f ";; Copied from Guile-SMC ~a~%"
                                               (smc-version/string))))
                                substitutes))
         (substitutes-extra (cons (cons "\\(define-module \\("
                                        (format #f
                                                "(define-module (~a "
                                                (string-join (map symbol->string
                                                                  root-module-name)
                                                             " ")))
                                  substitutes)))

    (log-debug "Copying core modules ...")
    (for-each (lambda (file)
                (let* ((src (string-append %guile-smc-modules-directory file))
                       (dst (string-append target-dir "/smc/" file)))
                  (copy src dst substitutes-smc)))
              files)
    (log-debug "Copying core modules ... done")

    (log-debug "Copying extra modules ...")
    (for-each (lambda (mod)
                (let* ((src (cdr mod))
                       (dst (string-append target-dir "/" (car mod))))
                  (copy src dst substitutes-extra)))
              (modules->paths %load-path extra-modules))
    (log-debug "Copying extra modules ... done")))

;;; guile.scm ends here.
