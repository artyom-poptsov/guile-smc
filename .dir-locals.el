;;; .dir-locals.el -- Per-directory local variables for GNU Emacs 23 and later.

((nil         . ((fill-column  . 78)
                 (tab-width    .  8)
                 (sentence-end-double-space . t)))
 (scheme-mode . ((indent-tabs-mode . nil)
                 (eval . (put 'test-assert 'scheme-indent-function 1))
                 (eval . (put 'test-error 'scheme-indent-function 1))
                 (eval . (put 'test-equal 'scheme-indent-function 1))
                 (eval . (put 'test-assert-with-log 'scheme-indent-function 1)))))

;;; .dir-locals.el ends here

