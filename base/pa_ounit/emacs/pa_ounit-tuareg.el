;; Extend tuareg to support pa_ounit keywords.
;; To enable just put this file in your site lisp and add
;;   (require 'pa_ounit-tuareg)
;; to your .emacs file.

(defconst tuareg-pa_ounit-keywords
  (concat "\\<" (regexp-opt '("TEST" "TEST_UNIT" "TEST_MODULE") t) "\\>")
  "Regexp for keywords used for new definitions with pa_ounit")

(font-lock-add-keywords
 'tuareg-mode
 `((,tuareg-pa_ounit-keywords . tuareg-font-lock-governing-face)))

(add-hook
 'tuareg-mode-hook
 '(lambda ()
    (setq tuareg-governing-phrase-regexp
          (concat tuareg-governing-phrase-regexp
                  "\\|"
                  tuareg-pa_ounit-keywords))))

(provide 'pa_ounit-tuareg)
