;;; inline-anki-extras.el -*- lexical-binding: t; -*-

;; DEPRECATED: Found myself just trying to infer the answer from the length of
;; the words, which ruins the test!  I'll wager the Anki creator had a similar
;; experience, thus the 3-dot default.  I still want some indication of the
;; answer's length, thus I made `inline-anki-dots-logarithmic'.
(defun inline-anki-dots-for-letters (text)
  "Return TEXT with all letters and digits replaced by dots.
Useful as placeholder in a cloze-deletion, so you can still see
how long the clozed part is.  Blank spaces and dashes will remain
visible as in the original text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward "[[:alnum:]]" nil t)
        (replace-match ".")))
    (buffer-string)))


(provide 'inline-anki-extras)
;;; inline-anki-extras.el ends here
