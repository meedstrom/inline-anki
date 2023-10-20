;;; inline-anki.el --- Embed implicit flashcards in flowing text -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Martin Edström <meedstrom91@gmail.com>
;;
;; Description: One-liner flashcards
;; Author: Martin Edström
;; Version: 0.1.1-pre
;; Created: 2023-09-19
;; Package-Requires: ((emacs "28") (asyncloop "0.3.2") (pcre2el "1.12") (request "0.3.0") (dash "2.12.0"))
;; URL: https://github.com/meedstrom/inline-anki

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Embed implicit flashcards in flowing text.
;;
;; More info at https://github.com/meedstrom/inline-anki
;;
;; Required programs:
;; - curl
;; - Anki with AnkiConnect add-on
;;
;; Recommended initfile snippet:
;;
;; (with-eval-after-load 'org
;;   (add-to-list 'org-structure-template-alist '("f" . "flashcard")))

;;; Code:

(defgroup inline-anki nil
  "Customizations for inline-anki."
  :group 'org)

(require 'inline-anki-anki-editor-fork)
(require 'map)

(defcustom inline-anki-deck "Default"
  "Name of deck to upload to."
  :type 'string)

(defcustom inline-anki-cloze-note-type "Cloze"
  "Name of your cloze note type."
  :type 'string)

(defcustom inline-anki-cloze-note-fields '("Text" "Extra")
  "Names of fields in your note type."
  :type '(repeat string))

(defcustom inline-anki-emphasis-type "_"
  "The kind of emphasis you want to indicate a cloze deletion.
Whatever you choose, it MUST be found in `org-emphasis-alist' and
can only be one character long."
  :type 'string)

(defcustom inline-anki-use-tags t
  "Whether to send Org tags to Anki.
Setting it to nil lets `inline-anki-push-notes-in-directory' work faster if
you have hundreds of files.

If you merely want to exclude parent tags, leave this at t and
configure `org-use-tag-inheritance' instead."
  :type 'boolean)

(defconst inline-anki-rx:list-bullet
  (rx (or (any "-+*") (seq (*? digit) (any ").") " "))))

(defconst inline-anki-rx:item-start:new
  (rx bol (*? space) (regexp inline-anki-rx:list-bullet) (*? space) "@anki "))

(defconst inline-anki-rx:item-start
  (rx bol (*? space) (regexp inline-anki-rx:list-bullet) (*? space) "@^{" (group (= 13 digit)) "}"))

(defconst inline-anki-rx:eol:new
  (rx (or "@anki" "^{anki}") (*? space) eol))

(defconst inline-anki-rx:eol
  (rx (? "@") "^{" (group (= 13 digit)) "}" (*? space) eol) )

(defconst inline-anki-rx:struct:new
  (rx bol (*? space) "#+begin_flashcard" (*? space) eol))

(defconst inline-anki-rx:struct
  (rx bol (*? space) "#+begin_flashcard " (group (= 13 digit))))

(defvar inline-anki--file-list nil
  "Internal use only.")

(defvar inline-anki--known-flashcard-places nil
  "Internal use only.")

;; TODO: make a version that Vertico/Helm can make interactive
;;;###autoload
(defun inline-anki-occur ()
  "Use `occur' to show all flashcards in the buffer."
  (interactive)
  (occur (rx (or (regexp inline-anki-rx:struct)
                 (regexp inline-anki-rx:struct:new)
                 (regexp inline-anki-rx:eol)
                 (regexp inline-anki-rx:eol:new)
                 (regexp inline-anki-rx:item-start)
                 (regexp inline-anki-rx:item-start:new)))))

;;;###autoload
(defun inline-anki-rgrep ()
  "Find all flashcards in current directory and descendants."
  (interactive)
  (require 'pcre2el)
  ;; Override rgrep's command to add -P for PCRE
  (let ((grep-find-template "find -H <D> <X> -type f <F> -exec grep <C> -nH -P --null -e <R> \\{\\} +"))
    (rgrep (rxt-elisp-to-pcre
            (rx (or (regexp inline-anki-rx:struct)
                    (regexp inline-anki-rx:struct:new)
                    (regexp inline-anki-rx:eol)
                    (regexp inline-anki-rx:eol:new)
                    (regexp inline-anki-rx:item-start)
                    (regexp inline-anki-rx:item-start:new))))
           "*.org")))

;; This does its own regexp searches because it's used as a callback with no
;; context.  But it'd be possible to pass another argument to
;; `inline-anki--create-note' that could let it choose one of 3 different
;; callbacks.  But tiny diff to code complexity in the end.
(defun inline-anki--dangerously-write-id (id)
  "Assign ID to the unlabeled flashcard at point."
  (unless id
    (error "Note creation failed for unknown reason (no ID returned)"))
  ;; Point is already on the correct line, at least
  (goto-char (line-beginning-position))
  (cond
   ;; Replace "@anki" with ID
   ((search-forward "@anki" (line-end-position) t)
    (delete-char -4)
    (insert "^{" (number-to-string id) "}"))

   ;; Replace "^{anki}" with ID
   ((re-search-forward (rx "^{" (group "anki") "}" (*? space) eol)
                       (line-end-position)
                       t)
    (replace-match (number-to-string id) nil nil nil 1))

   ;; Insert ID after "#+begin_flashcard"
   ((re-search-forward (rx (*? space) "#+begin_flashcard") (line-end-position) t)
    (insert " " (number-to-string id)))

   (t
    (error "No inline-anki magic string found"))))

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

(defun inline-anki-convert-implicit-clozes (text)
  "Return TEXT with emphasis replaced by Anki {{c::}} syntax."
  (require 'org)
  (cl-assert (member inline-anki-emphasis-type (map-keys org-emphasis-alist)))
  (with-temp-buffer
    (insert " ") ;; workaround bug where the regexp misses emph @ BoL
    (insert (substring-no-properties text))
    (insert " ")
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward org-emph-re nil t)
        (when (equal (match-string 3) inline-anki-emphasis-type)
          (let ((truth (match-string 4)))
            (replace-match (concat "{{c"
                                   (number-to-string (cl-incf n))
                                   "::"
                                   truth
                                   "::"
                                   (inline-anki-dots-for-letters text)
                                   "}}")
                           nil nil nil 2))))
      (if (= n 0)
          nil ;; Nil signals that no clozes found
        (string-trim (buffer-string))))))

(cl-defun inline-anki-push (&key field-beg field-end note-id)
  "Push a flashcard to Anki, identified by NOTE-ID.
Use the buffer substring delimited by FIELD-BEG and FIELD-END.

If a flashcard doesn't exist (indicated by passing a NOTE-ID
value of -1), create it."
  (let ((this-line (line-number-at-pos)))
    (if (member this-line inline-anki--known-flashcard-places)
        (error "Two magic strings on same line: %d" this-line)
      (push this-line inline-anki--known-flashcard-places)))

  (if-let* ((text (buffer-substring-no-properties field-beg field-end))
            (clozed (inline-anki-convert-implicit-clozes text)))
      (prog1 t
        (funcall
         (if (= -1 note-id)
             #'inline-anki--create-note
           #'inline-anki--update-note)
         (list (cons 'deck inline-anki-deck)
               (cons 'note-type inline-anki-cloze-note-type)
               (cons 'note-id note-id)
               (cons 'tags (cons (format-time-string "from-emacs-%F")
                                 (when inline-anki-use-tags
                                   (mapcar #'substring-no-properties
                                           (org-get-tags)))))
               ;; Drop text into the note's first field
               (cons 'fields (list (cons (car inline-anki-cloze-note-fields)
                                         clozed))))))
    (message "No implicit clozes found, skipping:  %s" text)
    nil))

(defun inline-anki-push-notes-in-buffer-1 ()
  "Push notes in buffer, and return the sum of pushes done."
  ;; NOTE: Scan for new flashcards last, otherwise you waste compute
  ;; cycles because you submit the new ones twice
  (save-mark-and-excursion
    (+
     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:item-start nil t)
              count (inline-anki-push
                     :field-beg (point)
                     :field-end (line-end-position)
                     :note-id (string-to-number (match-string 1))))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:item-start:new nil t)
              count (inline-anki-push
                     :field-beg (point)
                     :field-end (line-end-position)
                     :note-id -1))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:eol nil t)
              count (inline-anki-push
                     :field-beg (save-excursion
                                  (save-match-data
                                    (goto-char (line-beginning-position))
                                    (re-search-forward (rx bol (* space))
                                                       (line-end-position) t)
                                    (if (looking-at inline-anki-rx:list-bullet)
                                        (match-end 0)
                                      (point))))
                     :field-end (match-beginning 0)
                     :note-id (string-to-number (match-string 1))))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:eol:new nil t)
              count (inline-anki-push
                     :field-beg (save-excursion
                                  (save-match-data
                                    (goto-char (line-beginning-position))
                                    (re-search-forward (rx bol (* space))
                                                       (line-end-position) t)
                                    (if (looking-at inline-anki-rx:list-bullet)
                                        (match-end 0)
                                      (point))))
                     :field-end (match-beginning 0)
                     :note-id -1))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:struct nil t)
              count (inline-anki-push
                     :field-beg (1+ (line-end-position))
                     :field-end (save-excursion
                                  (save-match-data
                                    (search-forward "\n#+end_flashcard")
                                    (1- (line-beginning-position))))
                     :note-id (string-to-number (match-string 1))))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:struct:new nil t)
              count (inline-anki-push
                     :field-beg (1+ (line-end-position))
                     :field-end (save-excursion
                                  (save-match-data
                                    (search-forward "\n#+end_flashcard")
                                    (1- (line-beginning-position))))
                     :note-id -1)))))

;;;###autoload
(defun inline-anki-push-notes-in-buffer (&optional called-interactively)
  "Push all flashcards in the buffer to Anki.
Argument CALLED-INTERACTIVELY is automatically set; there is no
need to pass it."
  (interactive "p")
  (and called-interactively
       (string-empty-p (shell-command-to-string "ps -e | grep anki"))
       (message "Anki doesn't seem to be running"))
  (setq inline-anki--known-flashcard-places nil)
  (and inline-anki-use-tags
       (not (derived-mode-p 'org-mode))
       (cl-letf ((org-mode-hook nil))
         (org-mode)))
  (let ((pushed (inline-anki-push-notes-in-buffer-1)))
    (if called-interactively
        (message "Pushed %d notes!" pushed)
      pushed)))

;;;###autoload
(defun inline-anki-push-notes-in-directory ()
  "Push notes from every file under the current directory tree."
  (interactive)
  (require 'asyncloop)
  (if (string-empty-p (shell-command-to-string "ps -e | grep anki"))
      (message "Anki doesn't seem to be running")
    (asyncloop-run-function-queue
      (list
       (lambda (_)
         ;; TODO: exclude any .ignore and .gitignore-identified files
         (setq inline-anki--file-list
               (directory-files-recursively default-directory "\\.org$" nil t))
         (format "Will push from %d files in %s"
                 (length inline-anki--file-list)
                 default-directory))

       (defun inline-anki--scan-next-file (loop)
         (let* ((path (pop inline-anki--file-list))
                (visiting (find-buffer-visiting path)))
           (if visiting
               (with-current-buffer visiting
                 (if (buffer-modified-p)
                     (message
                      (asyncloop-log loop
                        "Unsaved changes in file, skipping %s" visiting))
                   (inline-anki-push-notes-in-buffer)))
             ;; Find file normally (not literally), but skip org-mode.
             (cl-letf (((symbol-function #'org-mode) #'fundamental-mode))
               (find-file path))
             (when (= 0 (inline-anki-push-notes-in-buffer))
               (cl-assert (not (buffer-modified-p)))
               (kill-buffer)))
           ;; Eat the file list, one item at a time
           (when inline-anki--file-list
             (push #'inline-anki--scan-next-file (asyncloop-remainder loop)))
           ;; Return useful debug string
           (format "%d files to go; was in %s"
                   (length inline-anki--file-list) path))))

      :debug-buffer-name "*inline-anki sync progress*")
    (display-buffer "*inline-anki sync progress*")))

(provide 'inline-anki)

;;; inline-anki.el ends here
