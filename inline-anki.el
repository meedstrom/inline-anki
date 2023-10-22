;;; inline-anki.el --- Embed implicit flashcards in flowing text -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2023 Martin Edström <meedstrom91@gmail.com>

;; Description: Embed implicit flashcards in flowing text
;; Author: Martin Edström
;; Version: 0.3.0-pre
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

;; Read more in the Info manual or https://github.com/meedstrom/inline-anki
;;
;; Requirements:
;; - curl
;; - Anki with AnkiConnect add-on
;; - A Unix-like system (Linux, MacOS, WSL, BSD)
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
(require 'seq)
(require 'map)

(defcustom inline-anki-deck "Default"
  "Name of deck to upload to."
  :type 'string)

(defcustom inline-anki-note-type "Cloze from Outer Space"
  "Name of your cloze note type.
Will fail silently if the note type doesn't exist, so get it
right!"
  :type 'string)

(defcustom inline-anki-emphasis-type "_"
  "The kind of emphasis you want to indicate a cloze deletion.
Whatever you choose, it MUST be found in `org-emphasis-alist' and
can only be one character long."
  :type 'string)

(defcustom inline-anki-use-tags nil
  "Whether to send Org tags to Anki.
Setting it to nil lets `inline-anki-push-notes-in-directory' work
faster if you have hundreds of files.

Defaults to nil because some users may be distraught to find a
load of new tags in their Anki database.

If you enable this but realize you you want to include the local
subtree tags only, set `org-use-tag-inheritance' to nil."
  :type 'boolean)

(defcustom inline-anki-ignore
  '("/logseq/version-files/"
    "/logseq/bak/")
  "List of substrings that bar a file-path from being visited."
  :type '(repeat string))

(defcustom inline-anki-fields
  '(("Text" . t)
    ("Source" . inline-anki-field:filename-link))
  "Alist specifying note fields and how to populate them.
The cdrs may be either t, a string or a function.  The symbol t
is replaced with the full HTML of the note, so you probably
only want t for one of the fields.

If the cdr is a function, it's evaluated with the appropriate
buffer set as current, with point on the first line of the
flashcard expression.

You have to create each field in Anki's \"Manage note types\"
before it will work.  Fields unknown to Anki will not be filled
in.  Conversely, it's OK if Anki defines more fields than this
variable has."
  :type '(alist
          :key-type string
          :value-type (choice function
                              string
                              (const :tag "The full HTML content" t)
                              sexp)))

(defconst inline-anki-rx:list-bullet
  (rx (or (any "-+*") (seq (*? digit) (any ").") " "))))

(defconst inline-anki-rx:item-start-new
  (rx bol (*? space) (regexp inline-anki-rx:list-bullet) (*? space) "@anki "))

(defconst inline-anki-rx:item-start
  (rx bol (*? space) (?? "# ") (*? space) (regexp inline-anki-rx:list-bullet) (*? space) "@^{" (group (= 13 digit)) "}"))

(defconst inline-anki-rx:eol-new
  (rx (or "@anki" "^{anki}") (*? space) eol))

(defconst inline-anki-rx:eol
  (rx (?? "@") "^{" (group (= 13 digit)) "}" (*? space) eol))

(defconst inline-anki-rx:struct-new
  (rx bol (*? space) "#+begin_flashcard" (*? space) eol))

(defconst inline-anki-rx:struct
  (rx bol (*? space) (?? "# ") (*? space) "#+begin_flashcard " (group (= 13 digit)) (or eol (not digit))))

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
                 (regexp inline-anki-rx:struct-new)
                 (regexp inline-anki-rx:eol)
                 (regexp inline-anki-rx:eol-new)
                 (regexp inline-anki-rx:item-start)
                 (regexp inline-anki-rx:item-start-new)))))

;;;###autoload
(defun inline-anki-rgrep ()
  "Find all flashcards in current directory and descendants."
  (interactive)
  (require 'pcre2el)
  ;; Override rgrep's command to add -P for PCRE
  (let ((grep-find-template "find -H <D> <X> -type f <F> -exec grep <C> -nH -P --null -e <R> \\{\\} +"))
    (rgrep (rxt-elisp-to-pcre
            (rx (or (regexp inline-anki-rx:struct)
                    (regexp inline-anki-rx:struct-new)
                    (regexp inline-anki-rx:eol)
                    (regexp inline-anki-rx:eol-new)
                    (regexp inline-anki-rx:item-start)
                    (regexp inline-anki-rx:item-start-new))))
           "*.org")))

;; This does its own regexp searches because it's used as a callback with no
;; context.  But it'd be possible to pass another argument to
;; `inline-anki--create-note' that could let it choose one of 3 different
;; callbacks.  Anyway, tiny diff to code complexity in the end.
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

(defun inline-anki--dots-for-letters (text)
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

(defconst inline-anki-rx:comment-glyph
  (rx bol (*? space) "# "))

(defun inline-anki--convert-implicit-clozes (text)
  "Return TEXT with emphasis replaced by Anki {{c::}} syntax."
  (with-temp-buffer
    (insert " ") ;; workaround bug where the regexp misses emph @ BoL
    (insert (substring-no-properties text))
    (insert " ")
    (goto-char (point-min))
    ;; comment means suspend, but don't also blank-out the html
    (while (re-search-forward inline-anki-rx:comment-glyph nil t)
      (delete-char -2))
    (let ((n 0))
      (goto-char (point-min))
      (while (re-search-forward org-emph-re nil t)
        (when (equal (match-string 3) inline-anki-emphasis-type)
          (let ((truth (match-string 4)))
            (replace-match (concat "{{c"
                                   (number-to-string (cl-incf n))
                                   "::"
                                   truth
                                   "::"
                                   (inline-anki--dots-for-letters truth)
                                   "}}")
                           nil nil nil 2))))
      (if (= n 0)
          nil ;; Nil signals that no clozes found
        (string-trim (buffer-string))))))

(cl-defun inline-anki--push-note (&key field-beg field-end note-id)
  "Push a flashcard to Anki, identified by NOTE-ID.
Use the buffer substring delimited by FIELD-BEG and FIELD-END.

If a flashcard doesn't exist (indicated by passing a NOTE-ID
value of -1), create it."
  (let ((this-line (line-number-at-pos)))
    (if (member this-line inline-anki--known-flashcard-places)
        (error "Two magic strings on same line: %d" this-line)
      (push this-line inline-anki--known-flashcard-places)))
  (if-let* ((text (buffer-substring field-beg field-end))
            (clozed (inline-anki--convert-implicit-clozes text))
            (html (org-export-string-as clozed
                                        inline-anki--ox-anki-html-backend
                                        t
                                        '(:with-toc nil))))
      (prog1 t
        (funcall
         (if (= -1 note-id)
             #'inline-anki--create-note
           #'inline-anki--update-note)
         (list
          (cons 'deck inline-anki-deck)
          (cons 'note-type inline-anki-note-type)
          (cons 'note-id note-id)
          (cons 'tags (cons (format-time-string "from-emacs-%F")
                            (when inline-anki-use-tags
                              (mapcar #'substring-no-properties
                                      (org-get-tags)))))
          (cons 'fields (cl-loop
                         for (field . value) in inline-anki-fields
                         as expanded = (inline-anki--expand value)
                         if (eq t value)
                         collect (cons field html)
                         else unless (null expanded)
                         collect (cons field expanded)))
          (cons 'suspend? (save-excursion
                            (goto-char (line-beginning-position))
                            (looking-at-p "[[:space:]]*?# "))))))
    (message "No implicit clozes found, skipping:  %s" text)
    nil))

(defun inline-anki--expand (input)
  "Return INPUT if it's a string, else funcall or eval it."
  (condition-case signal
      (if (stringp input)
          input
        (if (functionp input)
            (save-excursion
              (save-match-data
                (funcall input)))
          (if (null input)
              ;; TODO: Maybe warn that input was nil
              ""
            (if (listp input)
                (eval input t)
              ""))))
    ;; IME this is a common source of errors (and I'm the package dev!), so help
    ;; tell the user where the error's coming from.
    ((error debug)
     (error "There was likely a problem evaluating a member of `inline-anki-fields':  %s signaled %s"
            input
            signal))))

(defun inline-anki-field:filename-link ()
  "Return the buffer filename wrapped in <a href>."
  (concat "<a href=\"file://" buffer-file-name "\">" buffer-file-name "</a>"))

(defun inline-anki-push-notes-in-buffer-1 ()
  "Push notes in buffer, and return the count of pushes made."
  (require 'org)
  (cl-assert (member inline-anki-emphasis-type (map-keys org-emphasis-alist)))
  ;; NOTE: Scan for new flashcards last, otherwise you waste compute
  ;; cycles because you submit the new ones twice
  (save-mark-and-excursion
    (+
     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:item-start nil t)
              count (inline-anki--push-note
                     :field-beg (point)
                     :field-end (line-end-position)
                     :note-id (string-to-number (match-string 1))))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:item-start-new nil t)
              count (inline-anki--push-note
                     :field-beg (point)
                     :field-end (line-end-position)
                     :note-id -1))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:eol nil t)
              count (inline-anki--push-note
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
              while (re-search-forward inline-anki-rx:eol-new nil t)
              count (inline-anki--push-note
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
              count (inline-anki--push-note
                     :field-beg (1+ (line-end-position))
                     :field-end (save-excursion
                                  (save-match-data
                                    (search-forward "#+end_flashcard")
                                    (1- (line-beginning-position))))
                     :note-id (string-to-number (match-string 1))))

     (cl-loop initially (goto-char (point-min))
              while (re-search-forward inline-anki-rx:struct-new nil t)
              count (inline-anki--push-note
                     :field-beg (1+ (line-end-position))
                     :field-end (save-excursion
                                  (save-match-data
                                    (search-forward "#+end_flashcard")
                                    (1- (line-beginning-position))))
                     :note-id -1)))))

;;;###autoload
(defun inline-anki-push-notes-in-buffer (&optional called-interactively)
  "Push all flashcards in the buffer to Anki.
Argument CALLED-INTERACTIVELY is automatically set; there is no
need to pass it."
  (interactive "p")
  (setq inline-anki--known-flashcard-places nil)
  (and called-interactively
       (string-empty-p (shell-command-to-string "ps -e | grep anki"))
       (message "Anki doesn't seem to be running"))
  (and inline-anki-use-tags
       (not (derived-mode-p 'org-mode))
       (cl-letf ((org-mode-hook nil))
         (org-mode)))
  (unless (file-writable-p buffer-file-name)
    (user-error "No write permissions, cancelling: %s" buffer-file-name))
  (let (pushed)
    (unwind-protect
        (progn
          (advice-add 'org-html-link :around #'inline-anki--ox-html-link)
          (setq pushed (inline-anki-push-notes-in-buffer-1)))
      (advice-remove 'org-html-link #'inline-anki--ox-html-link))
    (if called-interactively
        (message "Pushed %d notes!" pushed)
      pushed)))

(defun inline-anki--list-files-except-gitignored ()
  "List all Org files under current dir, except gitignored files."
  (cl-loop for file in
           (string-split (shell-command-to-string
                          "git ls-files -oc --exclude-standard")
                         "\n")
           when (string-suffix-p ".org" file)
           collect (expand-file-name file)))

(defun inline-anki--git-repo-p (directory)
  "Non-nil if DIRECTORY is a Git repository."
  (and (file-directory-p directory)
       (or (file-regular-p (expand-file-name ".git" directory))
           (file-directory-p (expand-file-name ".git" directory)))))

;;;###autoload
(defun inline-anki-push-notes-in-directory ()
  "Push notes from every file in current dir and all subdirs."
  (interactive)
  (require 'asyncloop)
  (if (string-empty-p (shell-command-to-string "ps -e | grep anki"))
      (message "Anki doesn't seem to be running")
    (asyncloop-run-function-queue
      (list
       (defun inline-anki--prep-scanner (_)
         (setq inline-anki--file-list
               ;; Ideally we'd obey an `inline-anki-must-heed-gitignore' that
               ;; causes failure when git isn't installed, and check if subdirs
               ;; are git repos even if the top dir isn't.  Buuut
               ;; `inline-anki-ignore' was a simpler solution.
               (if (and (inline-anki--git-repo-p ".")
                        (executable-find "git"))
                   (inline-anki--list-files-except-gitignored)
                 (directory-files-recursively
                  default-directory "\\.org$" nil t)))
         ;; Filter out ignores
         (setq inline-anki--file-list
               (cl-loop
                for path in inline-anki--file-list
                unless (seq-find `(lambda (ign) (string-search ign ,path))
                                 inline-anki-ignore)
                collect path))
         (format "Will push from %d files in %s"
                 (length inline-anki--file-list)
                 default-directory))

       (defun inline-anki--next (loop)
         (let* ((path (pop inline-anki--file-list))
                (visiting (find-buffer-visiting path))
                (buf nil)
                (file nil)
                (pushed
                 (if visiting
                     (with-current-buffer visiting
                       (if (buffer-modified-p)
                           (message
                            (asyncloop-log loop
                              "Unsaved changes in file, skipping %s" visiting))
                         (setq buf visiting)
                         (inline-anki-push-notes-in-buffer)))
                   ;; Skip org-mode for speed.  Also tried `find-file-literally'
                   ;; in the past; bad idea.
                   (with-current-buffer
                       (cl-letf (((symbol-function #'org-mode) #'ignore))
                         (find-file-noselect path))
                     (setq buf (current-buffer))
                     (inline-anki-push-notes-in-buffer)))))
           (setq file (buffer-name buf))
           (if (= 0 pushed)
               (progn
                 (cl-assert (not (buffer-modified-p buf)))
                 (kill-buffer buf))
             (message
              "Pushed %s notes in %s" pushed buf))
           ;; Eat the file list, one item at a time
           (when inline-anki--file-list
             (push #'inline-anki--next (asyncloop-remainder loop)))
           ;; Return useful debug string
           (format "%d files to go; pushed %d from %s"
                   (length inline-anki--file-list) pushed file))))

      :debug-buffer-name "*inline-anki sync*")

    (unless (get-buffer-window "*inline-anki sync*" 'visible)
      (display-buffer "*inline-anki sync*"))))

(provide 'inline-anki)

;;; inline-anki.el ends here
