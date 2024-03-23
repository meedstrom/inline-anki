;;; inline-anki.el --- Embed implicit flashcards in flowing text -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Martin Edström <meedstrom91@gmail.com>

;; Description: Embed implicit flashcards in flowing text
;; Author: Martin Edström
;; Version: 0.3.8-pre
;; Created: 2023-09-19
;; Package-Requires: ((emacs "28") (asyncloop "0.5") (pcre2el "1.12") (request "0.3.3") (dash "2.19.1"))
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
;; - A Unix-like system (tested on GNU+Linux only)

;;; Code:

(require 'asyncloop)

(defgroup inline-anki nil
  "Customizations for inline-anki."
  :group 'org)

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

(defcustom inline-anki-send-tags nil
  "List of tags to include when sending flashcards to Anki.
This doesn't mean extra tags injected into every flashcard, but
rather that if the subtree has a tag in this list, then to
send the tag.  Special value t means include all tags.

Defaults to nil because users may be distraught to find a load of
new tags in their Anki database.  Also, the nil value lets the
command `inline-anki-push-notes-in-directory' work faster, since it won't
have to turn on Org-mode for each file visited.

A list of strings means include the tag only if it's in this
list.  Or if the first element is the symbol `not', then include
the tag only if it's NOT in this list.

A reasonable value is '\(not \"noexport\" \"archive\"\).

Case-insensitive.

If you want to include the local subtree tags only and exclude
global tags, that cannot be expressed here; you need
`org-use-tag-inheritance' at nil."
  :type '(choice
          (const :tag "All" t)
          (const :tag "None" nil)
          (repeat sexp)))

(defcustom inline-anki-ignore-file-regexps
  '("/logseq/version-files/"
    "/logseq/bak/"
    "/.git/")
  "List of regexps that bar a file-path from being visited.
Used by the command `inline-anki-push-notes-in-directory'.  Note
that the command only considers file-paths ending in .org, so
backup and auto-save files ending in ~ or # are already barred by
that fact."
  :type '(repeat string))

(defcustom inline-anki-fields
  '(("Source" . inline-anki-field:filename-link)
    ("Text" . t))
  "Alist specifying note fields and how to populate them.
The cdrs may be either t, a string or a function.  The symbol t
is replaced with the full HTML of the note, so you probably
only want t for one of the fields.

If the cdr is a function, it's evaluated with the appropriate
buffer set as current, with point on the first line of the
flashcard expression.  (In the case of a #+begin_flashcard
template, point is on that line.)

You have to create a field with the same name in Anki's \"Manage
note types\" before it will work.  Fields unknown to Anki will
not be filled in.  Conversely, it's OK if Anki defines more
fields than this variable has, they will not be edited."
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

(defconst inline-anki-rx:new-struct-new
  (rx bol (*? space) "#+begin_anki " (*? space) (+? word) (*? space) eol))

(defconst inline-anki-rx:new-struct
  (rx bol (*? space) (?? "# ") (*? space) "#+begin_anki " (*? space)
      (= 5 word) (+? space) (group (= 13 digit)) (or eol (not digit))))

;; Reason I didn't go with anki or cloze is there are already hotkeys a and c
;; (defvar inline-anki-structure-name "anki")
;; (defvar inline-anki-structure-name "fc")
;; (defvar inline-anki-structure-name "cloze")
;; but now I know a good way: "begin_anki cloze", "begin_anki basic", using
;; hotkeys f c and f b, respectively.

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

(defun inline-anki-dots-logarithmic (text)
  "Return TEXT replaced by dots.
Longer TEXT means more dots, but along a log-2 algorithm so it
doesn't get crazy-long in extreme cases."
  (make-string (max 3 (* 2 (round (log (length text) 2)))) ?\.))

(defcustom inline-anki-occluder
  #'inline-anki-dots-logarithmic
  "Function that takes a string and returns an occluded string, for
use in cloze.

To get the Anki default of three dots, set this variable to nil."
  :type '(choice function
          (const :tag "Anki default of three dots" nil)))

(defconst inline-anki-rx:comment-glyph
  (rx bol (*? space) "# "))

(defun inline-anki--convert-implicit-clozes (text)
  "Return TEXT with emphasis replaced by Anki {{c::}} syntax."
  (with-temp-buffer
    (insert " ") ;; workaround bug where the regexp misses emph @ BoL
    (insert (substring-no-properties text))
    (insert " ")
    (goto-char (point-min))
    ;; comment means suspend card, but don't also blank-out the html
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
                                   (when inline-anki-occluder
                                     (concat
                                      "::"
                                      (funcall inline-anki-occluder truth)))
                                   "}}")
                           nil nil nil 2))))
      (if (= n 0)
          nil ;; Nil signals that no clozes found
        (string-trim (buffer-string))))))

;; TODO: make capable of basic flashcard
(cl-defun inline-anki--push-note (&key field-beg field-end note-id)
  "Push a flashcard to Anki, identified by NOTE-ID.
Use the buffer substring delimited by FIELD-BEG and FIELD-END.

If a flashcard doesn't exist (indicated by a NOTE-ID
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
        ;; When `inline-anki-push-notes-in-directory' calls this, the buffer is
        ;; in fundamental-mode. Switch to org-mode if we need to read tags.
        (and inline-anki-send-tags
             (not (derived-mode-p 'org-mode))
             (delay-mode-hooks
               (org-mode)))
        (funcall
         (if (= -1 note-id)
             #'inline-anki--create-note
           #'inline-anki--update-note)
         (list
          (cons 'deck inline-anki-deck)
          (cons 'note-type inline-anki-note-type)
          (cons 'note-id note-id)
          (cons 'tags
                (delq nil
                      (cons
                       (when inline-anki-extra-tag
                         (format-time-string inline-anki-extra-tag))
                       (mapcar #'substring-no-properties
                               (cond ((eq t inline-anki-send-tags)
                                      (org-get-tags))
                                     ((null inline-anki-send-tags)
                                      nil)
                                     ((eq (car inline-anki-send-tags) 'not)
                                      (cl-set-difference
                                       (org-get-tags)
                                       (cdr inline-anki-send-tags)
                                       :test #'string-equal-ignore-case))
                                     (t
                                      (cl-set-difference
                                       inline-anki-send-tags
                                       (org-get-tags)
                                       :test #'string-equal-ignore-case)))))))
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

(defcustom inline-anki-extra-tag "from-emacs-%F"
  "Tag added to every note sent to Anki.
Will be passed through `format-time-string'.  Cannot be nil."
  :type 'string)

(defun inline-anki--expand (input)
  "Return INPUT if it's a string, else funcall or eval it."
  (condition-case signal
      (cond ((stringp input)
             input)
            ((functionp input)
             (save-excursion
               (save-match-data
                 (funcall input))))
            ((null input)
             (display-warning
              'inline-anki "A cdr of `inline-anki-fields' appears to be nil")
             "")
            ((listp input)
             (eval input t))
            (t ""))
    ;; IME this is a common source of errors (and I'm the package dev!), so
    ;; help tell the user where the error's coming from.
    ((error debug)
     (error "There was likely a problem evaluating a member of `inline-anki-fields':  %s signaled %s"
            input
            signal))))

(defun inline-anki-field:filename-link ()
  "Return the buffer filename wrapped in <a href>."
  (concat "<a href=\"file://" buffer-file-name "\">" buffer-file-name "</a>"))

(defun inline-anki-check ()
  "Check that everything is ready, else return nil."
  (inline-anki-warn-renamed-options)
  (cl-assert
   (member inline-anki-emphasis-type (mapcar #'car org-emphasis-alist)))
  (if (not (string-empty-p (shell-command-to-string "ps -e | grep anki")))
      t
    (message "Anki doesn't seem to be running")
    nil))

(defun inline-anki-push-notes-in-buffer-1 ()
  "Push notes in buffer, and return the count of pushes made."
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
Argument CALLED-INTERACTIVELY sets itself."
  (interactive "p")
  (require 'org)
  (require 'inline-anki-anki-editor-fork)
  (when (or (not called-interactively)
            (inline-anki-check))
    (setq inline-anki--known-flashcard-places nil)
    (unless (file-writable-p buffer-file-name)
      (error "Can't write to path (no permissions?): %s"
             buffer-file-name))
    (let (pushed
          (already-modified (buffer-modified-p)))
      (unwind-protect
          (progn
            (advice-add 'org-html-link :around #'inline-anki--ox-html-link)
            (setq pushed (inline-anki-push-notes-in-buffer-1)))
        (advice-remove 'org-html-link #'inline-anki--ox-html-link))
      (if already-modified
          (message "Not saving buffer %s" (current-buffer))
        (save-buffer))
      (if called-interactively
          (message "Pushed %d notes!" pushed)
        pushed))))

(defun inline-anki--prep-scanner (_)
  (setq inline-anki--file-list
        (cl-loop for path in (directory-files-recursively
                              default-directory "\\.org$" nil t)
                 ;; Filter out ignores
                 unless (cl-find-if (lambda (re) (string-match-p re path))
                                    inline-anki-ignore-file-regexps)
                 collect path))
  (format "Will push from %d files in %s"
          (length inline-anki--file-list)
          default-directory))

(defun inline-anki--next (loop)
  (if (null inline-anki--file-list)
      "All done"
    (let* ((path (car inline-anki--file-list))
           (buf (or (find-buffer-visiting path)
                    ;; Skip org-mode for speed
                    (cl-letf (((symbol-function #'org-mode) #'ignore)
                              ((symbol-function #'after-find-file) #'ignore)
                              (find-file-hook nil))
                      (find-file-noselect path))))
           (pushed
            (with-current-buffer buf
              (inline-anki-push-notes-in-buffer)))
           (file (buffer-name buf))
           (modified (buffer-modified-p buf)))
      (if (= 0 pushed)
          (progn
            (cl-assert (not modified))
            (kill-buffer buf))
        (message "%sPushed %d notes in %s"
                 (if modified
                     "Not saving! "
                   ;; Switch from fundamental-mode to org-mode (probably)
                   ;; to avoid shocking user
                   ;; REVIEW: Probably not necessary
                   (with-current-buffer buf
                     (normal-mode))
                   "")
                 pushed
                 buf))
      ;; Eat the file list, one item at a time
      (pop inline-anki--file-list)
      ;; Repeat this function
      (push t (asyncloop-remainder loop))
      (format "%d files to go; %s"
              (length inline-anki--file-list)
              (if (= 0 pushed)
                  (format "no cards in   %s" file)
                (format   "pushed %d from %s" pushed file))))))

;;;###autoload
(defun inline-anki-push-notes-in-directory ()
  "Push notes from every file in current dir and all subdirs."
  (interactive)
  (require 'org)
  (when (inline-anki-check)
    (asyncloop-run
      (list #'inline-anki--prep-scanner
            #'inline-anki--next)
      :log-buffer-name "*inline-anki*")
    (unless (get-buffer-window "*inline-anki*" 'visible)
      (display-buffer "*inline-anki*"))))

;;; Handle deprecations gracefully

(defcustom inline-anki-ignore
  '("/logseq/version-files/"
    "/logseq/bak/"
    "/.git/")
  "This option has been renamed, use `inline-anki-ignore-file-regexps'."
  :type '(repeat string))

(defun inline-anki-warn-renamed-options ()
  (require 'custom)
  ;; If it's at the standard value, pretend there's no such variable
  (when (equal (custom--standard-value 'inline-anki-ignore)
               (symbol-value 'inline-anki-ignore))
    (setq inline-anki-ignore nil))
  (when-let ((use-tags (bound-and-true-p inline-anki-use-tags)))
    (setq inline-anki-send-tags use-tags)
    (setq inline-anki-use-tags nil)
    (message "User option renamed: `inline-anki-use-tags' to `inline-anki-send-tags'"))
  (when-let ((igno (bound-and-true-p inline-anki-ignore)))
    (setq inline-anki-ignore-file-regexps igno)
    (setq inline-anki-ignore nil)
    (message "User option renamed: `inline-anki-ignore' to `inline-anki-ignore-file-regexps'")))

(provide 'inline-anki)

;;; inline-anki.el ends here
