;; inline-anki.el -- One-liner flashcards -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Martin Edström <meedstrom91@gmail.com>
;;
;; Description: Oneliner flashcards
;; Author: Martin Edström
;; Version: 0.1.0-pre
;; Package-Requires: ((emacs "28"))
;; URL:

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

;; External requirements:
;; - curl
;; - Anki with AnkiConnect add-on

;; TODO: give a way to skip `org-get-tags' and see if that makes a speed
;; difference

;; TODO: refactor to do as few regexp searches as possible -- not for speed,
;;       but for preventing bugs

;;; Code:

(defgroup inline-anki nil
  "Customizations for inline-anki."
  :group 'org)

(require 'inline-anki-anki-editor-fork)
(require 'asyncloop)

(defcustom inline-anki-deck "Default"
  "Name of deck to upload to."
  :type 'string)

(defcustom inline-anki-note-type "Cloze"
  "Name of your note type."
  :type 'string)

(defcustom inline-anki-note-fields '("Text" "Extra")
  "Names of fields in your note type."
  :type '(repeat string))

(defcustom inline-anki-default-tags nil
  "Tags to always include."
  :type '(repeat string))

(defcustom inline-anki-emphasis-type '(bold)
  "Which emphasis type to parse as implicit clozes.
Set this to '(bold), '(italic), or '(underline)."
  :type 'sexp)

(defun inline-anki-push-notes ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not org-mode: %s" buffer-file-name))
  ;; (unless org-fontify-emphasized-text
  ;; (error "Inline-anki relies on `org-fontify-emphasized-text'"))
  (setq truncate-lines t)
  (inline-anki-map-note-things
   (lambda ()
     (message "Scanning buffer for Anki notes: \"%s\"..." (buffer-name))
     (condition-case-unless-debug err
         (if-let ((note (inline-anki-note-at-point)))
             (inline-anki--push-note note)
           (error "-map-note-things found magic string, but note-at-point found no clozes"))
       (error (message "Flashcard export at point %d failed: %s"
                       (point)
                       (error-message-string err)))))))

(defvar inline-anki-directory
  (if (bound-and-true-p org-roam-directory)
      org-roam-directory
    org-directory))

(defvar inline-anki--file-list nil)

;; WIP: Experimental.
;;
;; Theory: Maybe it works better if I use find-file instead of
;; with-current-buffer, and force redisplay before proceeding.  If that makes a
;; difference, we know what the problem is.
;;
;; Theory: it's those org-element cache bugs behind it
;;
;; Theory: it's the fact i'm using with-temp-buffer that prevents normal
;; fontification (but apparently less often a problem when i just interactively
;; call push-notes)
(defun inline-anki-bulk-push ()
  "Run `inline-anki-push-notes' from every file in `inline-anki-directory'."
  (interactive)
  (require 'asyncloop-debug)
  (let ((modes-to-reenable nil))
    (when (bound-and-true-p git-auto-commit-mode)
      (git-auto-commit-mode 0)
      (push 'git-auto-commit-mode modes-to-reenable))
    (when (bound-and-true-p my-auto-commit-mode)
      (my-auto-commit-mode 0)
      (push 'my-auto-commit-mode modes-to-reenable))
    (asyncloop-run
      (list
       (lambda (_loop)
         (setq inline-anki--file-list
               (directory-files inline-anki-directory t "\\.org$")))
       (defun inline-anki--scan-next-file (loop)
         (let* ((path (pop inline-anki--file-list))
                (visiting (find-buffer-visiting path)))
           ;; (asyncloop-log loop "Scanning for flashcards in: %s" path)
           (with-current-buffer (or visiting (find-file-noselect path))
             (if (buffer-modified-p)
                 (asyncloop-log loop
                   "Unsaved changes in file, skipping it")
               (inline-anki-push-notes)
               ;; REVIEW: `inline-anki--anki-connect-invoke-queue' works asynchronously?
               ;;         maybe can't save at this point
               (save-buffer))
             (when inline-anki--file-list
               (push #'inline-anki--scan-next-file (asyncloop-remainder loop)))
             ;; Wasn't visiting before, so kill buffer
             (unless (or visiting (buffer-modified-p))
               (kill-buffer (current-buffer))))
           (format "%d files to go; was in %s" (length inline-anki--file-list) path)))
       ;; `(lambda (_loop)
       ;;   (dolist (mode ,modes-to-reenable)
       ;;     (funcall mode)))
       )
      :debug-buffer-name "*inline-anki bulk worker*"))
  (delete-other-windows)
  (switch-to-buffer "*inline-anki bulk worker*")
  (split-window)
  (switch-to-buffer "*Messages*")
  (split-window))


;; TODO: send filename, breadcrumbs, outline path, or all of them as extra fields.
;; and one day, we might be able to linkify the filepath and open with emacsclient
(defun inline-anki-note-at-point ()
  "Construct an alist representing a note at point."
  (let* (
         ;; An @anki at start of list item? Exclude it from note text.
         (begin (save-excursion
                  ;; Org's magic puts us at at start of the list item after the
                  ;; bullet point
                  (goto-char (org-element-property
                              :contents-begin (org-element-at-point)))
                  (unless (search-forward "@anki" (+ 6 (point)) t)
                    (re-search-forward (rx "@^{" (= 13 digit) "}")
                                       (+ 18 (point))
                                       t))
                  (point)))

         ;; An @anki at end of line?  Exclude it from note text.
         (end (save-excursion
                ;; Alas, (org-element-property :contents-end) jumps past all
                ;; sub-items of a list-item.  So we just line-orient and hope
                ;; that the user doesn't hard-wrap.
                (goto-char (line-beginning-position))
                (save-match-data
                  (if (or (re-search-forward (rx "@anki" (*? space) eol)
                                             (line-end-position)
                                             t)
                          (re-search-forward (rx (?? "@") "^{" (*? alnum) "}" (*? space) eol)
                                             (line-end-position)
                                             t))
                      (match-beginning 0)
                    (line-end-position))))))

    (list (cons 'deck inline-anki-deck)
          (cons 'note-id (inline-anki-thing-id))
          (cons 'note-type inline-anki-note-type)
          (cons 'tags (append inline-anki-default-tags
                              (mapcar #'substring-no-properties (org-get-tags))
                              (list (format-time-string "from-emacs-%F"))))
          ;; Drop text into the first field, however that's labeled
          (cons 'fields (list (cons (car inline-anki-note-fields)
                                    (inline-anki-convert-implicit-clozes
                                     (buffer-substring begin end))))))))

(defun inline-anki--set-note-id (id)
  "Assign the id ID to the unlabeled note at point."
  (unless id
    (error "Note creation failed for unknown reason"))
  (goto-char (line-beginning-position))
  (let ((eol-flag (rx "^{" (group "anki") "}" (*? space) eol)))
    (cond
     ((search-forward "@anki" (line-end-position) t)
      (delete-char -4)
      (insert "^{" (number-to-string id) "}"))

     ((re-search-forward eol-flag (line-end-position) t)
      (replace-match (number-to-string id) t t nil 1))

     (t
      (error "Inline-anki magic string not found on line")))))

(defun inline-anki-thing-id ()
  (save-excursion
    (let ((beg (line-beginning-position))
          (bound (progn
                   (if (org-at-item-p)
                       (org-end-of-item)
                     (org-forward-paragraph))
                   (point))))
      (goto-char beg)
      (if (re-search-forward (rx "^{" (group (= 13 digit)) "}") bound t)
          (string-to-number (match-string 1))
        -1))))

;; TOOD: detect if a line is a comment, and ignore
;; TODO: detect if there is more than one magic string on the same line (run all
;; searches upfront, record the line numbers, then check for duplicates among
;; the recorded line numbers)
(defun inline-anki-map-note-things (func)
  "Run FUNC with point on each flashcard in the buffer.
Note: FUNC should not move point backwards, or you can get an
infinite loop."
  (let* ((ctr 0)
         (list-bullet (rx (or (any "-+*") (seq (*? digit) (any ".)")))))
         (card@item-start&has-id (rx bol (*? space) (regexp list-bullet) (+? space) "@^{" (= 13 digit) "}"))
         (card@item-start&new (rx bol (*? space) (regexp list-bullet) (+? space) "@anki"))
         (card@eol&has-id (rx (? "@") "^{" (= 13 digit) "}" (*? space) eol))
         (card@eol&new (rx (or "@anki" "^{anki}") (*? space) eol)))
    (dolist (regexp (list card@item-start&has-id
                          card@item-start&new
                          card@eol&has-id
                          card@eol&new))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (forward-char -1)
        (cl-incf ctr)
        (funcall func)))
    ctr))

(defun inline-anki-convert-implicit-clozes (text)
  (with-temp-buffer
    ;; (org-mode)
    (insert text)
    (goto-char (point-min))
    (let ((org-fontify-emphasized-text t)
          (n 0))
      (org-restart-font-lock)
      (org-do-emphasis-faces (point-max))
      (goto-char (point-min))
      (while (setq prop (text-property-search-forward
                         'face inline-anki-emphasis-type t))
        (when org-hide-emphasis-markers
          (cl-decf (prop-match-beginning prop))
          (cl-incf (prop-match-end prop)))
        (let ((num (number-to-string (cl-incf n))))
          (goto-char (prop-match-beginning prop))
          (delete-char 1)
          (insert "{{c" num "::")
          ;; 4 because {{c:: adds 5 and we deleted 1 char
          (goto-char (+ (prop-match-end prop) 4 (length num)))
          (delete-char -1)
          (insert "}}")))
      (when (= n 0)
        (error "No clozes in note: %s" text)))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'inline-anki)
