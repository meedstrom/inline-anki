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

(defcustom inline-anki-emphasis-type '(bold)
  "Which emphasis type to parse as implicit clozes.
Set this to '(bold), '(italic), or '(underline)."
  :type 'sexp)

(defvar inline-anki-directory
  (if (bound-and-true-p org-roam-directory)
      org-roam-directory
    org-directory))

(defvar inline-anki--file-list nil)

;;;###autoload
(defun inline-anki-bulk-push ()
  "Push notes from every file in `inline-anki-directory'."
  (interactive)
  (require 'asyncloop-debug)
  (asyncloop-run
    (list
     (lambda (_loop)
       (setq inline-anki--file-list
             (directory-files inline-anki-directory t "\\.org$")))
     (defun inline-anki--scan-next-file (loop)
       (let* ((path (pop inline-anki--file-list))
              (visiting (find-buffer-visiting path)))
         ;; (asyncloop-log loop "Scanning for flashcards in: %s" path)
         (if visiting
             (with-current-buffer visiting
               (if (buffer-modified-p)
                   (asyncloop-log loop
                     "Unsaved changes in file, skipping %s" visiting)
                 (inline-anki-push-notes)))
           (find-file-literally path) ;; whooo fast
           (when (= 0 (inline-anki-push-notes))
             (cl-assert (not (buffer-modified-p)))
             (kill-buffer)))
         ;; Eat the file list, one item at a time
         (when inline-anki--file-list
           (push #'inline-anki--scan-next-file (asyncloop-remainder loop)))
         (format "%d files to go; was in %s"
                 (length inline-anki--file-list) path))))
    :debug-buffer-name "*inline-anki bulk worker*")
  (display-buffer "*inline-anki bulk worker*"))

;; this needs to do regexp searches by itself because it's used as a callback
;; and given no context
(defun inline-anki--dangerously-write-id (id)
  "Assign the label ID to the unlabeled note at point."
  (unless id
    (error "Note creation failed for unknown reason"))
  (goto-char (line-beginning-position))
  (cond
   ((search-forward "@anki" (line-end-position) t)
    (delete-char -4)
    (insert "^{" (number-to-string id) "}"))

   ((re-search-forward (rx "^{" (group "anki") "}" (*? space) eol)
                       (line-end-position)
                       t)
    (replace-match (number-to-string id) nil nil nil 1))

   ((re-search-forward (rx (*? space) ":anki:") (line-end-position) t)
    (forward-char -1)
    (insert "-" (number-to-string id)))

   (t
    (error "No inline-anki magic string found"))))


;; TODO: send filename, breadcrumbs, outline path, or all of them as extra
;; fields.  and one day... we might be able to linkify the filepath in anki and
;; have anki open with emacsclient when you click the link
;;
;; for me, i could even add a web link to the source file as represented on my
;; website, so i can look (but not edit) on the phone

(cl-defun inline-anki-push (&key text-beg text-end note-id)
  (let ((this-line (line-number-at-pos)))
    (if (member this-line inline-anki-known-flashcard-places)
        (error "Two magic strings on same line: %d" this-line)
      (push this-line inline-anki-known-flashcard-places)))

  (if-let* ((text (buffer-substring text-beg text-end))
            (clozed (inline-anki-convert-implicit-clozes text)))
      (prog1 t
        (funcall
         (if (= -1 note-id)
             #'inline-anki--create-note
           #'inline-anki--update-note)
         (list (cons 'deck inline-anki-deck)
               (cons 'note-type inline-anki-note-type)
               (cons 'note-id note-id)
               (cons 'tags (cons (format-time-string "from-emacs-%F")
                                 (mapcar #'substring-no-properties (org-get-tags))))
               ;; Drop text into the note's first field
               (cons 'fields (list (cons (car inline-anki-note-fields) clozed))))))
    (message "No implicit clozes found, skipping:  %s" text)
    nil))

(defvar inline-anki-known-flashcard-places nil)

;; TODO: check upfront if implicit clozes are absent?
;; TODO: detect if a line is a comment, and ignore
;;;###autoload
(defun inline-anki-push-notes ()
  (interactive)
  ;; (unless (derived-mode-p 'org-mode)
    ;; (error "Not org-mode: %s" buffer-file-name))
  (unless org-fontify-emphasized-text
    (error "To use inline-anki, restart Emacs with `org-fontify-emphasized-text' t"))
  (let* ((list-bullet (rx (or (any "-+*") (seq (*? digit) (any ").") " "))))
         (item-start&new (rx bol (*? space) (regexp list-bullet) (*? space) "@anki "))
         (item-start&has-id (rx bol (*? space) (regexp list-bullet) (*? space) "@^{" (group (= 13 digit)) "}"))
         (eol&new (rx (or "@anki" "^{anki}") (*? space) eol))
         (eol&has-id (rx (? "@") "^{" (group (= 13 digit)) "}" (*? space) eol))
         (drawer&new (rx bol ":anki:"))
         (drawer&has-id (rx bol ":anki-" (group (= 13 digit)) ":"))
         (list-or-paragraph-start (rx bol (*? space) (? (regexp list-bullet))))
         (ctr 0))
    (setq inline-anki-known-flashcard-places nil)

    ;; NOTE: scan for the has-id flashcards first, otherwise you waste compute
    ;; cycles because the new ones get an id which would then be picked up again

    (goto-char (point-min))
    (while (re-search-forward item-start&has-id nil t)
      (when (inline-anki-push
           :text-beg (point)
           :text-end (line-end-position)
           :note-id (string-to-number (match-string 1)))
        (cl-incf ctr)))

    (goto-char (point-min))
    (while (re-search-forward item-start&new nil t)
      (when (inline-anki-push
             :text-beg (point)
             :text-end (line-end-position)
             :note-id -1)
        (cl-incf ctr)))

    (goto-char (point-min))
    (while (re-search-forward eol&has-id nil t)
      (when (inline-anki-push
             :text-beg (save-excursion
                         (save-match-data
                           (goto-char (line-beginning-position))
                           (re-search-forward list-or-paragraph-start
                                              (line-end-position))))
             :text-end (match-beginning 0)
             :note-id (string-to-number (match-string 1)))
        (cl-incf ctr)))

    (goto-char (point-min))
    (while (re-search-forward eol&new nil t)
      (when (inline-anki-push
             :text-beg (save-excursion
                         (save-match-data
                           (goto-char (line-beginning-position))
                           (re-search-forward list-or-paragraph-start
                                              (line-end-position))))
             :text-end (match-beginning 0)
             :note-id -1)
        (cl-incf ctr)))

    (goto-char (point-min))
    (while (re-search-forward drawer&has-id nil t)
      (when (inline-anki-push
             :text-beg (1+ (line-end-position))
             :text-end (save-excursion
                         (save-match-data
                           (search-forward "\n:end:")
                           (1- (line-beginning-position))))
             :note-id (string-to-number (match-string 1)))
        (cl-incf ctr)))

    (goto-char (point-min))
    (while (re-search-forward drawer&new nil t)
      (when (inline-anki-push
             :text-beg (1+ (line-end-position))
             :text-end (save-excursion
                         (search-forward "\n:end:")
                         (1- (line-beginning-position)))
             :note-id -1)
        (cl-incf ctr)))

    (setq inline-anki-known-flashcard-places nil)
    ctr))

;; FIXME
(defun inline-anki-convert-implicit-clozes (text)
  (with-temp-buffer
    (org-mode)
    (insert text)
    (goto-char (point-min))
    (org-do-emphasis-faces (point-max))
    (goto-char (point-min))
    (let ((n 0))
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
      (if (= n 0)
          nil ;; Nil signals that no clozes found
        (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'inline-anki)
