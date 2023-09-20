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

;; TODO: make a command to auto-push an entire directory

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
  (unless (eq major-mode 'org-mode)
    (error "Not org-mode"))
  (unless org-fontify-emphasized-text
    (error "Inline-anki relies on `org-fontify-emphasized-text'"))
  (inline-anki-map-note-things
   (lambda ()
     (message "Processing notes in buffer \"%s\"..." (buffer-name))
     (condition-case-unless-debug err
         (inline-anki--push-note (inline-anki-note-at-point))
       (error (message "Note at point %d failed: %s"
                       (point)
                       (error-message-string err)))))))

(defvar inline-anki-directory
  (if (bound-and-true-p org-roam-directory)
      org-roam-directory
    org-directory))

(defvar inline-anki--file-list nil)

(defun inline-anki-bulk-push ()
  "Run `inline-anki-push-notes' from every file in `inline-anki-directory'."
  (interactive)
  (setq inline-anki--file-list
        (directory-files inline-anki-directory t "\\.org$"))
  (asyncloop-run
    (list
     (defun inline-anki--scan-next-file (loop)
       (let* ((path (pop inline-anki--file-list))
              (visiting (find-buffer-visiting path)))
         ;; (asyncloop-log loop "Scanning for flashcards in: %s" path)
         (with-current-buffer (or visiting (find-file-noselect path))
           (inline-anki-push-notes)
           (when inline-anki--file-list
             (push #'inline-anki--scan-next-file (asyncloop-remainder loop)))
           (unless visiting
             (kill-buffer (current-buffer))))
         path)))
    :debug-buffer-name "*inline-anki bulk worker*"))

;; TODO: send breadcrumbs as an extra field
(defun inline-anki-note-at-point ()
  "Construct an alist representing a note at point."
  (let* (
         ;; Flag at start of list item? Exclude it from note text.
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

         ;; Flag at end of line?  Exclude it from note text.
         (end (save-excursion
                ;; Alas, (org-element-property :contents-end) jumps past all
                ;; sub-items of a list-item.  So we just line-orient and hope
                ;; that the user doesn't hard-wrap.
                (goto-char (line-end-position))
                (unless (re-search-backward (rx "@anki" (*? space) eol) begin t)
                  (re-search-backward (rx (?? "@") "^{" (*? alnum) "}" (*? space) eol)
                                      begin
                                      t))
                (point))))

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
;; TODO: detect if there is more than one flag on the same line (run all
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
    (insert text)
    (goto-char (point-min))
    (let ((n 0))
      (while (setq prop (text-property-search-forward
                         'face inline-anki-emphasis-type t))
        (let ((num (number-to-string (cl-incf n))))
          (goto-char (prop-match-beginning prop))
          (delete-char -1)
          (insert "{{c" num "::")
          (goto-char (+ (prop-match-end prop) 4 (length num)))
          (delete-char 1)
          (insert "}}")))
      (when (= n 0)
        (error "No clozes in note: %s" text)))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'inline-anki)
