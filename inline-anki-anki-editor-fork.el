;;; inline-anki-anki-editor-fork.el --- Functions from anki-editor  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Lei Tan <louietanlei@gmail.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'org-element)
(require 'ox)
(require 'ox-html)
(require 'request)

(defcustom inline-anki-break-consecutive-braces-in-latex
  nil
  "If non-nil, consecutive `}' will be automatically separated by spaces to prevent early-closing of cloze.
See https://apps.ankiweb.net/docs/manual.html#latex-conflicts.")

(defcustom inline-anki-protected-tags
  '("marked" "leech")
  "A list of tags that won't be deleted from Anki even though they're absent in Org entries, such as special tags `marked', `leech'."
  :type '(repeat string))

(defcustom inline-anki-ignored-org-tags
  (append org-export-select-tags org-export-exclude-tags)
  "A list of Org tags that are ignored when constructing notes form entries."
  :type '(repeat string))

(defcustom inline-anki-anki-connect-listening-address
  "127.0.0.1"
  "The network address AnkiConnect is listening.")

(defcustom inline-anki-anki-connect-listening-port
  "8765"
  "The port number AnkiConnect is listening.")

(defcustom inline-anki-use-math-jax nil
  "Use Anki's built in MathJax support instead of LaTeX.")

;;; AnkiConnect

(defun inline-anki--anki-connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))

(defun inline-anki--anki-connect-invoke-queue ()
  (let (action-queue)
    (lambda (&optional action params handler)
      (if action
          (push (cons (inline-anki--anki-connect-action action params) handler) action-queue)
        (when action-queue
          (apply #'inline-anki--anki-connect-invoke-multi (nreverse action-queue))
          (setq action-queue nil))))))

(defun inline-anki--anki-connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (inline-anki--anki-connect-action action params 5)))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (let ((response (request (format "http://%s:%s"
                                     inline-anki-anki-connect-listening-address
                                     inline-anki-anki-connect-listening-port)
                             :type "POST"
                             :parser 'json-read
                             :data request-body
                             :success (cl-function (lambda (&key data &allow-other-keys)
                                                     (setq reply data)))
                             :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                                   (setq err (string-trim (cdr error-thrown)))))
                             :sync t)))

      ;; HACK: With sync set to t, `request' waits for curl process to
      ;; exit, then response data becomes available, but callbacks
      ;; might not be called right away but at a later time, that's
      ;; why here we manually invoke callbacks to receive the result.
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defmacro inline-anki--anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (inline-anki--anki-connect-invoke ,@args)
     (when .error (error .error))
     .result))

(defun inline-anki--anki-connect-invoke-multi (&rest actions)
  (-zip-with (lambda (result handler)
               (when-let ((_ (listp result))
                          (err (alist-get 'error result)))
                 (error err))
               (and handler (funcall handler result)))
             (inline-anki--anki-connect-invoke-result
              "multi" `((actions . ,(mapcar #'car actions))))
             (mapcar #'cdr actions)))

(defun inline-anki--anki-connect-map-note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (let-alist note
    (list (cons "id" .note-id)
          (cons "deckName" .deck)
          (cons "modelName" .note-type)
          (cons "fields" .fields)
          ;; Convert tags to a vector since empty list is identical to nil
          ;; which will become None in Python, but AnkiConnect requires it
          ;; to be type of list.
          (cons "tags" (vconcat .tags)))))


;;; Org Export Backend

(defconst inline-anki--ox-anki-html-backend
  (if inline-anki-use-math-jax
      (org-export-create-backend
       :parent 'html
       :transcoders '((latex-fragment . inline-anki--ox-latex-for-mathjax)
                      (latex-environment . inline-anki--ox-latex-for-mathjax)))
    (org-export-create-backend
     :parent 'html
     :transcoders '((latex-fragment . inline-anki--ox-latex)
                    (latex-environment . inline-anki--ox-latex)))))

(defun inline-anki--translate-latex-delimiters (latex-code)
  (catch 'done
    (let ((delimiter-map (list (list (cons (format "^%s" (regexp-quote "$$")) "[$$]")
                                     (cons (format "%s$" (regexp-quote "$$")) "[/$$]"))
                               (list (cons (format "^%s" (regexp-quote "$")) "[$]")
                                     (cons (format "%s$" (regexp-quote "$")) "[/$]"))
                               (list (cons (format "^%s" (regexp-quote "\\(")) "[$]")
                                     (cons (format "%s$" (regexp-quote "\\)")) "[/$]"))
                               (list (cons (format "^%s" (regexp-quote "\\[")) "[$$]")
                                     (cons (format "%s$" (regexp-quote "\\]")) "[/$$]"))))
          (matched nil))
      (save-match-data
        (dolist (pair delimiter-map)
          (dolist (delimiter pair)
            (when (setq matched (string-match (car delimiter) latex-code))
              (setq latex-code (replace-match (cdr delimiter) t t latex-code))))
          (when matched (throw 'done latex-code)))))
    latex-code))

(defun inline-anki--translate-latex-delimiters-to-anki-mathjax-delimiters (latex-code)
  (catch 'done
    (let ((delimiter-map (list (list (cons (format "^%s" (regexp-quote "$$")) "\\[")
                                     (cons (format "%s$" (regexp-quote "$$")) "\\]"))
                               (list (cons (format "^%s" (regexp-quote "$")) "\\(")
                                     (cons (format "%s$" (regexp-quote "$")) "\\)"))))
          (matched nil))
      (save-match-data
        (dolist (pair delimiter-map)
          (dolist (delimiter pair)
            (when (setq matched (string-match (car delimiter) latex-code))
              (setq latex-code (replace-match (cdr delimiter) t t latex-code))))
          (when matched (throw 'done latex-code)))))
    latex-code))

(defun inline-anki--wrap-latex (content)
  "Wrap CONTENT with Anki-style latex markers."
  (format "<p><div>[latex]</div>%s<div>[/latex]</div></p>" content))

(defun inline-anki--wrap-latex-for-mathjax (content)
  "Wrap CONTENT for Anki's native MathJax support."
  (format "<p>%s</p>" content))

(defun inline-anki--wrap-div (content)
  (format "<div>%s</div>" content))

(defun inline-anki--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code
          (pcase (org-element-type latex)
            ('latex-fragment (inline-anki--translate-latex-delimiters code))
            ('latex-environment (inline-anki--wrap-latex
                                 (mapconcat #'inline-anki--wrap-div
                                            (split-string (org-html-encode-plain-text code) "\n")
                                            "")))))

    (if inline-anki-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun inline-anki--ox-latex-for-mathjax (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code
          (pcase (org-element-type latex)
            ('latex-fragment (inline-anki--translate-latex-delimiters-to-anki-mathjax-delimiters code))
            ('latex-environment (inline-anki--wrap-latex-for-mathjax
                                 (mapconcat #'inline-anki--wrap-div
                                            (split-string (org-html-encode-plain-text code) "\n")
                                            "")))))

    (if inline-anki-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))


;;; Core Functions

(defun inline-anki--create-note (note)
  "Request AnkiConnect for creating NOTE."
  (let ((queue (inline-anki--anki-connect-invoke-queue)))
    (funcall queue
             'addNote
             `((note . ,(inline-anki--anki-connect-map-note note)))
             #'inline-anki--dangerously-write-id)

    (funcall queue)))

(defun inline-anki--update-note (note)
  "Request AnkiConnect for updating fields and tags of NOTE."

  (let ((queue (inline-anki--anki-connect-invoke-queue)))
    (funcall queue
             'updateNoteFields
             `((note . ,(inline-anki--anki-connect-map-note note))))

    (funcall queue
             'notesInfo
             `((notes . (,(alist-get 'note-id note))))
             (lambda (result)
               ;; update tags
               (let* ((existing-note (car result))
                      (tags-to-add (-difference (-difference (alist-get 'tags note)
                                                             (alist-get 'tags existing-note))
                                                inline-anki-ignored-org-tags))
                      (tags-to-remove (-difference (-difference (alist-get 'tags existing-note)
                                                                (alist-get 'tags note))
                                                   inline-anki-protected-tags))
                      (tag-queue (inline-anki--anki-connect-invoke-queue)))

                 (when tags-to-add
                   (funcall tag-queue
                            'addTags `((notes . (,(alist-get 'note-id note)))
                                       (tags . ,(string-join tags-to-add " ")))))

                 (when tags-to-remove
                   (funcall tag-queue
                            'removeTags `((notes . (,(alist-get 'note-id note)))
                                          (tags . ,(string-join tags-to-remove " ")))))

                 (funcall tag-queue))))

    (funcall queue)))


(provide 'inline-anki-anki-editor-fork)

;;; inline-anki-anki-editor-fork.el ends here
