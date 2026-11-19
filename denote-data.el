;;; denote-data.el --- In-memory cache for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-data

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; In-memory cache for Denote.

;;; Code:

(require 'denote)
(eval-when-compile (require 'cl-lib))

(defgroup denote-data nil
  "In-memory cache for Denote."
  :group 'denote)

;;;; The cache with `denote-data'

;; TODO 2026-10-12: Maybe we can leverage the `file-attribute-modification-time'
;; to do this sort of thing: check files for newer modified time and
;; only then update the cache.
(cl-defstruct (denote-data-entry (:constructor denote-data-entry-create))
  "Data structure of a Denote file."
  (identifier nil :documentation "The file IDENTIFIER." :type string)
  (signature nil :documentation "The file SIGNATURE." :type string)
  (title nil :documentation "The file TITLE." :type string)
  (keywords nil :documentation "The file KEYWORDS as a string." :type string)
  (path nil :documentation "The file PATH." :type string)
  (links nil :documentation "The LINKS as a list of identifiers (strings)." :type list))

(defvar denote-data (make-hash-table :test #'equal)
  "The hash-table of `denote-data-entry' elements.")

;;;;; Prepare the cache

(defvar denote-data--content-fns
  '((title . denote-data--get-contents-title)
    (links . denote-data--get-contents-links))
  "List of entries to read data from a file for `denote-data--get-contents'.
Each element is a cons cell of the form (SYMBOL . FUNCTION), where
SYMBOL corresponds to a slot in `denote-data-entry' and thus describes
what FUNCTION is about.")

(defun denote-data--get-contents-title (file-supported-p _identifer file-type)
  "Return title of FILE-TYPE for `denote-data--get-contents'.
Do it when FILE-SUPPORTED-P is non-nil."
  (when file-supported-p
    (goto-char (point-min))
    (when-let* ((regexp (denote--title-key-regexp file-type))
                (value-fn (denote--title-value-reverse-function file-type))
                (_ (re-search-forward regexp nil t 1)))
      (funcall value-fn (buffer-substring-no-properties (point) (line-end-position))))))

(defun denote-data--get-contents-links (file-supported-p _identifier file-type)
  "Return denote: links of FILE-TYPE for `denote-data--get-contents'.
Do it when FILE-SUPPORTED-P is non-nil."
  (when file-supported-p
    (goto-char (point-min))
    (let ((links nil))
      (when-let* ((regexp (denote--link-in-context-regexp file-type)))
        (while (re-search-forward regexp nil t)
          (push (match-string 1) links))
        (seq-uniq links)))))

(defun denote-data--get-contents (file)
  "Read FILE contents and return relevant `denote-data'.
Do not read FILE contents if FILE has
`denote-encryption-file-extensions' or does not conform with
`denote-file-is-writable-and-supported-p'.

Do so by using the `denote-data--content-fns'."
  (let ((file-supported-p (denote-file-is-writable-and-supported-p file))
         (identifier (denote-retrieve-filename-identifier file))
         (file-type (denote-filetype-heuristics file))
         (data nil))
    (with-temp-buffer
      (cond
       ;; NOTE 2026-10-21: Maybe we need a more general approach to
       ;; how we `insert-file-contents' when dealing with encrypted
       ;; files, but this is the only case I am aware of right now
       ;; where we can run into problems.
       ((when-let* ((extension (file-name-extension file :period))
                    (_ (member extension denote-encryption-file-extensions)))
          (setq file-supported-p nil)
          t))
       (file-supported-p
        (insert-file-contents file)))
      (pcase-dolist (`(,slot . ,fn) denote-data--content-fns)
        (when-let* ((return (funcall fn file-supported-p identifier file-type)))
          (push (cons slot return) data))))
    data))

(defun denote-data-write-entry (file)
  "Write data about FILE to `denote-data'."
  (when-let* ((_ (file-exists-p file))
              (identifier (denote-retrieve-filename-identifier file)))
    (let* ((title (denote-retrieve-filename-title file))
           (signature (denote-retrieve-filename-signature file))
           (keywords (denote-retrieve-filename-keywords file))
           (data (denote-data--get-contents file))
           (contents-title (alist-get 'title data))
           (links (alist-get 'links data))
           (slots (list :identifier identifier
                        :title (or contents-title title)
                        :signature signature
                        :keywords keywords
                        :path file
                        :links links))
           (entry (apply 'denote-data-entry-create slots)))
      (puthash identifier entry denote-data))))

;; NOTE 2026-10-27: If I do `clrhash', then I get confusing output
;; from `hash-table-keys' and `hash-table-size'.  The former returns
;; nil, which is what I expect, but the latter returns the length that
;; the hash-table had.  Indeed, if I check the Help buffer it still
;; shows #<hash-table equal 0/N...>.  I guess this is normal, so I
;; will keep it this way for now.
(defun denote-data-clear ()
  "Clear `denote-data'."
  (setq denote-data (clrhash denote-data)))

(defvar denote-data--write-all-called-p nil
  "Non-nil if `denote-data-write-all' has been called.")

;; FIXME 2026-10-05: I just realised that in `denote-get-path-by-id'
;; we actually check if there are multiple files with the same
;; identifier (`denote--get-path-by-id-pick-likely-authoritative-file').
;; But here we will not have that flexibility because there will be
;; only one entry in the hash-table.  Maybe that logic should be built
;; into how we build up the cache in `denote-data-write-all'?  Or
;; maybe that goes even deeper into `denote--directory-get-files'?

;; TODO 2026-10-16: What about silos?
(defun denote-data-write-all (&optional files force)
  "Write all FILES to `denote-data'.
If FILES is nil, then write all files in the variable `denote-directory'
that satisfy `denote-file-has-denoted-filename-p'.

With optional FORCE build up the cache again even if this function was
already called."
  (when force
    (denote-data-clear))
  (if-let* ((_ (or force (null denote-data--write-all-called-p))))
      (if-let* ((files (or files (denote--directory-get-files)))
                (total (length files))
                (reporter (make-progress-reporter "`denote-data' processing files..." 0 total 0 1 1.5))
                (index 1))
          (progn
            (dolist (file files)
              (progress-reporter-update reporter index)
              (setq index (+ index 1))
              (denote-data-write-entry file))
            (setq denote-data--write-all-called-p t)
            (progress-reporter-done reporter))
        (message "No files; nothing to add to `denote-data'"))
    (message "Data already exists; call `denote-data-write-all' with FORCE if needed")))

;;;;; Operate on a single `denote-data' entry

(defun denote-data-get (identifier)
  "Get data about IDENTIFIER in `denote-data'."
  (gethash identifier denote-data))

(defmacro denote-data--define-entry-set (slot)
  "Define setter function for SLOT in `denote-data-entry'."
  `(defun ,(intern (format "denote-data-entry-set-%s" slot)) (entry new-value)
     ,(format "Set ENTRY %s to NEW-VALUE." slot)
     (setf (,(intern (format "denote-data-entry-%s" slot)) entry) new-value)))

(denote-data--define-entry-set identifier)
(denote-data--define-entry-set signature)
(denote-data--define-entry-set title)
(denote-data--define-entry-set keywords)
(denote-data--define-entry-set path)
(denote-data--define-entry-set links)

(defun denote-data-modify (slot new-value identifier)
  "Modify the SLOT with NEW-VALUE of file with IDENTIFIER in `denote-data'."
  (when-let* ((entry (denote-data-get identifier)))
    (pcase-exhaustive slot
      (:identifier (denote-data-entry-set-identifier entry new-value))
      (:signature (denote-data-entry-set-signature entry new-value))
      (:keywords (denote-data-entry-set-keywords entry new-value))
      (:title (denote-data-entry-set-title entry new-value))
      (:path (denote-data-entry-set-path entry new-value))
      (:links (denote-data-entry-set-links entry new-value)))))

(defun denote-data-update ()
  "Update the current Denote file entry in `denote-data'.
Use this as part of `after-save-hook' or related.  Otherwise use
`denote-data-write-entry'."
  (when (denote-file-has-denoted-filename-p buffer-file-name)
    (denote-data-write-entry buffer-file-name)))

(defun denote-data-add-file (identifier signature title keywords note-path)
  "Add file to `denote-data'.
IDENTIFIER, SIGNATURE, TITLE, KEYWORDS, NOTE-PATH are those documented
in `denote-new-note-functions'."
  (let* ((buffer (get-file-buffer note-path))
         (data (denote-data--get-contents note-path))
         (links (alist-get 'links data))
         (slots (list :identifier identifier
                      :title title
                      :signature signature
                      :keywords (denote-keywords-combine keywords)
                      :path note-path
                      :links links))
         (entry (apply 'denote-data-entry-create slots)))
    (puthash identifier entry denote-data)))

(defun denote-data-remove-file (file)
  "Remove FILE from `denote-data'.
FILE may be a file path or the identifier."
  (let ((id (denote-retrieve-filename-identifier file)))
    (remhash id denote-data)
    (message "Removed file with identifier `%s' from `denote-data'" id)))

(defun denote-data-post-rename-update (old-identifier identifier title keywords signature new-name)
  "Function to add to `denote-after-rename-functions'.
The arguments OLD-IDENTIFIER, IDENTIFIER, TITLE, KEYWORDS, SIGNATURE,
NEW-NAME have the meaning of that hook."
  (remhash old-identifier denote-data)
  (let* ((data (denote-data--get-contents new-name))
         (links (alist-get 'links data))
         (slots (list :identifier identifier
                      :title title
                      :signature signature
                      :keywords (denote-keywords-combine keywords)
                      :path new-name
                      :links links))
         (entry (apply 'denote-data-entry-create slots)))
    (puthash identifier entry denote-data)))

;;;;; The asynchronous call to build the `denote-data'

(defvar denote-data--write-all-asynchronous-process nil
  "Process object of `denote-data--write-all-asynchronous'.")

(defun denote-data--cancel-asynchronous ()
  "Cancel the asynchronous write process."
  (when (and denote-data--write-all-asynchronous-process
             (process-live-p denote-data--write-all-asynchronous-process))
    (kill-process denote-data--write-all-asynchronous-process)
    (setq denote-data--write-all-asynchronous-process nil)))

(defvar denote-data-mode)

(defun denote-data--write-all-asynchronous-sentinel-done (data)
  "Set the `denote-data' to DATA as part of the process sentinel."
  (when denote-data-mode
    (denote-data-mode--enable))
  (setq denote-data data))

(defun denote-data--write-all-asynchronous-sentinel-fail ()
  "Handle case where `denote-data' failed."
  (when denote-data-mode
    (denote-data-mode--disable)
    (denote-data-mode -1))
  (error "Could not generate `denote-data' asynchronously"))

(defun denote-data--write-all-asynchronous-sentinel (process event)
  "Process sentinel for `denote-data--write-all-asynchronous'.
PROCESS and EVENT are the arguments described in Info node `(elisp) Sentinels'."
  (cond
   ((string= event "finished\n")
    (setq denote-data--write-all-called-p t)
    (when-let* ((buffer-process (process-buffer process)))
      (with-current-buffer buffer-process
        (goto-char (point-min))
        (if-let* ((data (read (current-buffer)))
                  (_ (hash-table-p data)))
            (denote-data--write-all-asynchronous-sentinel-done data)
          (denote-data--write-all-asynchronous-sentinel-fail)))))
   ((string-match-p "\\(exited abnormally\\|failed with code\\)" event)
    (message "FAILED to build `denote-data'; something unexpected happened"))))

(defun denote-data--write-all-asynchronous-progress-pipe-filter (process string)
  "Process filter for `make-pipe-process' of `denote-data--write-all-asynchronous'.
PROCESS and STRING are the arguments described in Info node `(elisp)
Filter Functions'."
  ;; I learnt about `message-log-max' from `progress-reporter--pulse-characters'.
  (let ((message-log-max nil))
    (message "%s" (string-trim string))
    (when-let* ((buffer-pipe (process-buffer process)))
      (with-current-buffer buffer-pipe
        (save-excursion
          (goto-char (point-max))
          (insert string))))))

(defun denote-data--write-all-asynchronous-get-buffer (name)
  "Return buffer with NAME for `denote-data--write-all-asynchronous'."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (erase-buffer))
    buffer))

;; TODO 2026-10-15: Double-check that the configuration here is
;; exhaustive.  We want to capture EVERYTHING.  Specifically, we might
;; need to think about options to extend this, such as
;; `denote-journal-directory', and so on.  Maybe the best approach is
;; to have some kind of arrangement whereby packages/users add
;; arbitrary things for us to include in the cache's configuration.
;; This could be as simple as adding an OPTIONS parameter which is an
;; alist of cons cells of the form (USER-OPTION . VALUE) and then we
;; take care to run the formatting here.  That would have to be called
;; before `denote-data-mode' or the cache would need to be rebuilt
;; accordingly.
(defvar denote-data-asynchronous-build-variables
  '( denote-directory denote-file-type denote-file-types
     denote-file-name-components-order denote-excluded-directories-regexp
     denote-keywords-to-not-infer-regexp denote-excluded-files-regexp
     denote-file-name-slug-functions)
  "List of variables to consider in the asynchronous build of `denote-data'.")

(defun denote-data--get-user-configuration ()
  "Return user configuration for `denote-data--write-all-asynchronous'."
  (let* ((format-fn (lambda (symbol)
                      ;; TODO 2026-10-15: Maybe tweak the formatting
                      ;; here, but the point is that it works.
                      (let ((value (symbol-value symbol)))
                        (format "%s %s%S\n"
                                symbol
                                (if (and value
                                         (or (listp value)
                                             (symbolp value)))
                                    "'"
                                  "")
                                value))))
         (variables (mapconcat format-fn denote-data-asynchronous-build-variables "")))
    (format "(setq\n%s)" variables)))

(defun denote-data--write-all-asynchronous (&optional force)
  "Call `denote-data-write-all' in a separate process.
With optional FORCE run the process again even if it was already called
before."
  (when (or force (null denote-data--write-all-called-p))
    (let* ((buffer-output (denote-data--write-all-asynchronous-get-buffer " *denote-data*"))
           (buffer-error (denote-data--write-all-asynchronous-get-buffer " *denote-data-error*"))
           (denote-source-file (or (locate-library "denote")
                                   (error "File denote.el is not in the `load-path'")))
           (progress-pipe (make-pipe-process
                           :name "denote-data-progress-pipe"
                           :buffer buffer-error
                           :filter #'denote-data--write-all-asynchronous-progress-pipe-filter))
           (emacs-binary (expand-file-name invocation-name invocation-directory))
           (user-options (denote-data--get-user-configuration))
           (command (list emacs-binary "--batch" "-Q" "-l" denote-source-file "--eval"
                          (format "(progn %s (denote-data-write-all) (prin1 denote-data))" user-options)))
           (process (make-process
                     :name "denote-data"
                     :buffer buffer-output
                     :stderr progress-pipe
                     :command command
                     :sentinel #'denote-data--write-all-asynchronous-sentinel)))
      (setq denote-data--write-all-asynchronous-process process))))

;;;;; The `denote-data-mode'

(defun denote-data-get-identifiers ()
  "Return all identifiers as a hash-table.
Include `denote-get-buffer-file-names'."
  (let* ((table (make-hash-table :test #'equal))
         (buffer-file-names (denote-get-buffer-file-names))
         (buffer-identifiers (mapcar #'denote-retrieve-filename-identifier buffer-file-names))
         (cached-identifiers (hash-table-keys denote-data))
         (all-identifiers nil))
    (setq all-identifiers (seq-uniq (append buffer-identifiers cached-identifiers)))
    (dolist (identifier all-identifiers)
      (puthash identifier t table))
    table))

(defun denote-data-get-files ()
  "Return list of files in `denote-data'."
  (let ((files nil))
    (maphash
     (lambda (_key value)
       (when-let* ((path (denote-data-entry-path value)))
         (push path files)))
     denote-data)
    files))

(defun denote-data-get-keywords-all (&optional files-matching-regexp)
  "Return keywords found in files, optionally FILES-MATCHING-REGEXP.
Respect `denote-keywords-to-not-infer-regexp' and do not remove any duplicates."
  (let ((keywords nil))
    (maphash
     (lambda (_key value)
       (when-let* ((path (denote-data-entry-path value))
                   (file-keywords (denote-data-entry-keywords value))
                   (file-keywords-list (split-string file-keywords "_" :omit-nulls))
                   (final-keywords (denote--infer-keywords-from-files-filter file-keywords-list)))
         (if files-matching-regexp
             (when (string-match-p files-matching-regexp path)
               (push final-keywords keywords))
           (push final-keywords keywords))))
     denote-data)
    (flatten-list keywords)))

;; TODO 2026-10-05: We need to deal with the scenario where one
;; identifier is shared by multiple files.  This can happen, for
;; example, if the Org file is exported to an HTML file which is
;; stored in the same directory.  The exported file has the same name
;; as the original except for a different file type extension.
(defun denote-data-get-path (identifier)
  "Return file path of IDENTIFIER."
  (when-let* ((entry (denote-data-get identifier))
              (path (denote-data-entry-path entry)))
    path))

;; This is because the original function is called with a FILE
;; argument, as in `denote-get-backlinks-as-files-function'.  Maybe
;; long-term we should change everything to expect an IDENTIFIER
;; instead.
(defun denote-data--resolve-file-identifier (arg)
  "Return ARG identifier if it is a file, else return ARG."
  (if (file-exists-p (expand-file-name arg))
      (denote-retrieve-filename-identifier arg)
    arg))

(defun denote-data-get-backlinks-files-only (identifier)
  "Return list of FILES that link to file with IDENTIFIER.
Also see `denote-data-get-backlinks'."
  (let ((id (denote-data--resolve-file-identifier identifier))
        (files nil))
    (maphash
     (lambda (_key value)
       (when-let* ((path (denote-data-entry-path value))
                   (links (denote-data-entry-links value))
                   (_ (member id links)))
         (push path files)))
     denote-data)
    files))

(defun denote-data-get-title (identifier)
  "Return title of file with IDENTIFIER."
  (when-let* ((id (denote-data--resolve-file-identifier identifier))
              (entry (denote-data-get id)))
    (denote-data-entry-title entry)))

(defun denote-data-get-signature (identifier)
  "Return signature of file with IDENTIFIER."
  (when-let* ((id (denote-data--resolve-file-identifier identifier))
              (entry (denote-data-get id)))
    (denote-data-entry-signature entry)))

(defun denote-data-get-keywords (identifier)
  "Return keywords of file with IDENTIFIER as a string."
  (when-let* ((id (denote-data--resolve-file-identifier identifier))
              (entry (denote-data-get id)))
    (denote-data-entry-keywords entry)))

(defun denote-data-get-backlinks (identifier)
  "Return an xref alist of backlinks for IDENTIFIER.
Also see `denote-data-get-backlinks-files-only'."
  (when-let* ((backlinks (denote-data-get-backlinks-files-only identifier)))
    (let ((backlinks-by-file-type (denote--get-files-by-file-type backlinks)))
      (denote--make-xref-alist identifier backlinks-by-file-type))))

(defvar denote-directory-files-get-function--original nil)
(defvar denote-file-has-backlinks-function--original nil)
(defvar denote-get-backlinks-as-files-function--original nil)
(defvar denote-get-identifiers-function--original nil)
(defvar denote-get-keywords-function--original nil)
(defvar denote-get-path-by-id-function--original nil)
(defvar denote-get-signature-function--original nil)
(defvar denote-get-title-function--original nil)
(defvar denote-infer-keywords-from-files-function--original nil)
(defvar denote-retrieve-xref-alist-for-backlinks-function--original nil)

(defun denote-data-mode--enable ()
  "Change the state when `denote-data-mode' is enabled."
  (setq denote-directory-files-get-function--original denote-directory-files-get-function)
  (setq denote-file-has-backlinks-function--original denote-file-has-backlinks-function)
  (setq denote-get-backlinks-as-files-function--original denote-get-backlinks-as-files-function)
  (setq denote-get-identifiers-function--original denote-get-identifiers-function)
  (setq denote-get-keywords-function--original denote-get-keywords-function)
  (setq denote-get-path-by-id-function--original denote-get-path-by-id-function)
  (setq denote-get-signature-function--original denote-get-signature-function)
  (setq denote-get-title-function--original denote-get-title-function)
  (setq denote-infer-keywords-from-files-function--original denote-infer-keywords-from-files-function)
  (setq denote-retrieve-xref-alist-for-backlinks-function--original denote-retrieve-xref-alist-for-backlinks-function)

  (setq denote-directory-files-get-function #'denote-data-get-files)
  (setq denote-file-has-backlinks-function #'denote-data-get-backlinks-files-only)
  (setq denote-get-backlinks-as-files-function #'denote-data-get-backlinks-files-only)
  (setq denote-get-identifiers-function #'denote-data-get-identifiers)
  (setq denote-get-keywords-function #'denote-data-get-keywords)
  (setq denote-get-path-by-id-function #'denote-data-get-path)
  (setq denote-get-signature-function #'denote-data-get-signature)
  (setq denote-get-title-function #'denote-data-get-title)
  (setq denote-infer-keywords-from-files-function #'denote-data-get-keywords-all)
  (setq denote-retrieve-xref-alist-for-backlinks-function #'denote-data-get-backlinks)

  (add-hook 'after-save-hook #'denote-data-update)
  (add-hook 'denote-new-note-functions #'denote-data-add-file)
  (add-hook 'denote-link-file-no-exist-functions #'denote-data-remove-file)
  (add-hook 'denote-after-rename-functions #'denote-data-post-rename-update))

(defun denote-data-mode--disable ()
  "Undo changes made by `denote-data-mode'."
  (setq denote-directory-files-get-function denote-directory-files-get-function--original)
  (setq denote-file-has-backlinks-function denote-file-has-backlinks-function--original)
  (setq denote-get-backlinks-as-files-function denote-get-backlinks-as-files-function--original)
  (setq denote-get-identifiers-function denote-get-identifiers-function--original)
  (setq denote-get-keywords-function denote-get-keywords-function--original)
  (setq denote-get-path-by-id-function denote-get-path-by-id-function--original)
  (setq denote-get-signature-function denote-get-signature-function--original)
  (setq denote-get-title-function denote-get-title-function--original)
  (setq denote-infer-keywords-from-files-function denote-infer-keywords-from-files-function--original)
  (setq denote-retrieve-xref-alist-for-backlinks-function denote-retrieve-xref-alist-for-backlinks-function--original)

  (setq denote-data--write-all-called-p nil)
  (denote-data-clear)
  (remove-hook 'after-save-hook #'denote-data-update)
  (remove-hook 'denote-new-note-functions #'denote-data-add-file)
  (remove-hook 'denote-link-file-no-exist-functions #'denote-data-remove-file)
  (remove-hook 'denote-after-rename-functions #'denote-data-post-rename-update))

;;;###autoload
(define-minor-mode denote-data-mode
  "When enabled, store all Denote file data in `denote-data' and use it.
If `denote-data' fails to build, this mode is automatically disabled."
  :global t
  :init-value nil
  (denote-data--cancel-asynchronous)
  (if denote-data-mode
      (denote-data--write-all-asynchronous)
    (denote-data-mode--disable)))

(provide 'denote-data)
;;; denote-data.el ends here
