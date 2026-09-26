;;; denote-data.el --- Cache Denote files in the `denote-data' hashmap. -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

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

;; Cache Denote files in the `denote-data' hashmap.

;;; Code:

(require 'denote)
(eval-when-compile (require 'cl-lib))

;;;; The cache with `denote-data'

(defgroup denote-data nil
  "Cache Denote files in the `denote-data' hashmap."
  :group 'denote)

;; FIXME 2026-09-26: Can we make `denote-data-write-entry' and
;; `denote-data-write-all' asynchronous while ensuring everything
;; still works?  Then we can even set this to non-nil by default.
;;
;; TODO 2026-09-26: A :set function here is contingent on the above,
;; otherwise it can cause trouble.  Plus, we want to guard against
;; multiple processes, so a `use-package' with a :custom followed by a
;; call to `denote-data-write-all' do not do extra work.
(defcustom denote-data-read-contents nil
  "When non-nil, read file contents for `denote-data'.
Reading file contents means that `denote-data' will include non-nil
slots for forelinks, backlinks, the exact file title, and the entire
text of the file.

When nil, `denote-data' only includes what the Denote file name
provides, namely, identifier, signature, title, keywords, and file path.

NOTE setting this user option to a non-nil value will making the initial
indexing of all files considerably slower.  Here is a sample with 300
moderately sized files (~1000 words on average), showing total elapsed
time in seconds, number of garbage collections, and time spent on
garbage collection:

    (let ((denote-data-read-contents nil))
      (benchmark-run 5 (denote-data-write-all nil :force-update)))
    ;; => (0.16273555299999998 3 0.08853280699997867)

    (let ((denote-data-read-contents t))
      (benchmark-run 5 (denote-data-write-all nil :force-update)))
    ;; => (127.905639756 2226 63.73085589600001)"
  :type 'boolean
  :group 'denote-data)

;;;;; Prepare the cache

(cl-defstruct (denote-data-entry (:constructor denote-data-entry-create))
  "Data structure of a Denote file."
  ;; From file name
  identifier signature title keywords path
  ;; From file contents
  forelinks backlinks text)

(defvar denote-data (make-hash-table :test #'equal)
  "List of `denote-data-entry' elements.")

(defvar denote-data--content-fns
  '((title . denote-data--get-contents-title)
    (forelinks . denote-data--get-contents-forelinks)
    (backlinks . denote-data--get-contents-backlinks)
    (text . denote-data--get-contents-text))
  "List of entries to read data from a file for `denote-data--get-contents'.
Each element is a cons cell of the form (SYMBOL . FUNCTION), where
SYMBOL corresponds to a slot in `denote-data-entry' and thus describes
what FUNCTION is about.")

(defun denote-data--get-contents-title (file-readable-p _identifer file-type)
  "Return title of FILE-TYPE for `denote-data--get-contents'.
Do it when FILE-READABLE-P."
  (when file-readable-p
    (goto-char (point-min))
    (when-let* ((regexp (denote--title-key-regexp file-type))
                (value-fn (denote--title-value-reverse-function file-type))
                (_ (re-search-forward regexp nil t 1)))
      (funcall value-fn (buffer-substring-no-properties (point) (line-end-position))))))

(defun denote-data--get-contents-forelinks (file-readable-p _identifier file-type)
  "Return denote: links of FILE-TYPE for `denote-data--get-contents'.
Do it when FILE-READABLE-P."
  (when file-readable-p
    (goto-char (point-min))
    (let ((forelinks nil))
      (when-let* ((regexp (denote--link-in-context-regexp file-type)))
        (while (re-search-forward regexp nil t)
          (push (match-string 1) forelinks))
        (seq-uniq forelinks)))))

(defun denote-data--get-contents-backlinks (_file identifier _file-type)
  "Return backlinks for file with IDENTIFIER for `denote-data--get-contents'."
  (when-let* ((xrefs (denote-retrieve-xref-alist-for-backlinks identifier)))
    (mapcar #'car xrefs)))

(defun denote-data--get-contents-text (file-readable-p _identifier _file-type)
  "Return `buffer-string' for `denote-data--get-contents'.
Do it when FILE-READABLE-P."
  (when file-readable-p
    (buffer-string)))

(defun denote-data--get-contents (file)
  "Read FILE contents and return relevant `denote-data'.
Do so by using the `denote-data--content-fns'."
  (let ((file-readable-p (file-readable-p file))
        (identifier (denote-retrieve-filename-identifier file))
        (file-type (denote-filetype-heuristics file))
        (data nil))
    (with-temp-buffer
      (insert-file-contents file)
      (pcase-dolist (`(,slot . ,fn) denote-data--content-fns)
        (when-let* ((return (funcall fn file-readable-p identifier file-type)))
          (push (cons slot return) data))))
    data))

(defun denote-data-write-entry (file)
  "Write data about FILE to `denote-data'."
  (when-let* ((identifier (denote-retrieve-filename-identifier file)))
    (let* ((title (denote-retrieve-filename-title file))
           (signature (denote-retrieve-filename-signature file))
           (keywords (denote-retrieve-filename-keywords-as-list file))
           (slots (if-let* ((_ denote-data-read-contents)
                            (data (denote-data--get-contents file)))
                      (let ((contents-title (alist-get 'title data))
                            (forelinks (alist-get 'forelinks data))
                            (backlinks (alist-get 'backlinks data))
                            (text (alist-get 'text data)))
                        (list :identifier identifier
                              :title (or contents-title title)
                              :signature signature
                              :keywords keywords
                              :path file
                              :forelinks forelinks
                              :backlinks backlinks
                              :text text))
                    (list :identifier identifier
                          :title title
                          :signature signature
                          :keywords keywords
                          :path file)))
           (entry (apply 'denote-data-entry-create slots)))
      (puthash identifier entry denote-data))))

(defvar denote-data--write-all-called-p nil
  "Non-nil if `denote-data-write-all' has been called.")

;;;###autoload
(defun denote-data-write-all (&optional files force)
  "Write all FILES to `denote-data'.
If FILES is nil, then write all `denote-directory-files'.

With optional FORCE build up the cache again even if this function was
already called."
  (if-let* ((_ (or force (null denote-data--write-all-called-p)))
            (files (or files (denote--directory-get-files))))
      (progn
        (dolist (file files)
          (denote-data-write-entry file))
        (setq denote-data--write-all-called-p t)
        (message "Created `denote-data' for `%d' files" (length files)))
    (message "Data already exists; call `denote-data-write-all' with FORCE if needed")))

;; NOTE 2026-09-25: The idea with this function is to plug it in to
;; the `denote-directory-files'.  That function would read from this
;; one given some reasonable condition, such as if `denote-data-mode'
;; is non-nil.
(defun denote-data-get-files ()
  "Return list of files in `denote-data'."
  (let ((files nil))
    (maphash
     (lambda (_key value)
       (when-let* ((path (denote-data-entry-path value)))
         (push path files)))
     denote-data)
    files))

;; NOTE 2026-09-25: Here the idea is to call this after a file is
;; deleted or moved outside the `denote-directory'.
(defun denote-data-clear-outdated ()
  "Remove `denote-data' entries that do not correspond to a file."
  (maphash
   (lambda (key value)
     (when-let* ((path (denote-data-entry-path value))
                 (_ (not (file-exists-p path))))
       (remhash key denote-data)))
   denote-data))

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
(denote-data--define-entry-set forelinks)
(denote-data--define-entry-set backlinks)
(denote-data--define-entry-set text)

(defun denote-data-modify (slot new-value identifier)
  "Modify the SLOT with NEW-VALUE of file with IDENTIFIER in `denote-data'."
  (when-let* ((entry (denote-data-get identifier))
              (new-entry (pcase-exhaustive slot
                           (:identifier (denote-data-entry-set-identifier entry new-value))
                           (:signature (denote-data-entry-set-signature entry new-value))
                           (:keywords (denote-data-entry-set-keywords entry new-value))
                           (:title (denote-data-entry-set-title entry new-value))
                           (:path (denote-data-entry-set-path entry new-value))
                           (:forelinks (denote-data-entry-set-forelinks entry new-value))
                           (:backlinks (denote-data-entry-set-backlinks entry new-value))
                           (:text (denote-data-entry-set-text entry new-value)))))
    (puthash identifier entry denote-data)))

(defun denote-data-update (&optional file)
  "Update the current Denote file or FILE entry in `denote-data'."
  (when-let* ((file (or file buffer-file-name))
              (denote-file-has-denoted-filename-p file)
              (identifier (denote-retrieve-filename-identifier file))
              (title (denote-retrieve-filename-title file))
              (signature (denote-retrieve-filename-signature file))
              (keywords (denote-retrieve-filename-keywords-as-list file))
              (entry (denote-data-entry-create :identifier identifier :title title :signature signature :keywords keywords :path file)))
    (puthash identifier entry denote-data)))

;;;;; The `denote-data-mode'

;;;###autoload
(define-minor-mode denote-data-mode
  "When non-nil, cache Denote data in the `denote-data' hashmap and use it.
Activating this mode also calls `denote-data-write-all'."
  :global t
  :init-value nil
  ;; TODO 2026-09-03: What about changes to the file happening outside of Emacs?
  ;; TODO 2026-09-25: Same idea for changes happening in Dired.
  ;; TODO 2026-09-25: What about a rename that changes the identifier?  Maybe a `before-save-hook' for that case?
  (if denote-data-mode
      (progn
        (denote-data-write-all)
        (add-hook 'after-save-hook #'denote-data-update))
    (setq denote-data--write-all-called-p nil)
    (remove-hook 'after-save-hook #'denote-data-update)))

;; TODO 2026-09-03: Determine what needs to be done in `denote.el' to
;; SEAMLESSLY integrate the cache for all of its existing
;; functionality.

(provide 'denote-data)
;;; denote-data.el ends here
