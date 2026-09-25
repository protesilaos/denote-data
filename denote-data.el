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

(defgroup denote-data nil
  "Cache Denote files in the `denote-data' hashmap."
  :group 'denote)

;; NOTE 2026-09-03: We can extend this as needed, such as with file
;; metadata, file contents, forelinks, and backlinks.  Though we need
;; to consider the implications of each addition.
(cl-defstruct (denote-data-entry (:constructor denote-data-entry-create))
  "Data structure of a Denote file."
  identifier signature title keywords path)

(defvar denote-data (make-hash-table :test #'equal)
  "List of `denote-data-entry' elements.")

(defun denote-data-write (file)
  "Write data about FILE to `denote-data'."
  (let* ((identifier (or (denote-retrieve-filename-identifier file)
                         (error "The file `%s' does not have an IDENTIFIER" file)))
         (title (denote-retrieve-filename-title file))
         (signature (denote-retrieve-filename-signature file))
         (keywords (denote-retrieve-filename-keywords-as-list file))
         (entry (denote-data-entry-create :identifier identifier :title title :signature signature :keywords keywords :path file)))
    (puthash identifier entry denote-data)))

;;;###autoload
(defun denote-data-write-all (&optional files)
  "Write all FILES to `denote-data'.
If FILES is nil, then write all `denote-directory-files'."
  (when-let* ((files (or files (denote--directory-get-files))))
    (dolist (file files)
      (denote-data-write file))))

(defun denote-data-get-files ()
  "Return list of files in `denote-data'."
  (let ((files nil))
    (maphash
     (lambda (_key value)
       (when-let* ((path (denote-data-entry-path value)))
         (push path files)))
     denote-data)
    files))

;; TODO 2026-09-04: We need a function to automatically delete stale
;; data.  For example, if we have data about a file that has since
;; been deleted.  Maybe there are other cases.
(defun denote-data-get (identifier)
  "Get data about IDENTIFIER in `denote-data'."
  (gethash identifier denote-data))

;; TODO 2026-09-03: We can implement helper functions that modify specific things, such as `denote-data-modify-title'.
(defun denote-data-modify (slot new-value identifier)
  "Modify the SLOT with NEW-VALUE of file with IDENTIFIER in `denote-data'."
  (if-let* ((entry (denote-data-get identifier))
            (new-entry (pcase-exhaustive slot
                         (:identifier (setf (denote-data-entry-identifier entry) new-value))
                         (:signature (setf (denote-data-entry-signature entry) new-value))
                         (:keywords (setf (denote-data-entry-keywords entry) new-value))
                         (:title (setf (denote-data-entry-title entry) new-value))
                         (:path (setf (denote-data-entry-path entry) new-value)))))
      (puthash identifier entry denote-data)
    (error "No entry with identifier `%s' in `denote-data'" identifier)))

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

;;;###autoload
(define-minor-mode denote-data-mode
  "When non-nil, cache Denote data in the `denote-data' hashmap and use it."
  :global t
  :init-value nil
  ;; TODO 2026-09-03: What about changes to the file happening outside of Emacs?
  ;; TODO 2026-09-25: Same idea for changes happening in Dired.
  ;; TODO 2026-09-25: What about a rename that changes the identifier?  Maybe a `before-save-hook' for that case?
  (if denote-data-mode
      (add-hook 'after-save-hook #'denote-data-update)
    (remove-hook 'after-save-hook #'denote-data-update)))

;; TODO 2026-09-03: Determine what needs to be done in `denote.el' to
;; SEAMLESSLY integrate the cache for all of its existing
;; functionality.

(provide 'denote-data)
;;; denote-data.el ends here
