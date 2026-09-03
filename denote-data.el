;;; denote-data.el --- PROOF OF CONCEPT FOR A DENOTE CACHE -*- lexical-binding: t -*-

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

;; PROOF OF CONCEPT FOR A DENOTE CACHE.

;;; Code:

(require 'denote)
(eval-when-compile (require 'cl-lib))

;; NOTE 2026-09-03: We can extend this as needed, such as with file
;; metadata, file contents, forelinks, and backlinks.  Though we need
;; to consider the implications of each addition.
(cl-defstruct (denote-data-entry (:constructor denote-data-entry-create))
  "Data structure of a Denote file."
  identifier signature title keywords)

(defvar denote-data (make-hash-table :test #'equal)
  "List of `denote-data-entry' elements.")

(defun denote-data-write (file)
  "Write data about FILE to `denote-data'."
  (let* ((identifier (or (denote-retrieve-filename-identifier file)
                         (error "The file `%s' does not have an IDENTIFIER" file)))
         (title (denote-retrieve-filename-title file))
         (signature (denote-retrieve-filename-signature file))
         (keywords (denote-retrieve-filename-keywords-as-list file))
         (entry (denote-data-entry-create :identifier identifier :title title :signature signature :keywords keywords)))
    (puthash identifier entry denote-data)))

;;;###autoload
(defun denote-data-write-all (&optional files)
  "Write all FILES to `denote-data'.
If FILES is nil, then write all `denote-directory-files'."
  (when-let* ((files (or files (denote-directory-files))))
    (dolist (file files)
      (denote-data-write file))))

(defun denote-data-get (identifier)
  "Get data about IDENTIFIER in `denote-data'."
  (gethash identifier denote-data))

;; TODO 2026-09-03: We can implement helper functions that modify specific things, such as `denote-data-modify-title'.
(defun denote-data-modify (slot new-value identifier)
  "Modify the SLOT with NEW-VALUE of file with IDENTIFIER in `denote-data'."
  (if-let* ((entry (denote-data-get identifier))
            (new-entry (pcase-exhaustive slot
                         ;; ;; TODO 2026-09-03: If we are changing the
                         ;; ;; identifier then we need to update the
                         ;; ;; struct but also the hashmap.  I do not
                         ;; ;; have enough experience with hashmaps,
                         ;; ;; but I expect this to be possible.  Is
                         ;; ;; it needed though, or should we simply
                         ;; ;; create a new entry and perhaps delete
                         ;; ;; the old one?
                         ;;
                         ;; ('identifier (setf (denote-data-entry-identifier entry) new-value))
                         (:signature (setf (denote-data-entry-signature entry) new-value))
                         (:keywords (setf (denote-data-entry-keywords entry) new-value))
                         (:title (setf (denote-data-entry-title entry) new-value)))))
      (puthash identifier entry denote-data)
    (error "No entry with identifier `%s' in `denote-data'" identifier)))

;; TODO 2026-09-03: Write a minor mode for users to opt in to this
;; functionality.

;; TODO 2026-09-03: The minor mode hooks to buffer saving and all
;; relevant Denote commands that modify the file data.

;; TODO 2026-09-03: What about changes to the file happening outside
;; of Emacs?

;; TODO 2026-09-03: Once all of the above are handled, we can expect
;; the cache to be reliable.  Determine what needs to be done in
;; `denote.el' to SEAMLESSLY integrate the cache for all of its
;; existing functionality.

(provide 'denote-data)
;;; denote-data.el ends here
