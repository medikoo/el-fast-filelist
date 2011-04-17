;; el-fast-filelist.el --- Fast file access for given folder

;; Author:	Mariusz Nowak <medikoo+el-fast-filelist@medikoo.com>
;; Copyright (C) 2011 Mariusz Nowak <medikoo+el-fast-filelist@medikoo.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; See README.

(require 'el-kit/directory nil t)
(require 'el-kit/file nil t)
(require 'el-kit/key nil t)
(require 'el-kit/list nil t)
(require 'el-index/el-index nil t)

(defvar el-fast-filelist-help-file
	(concat (or (file-name-directory
				(or load-file-name (buffer-file-name) "")) "") "HELP")
	"Path to help file.")

(defun el-fast-filelist-help ()
	"Shows help."
	(interactive)
	(with-help-window (help-buffer) (princ
			(el-kit-file-read el-fast-filelist-help-file))))

;;;###autoload
(defun el-fast-filelist (directory prefix-key &optional options)
	"Configure fast DIRECTORY file list access for given PREFIX-KEY.
	OPTIONS are taken as association list, they're described in README file."
	(lexical-let* (
			(directory
				(if (equal (substring directory (- (length directory) 1)) "/")
					directory
					(concat directory "/")))
			(extension
				(or (assoc 'extension options)
					(el-kit-directory-common-extension directory 0.5)))
			(match (or (assoc 'match options) (concat "^[^.].*" extension "$")))
			(sort (or (assoc 'sort options) 'el-kit-file-access-date-sort))
			(prompt (or (assoc 'prompt options) "Name: "))
			(name (or (assoc 'name options)
					(file-name-nondirectory (substring directory 0 -1))))
			(trim (lambda (path)
					"Trim PATH to filename, if needed trim extension as well."
					(setq path (file-name-nondirectory path))
					(if (equal (concat "." (file-name-extension path)) extension)
						(file-name-sans-extension path)
						path)))
			(getlist (lambda ()
					"Return directory filelist."
					(el-kit-directory-files directory
						match
						sort
						(if extension
							trim
							'file-name-nondirectory))))
			(getfilename (lambda (filename)
					"Return full path to NAME."
					(concat directory filename extension)))
			(open (lambda (filename)
					"Open file NAME."
					(find-file (funcall getfilename filename))))
			(ido (lambda ()
					"Choose file using ido prompt and open it."
					(interactive)
					(funcall open
						(ido-completing-read prompt
							(funcall getlist)))))
			(index-getlist (lambda ()
					"Return list for index buffer."
					(list (cons name
							(mapcar (lambda (filename) (intern filename)) (funcall getlist))))))
			(actions (assoc 'index-actions options))
			(actions (list
					(cons 'write (or (cdr (assoc 'write actions)) (lambda (filename)
								(insert (symbol-name filename)))))
					(cons 'select (or (cdr (assoc 'select actions)) (lambda (filename)
								(funcall open (symbol-name filename)))))
					(cons 'rename (or (cdr (assoc 'rename actions))
							(lambda (filename newname)
								(rename-file
									(funcall getfilename (symbol-name filename))
									(funcall getfilename newname)))))
					(cons 'delete (or (cdr (assoc 'delete actions)) (lambda (filename)
								(delete-file
									(funcall getfilename (symbol-name filename))))))))
			(index (lambda ()
					"Show index buffer with assigned ACTIONS."
					(interactive)
					(el-index-display name (funcall index-getlist) actions)))
			(key-map (make-sparse-keymap)))

		(nconc actions (list (cons 'reload index)))
		(define-key key-map "w" index)
		(define-key key-map "\"" index)
		(define-key key-map "'" ido)
		(define-key key-map "c" ido)
		(define-key key-map "\C-c" ido)
		(define-key key-map "?" 'el-fast-filelist-help)

		(el-kit-key-set prefix-key key-map)))

(provide 'el-fast-filelist/el-fast-filelist)
