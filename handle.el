;;; handle.el --- A handle for things. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/handle
;;
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; A handle for things.

;;; Code:

(defvar handle-alist nil)
(defvar handle-keywords
  '(:evaluators :repls :docs :gotos
                :formaters :compilers :errors))

(defun handle--enlist (exp)
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;;###autoload
(defun handle (modes &rest args)
  (let ((modes (handle--enlist modes))
        (args (cl-loop
               for arg in args collect
               (if (keywordp arg) arg (handle--enlist arg)))))
    (dolist (mode modes)
      (push `(,mode . ,args)
            handle-alist))))

(defun handle--command-execute (commands)
  (when (commands)
    (let ((first (car commands))
          (rest (cdr commands)))
      (condition-case _
          (unless (and (command-execute first)
                       (message (format "`handle' ran %s." first)))
            (handle--command-execte rest))
        (error (handle--command-execute rest))))))

(dolist (keyword handle-keywords)
  (defalias
    (intern (format "handle-%s" (substring (symbol-name keyword) 1)))
    (lambda ()
      (interactive)
      (let ((handle-plist (alist-get major-mode handle-alist)))
        (handle--command-execute
         (plist-get handle-plist keyword))))))

(provide 'handle)
;;; handle.el ends here
