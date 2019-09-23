;;; handler.el --- A handler of things. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/handler
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
;; A handler of things.

;;; Code:

(defvar handler-alist ())
(defvar handler-keywords '(:repls :docs :defs))

(defun handler-def (modes &optional args)
  (let ((modes (if (listp modes) modes (list (modes)))))
    (dolist (mode modes)
      (push `(,mode . ,args)
            handler-alist))))

(defun handler--command-execute (commands)
  (if (listp commands)
      (unless (command-execute (car commands))
        (handler--command-execute (cdr commands)))
    (command-execute commands)))

(dolist (keyword handler-keywords)
  (defalias
    (intern (format "handler-%s" (substring (symbol-name keyword) 1)))
    (lambda ()
      (interactive)
      (let ((handler-plist (alist-get major-mode handler-alist)))
        (handler--command-execute
         (plist-get handler-plist keyword))))))

(provide 'handler)
;;; handler.el ends here
