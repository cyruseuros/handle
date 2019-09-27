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

;; `handle' provides handlers for functions frequently shared accros
;; major modes, and often performend by multiple callables in the same
;; one (major mode) such as:

;; - evaluating expressions
;; - starting repls
;; - finding documentation
;; - going to definition
;; - formatting code
;; - compiling code
;; - lisiting errors

;; `handle' generates `handle-*' functions based on (user-specified)
;; `handle-keywords' that can then be bound to a single global `kbd'
;; and used in any major mode.

;;; Code:

(defvar handle-alist nil)
(defvar handle-keywords
  '(:evaluators :repls :docs :gotos
                :formaters :compilers :errors))

(defun handle--enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun handle--keyword-name (keyword)
  "Get KEYWORD name as a string."
  (substring (symbol-name keyword) 1))

(defalias 'handle
  (lambda (modes &rest args)
    (let ((modes (handle--enlist modes))
          (args (cl-loop
                 for arg in args collect
                 (if (keywordp arg) arg
                   (handle--enlist arg)))))
      (dolist (mode modes)
        (push `(,mode . ,args)
              handle-alist))))
  (format
   "Define handles for MODES through plist ARGS.
You can use any keyword from `handle-keywords', as long as you
define them before the package is loaded.

\(fn MODES &key %s)"
   (upcase (mapconcat #'handle--keyword-name handle-keywords " "))))

(defun handle--command-execute (commands &optional message &rest args)
  "Run COMMANDS with `command-execute'.
Stop when one returns non-nil.  Try next command on `error'.
`message' MESSAGE `format'ted with ARGS."
  (when message
    (message (apply #'format message args)))
  (when commands
    (let ((first (car commands))
          (rest (cdr commands)))
      (condition-case nil
          (unless (and (command-execute first)
                       (message (format "`handle' ran %s successfully." first)))
            (handle--command-execute rest "`handle' ran %s unsuccessfully." first))
        (error (handle--command-execute rest "`handle' failed to run %s." first))))))

(dolist (keyword handle-keywords)
  (let ((keyword-name (handle--keyword-name keyword)))
    (defalias
      (intern (format "handle-%s" keyword-name))
      (lambda nil
        (interactive)
        (let* ((handle-plist (alist-get major-mode handle-alist))
               (handle-list (plist-get handle-plist keyword)))
          (if handle-list
              (handle--command-execute handle-list)
            (message (format "No `handle' for %s %s."
                             major-mode keyword-name)))))
      (format "`handle' %s." keyword-name))))

(provide 'handle)
;;; handle.el ends here
