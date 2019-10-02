;;; handle.el --- A handle for major-mode generic functions. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/handle
;;
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1") (parent-mode "2.3"))

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
;; A handle for major-mode generic functions.

;; `handle' provides generic functions that specialize on major modes
;; and intended purpose instead of arguments.  Similar functionality is
;; needed across programming languages such as:

;; - evaluating expressions
;; - starting repls
;; - finding documentation
;; - going to definition
;; - formatting code
;; - compiling code
;; - listing errors

;; But this functionality is performed by different callables across
;; major modes and often by multiple callables in the same
;; one.  `handle' groups them together so that they can be bound to a
;; single global `kbd', selected based on major mode, and run in
;; sequence until one succeeds.

;;; Code:
(require 'parent-mode)

(defvar handle-alist nil
  "`handle' dispatch alist.
Associates major modes with handlers.")

(defvar handle-nil 'all
  "List of commands that return nil on success.
If `all', treat all commands passed to `handle' this way.")

(defvar handle-keywords
  '(:evaluators :repls :docs :gotos
                :formatters :compilers :errors)
  "Package author's preffered keywords.
Users are strongly encouraged to override this vairable to suit
their needs, and to do so /before/ the package is loaded.")

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

(defun handle--command-execute (commands arg)
  "Run COMMANDS with `command-execute'.
Try next command on `error', passing ARG as `prefix-arg'."
  (when commands
    (let ((first (car commands))
          (rest (cdr commands)))
      (condition-case nil
          (unless
              (let ((prefix-arg arg))
                (message "`handle' running `%s'..." first)
                (command-execute first 'record))
            ;; separate for readability
            (if (or (eq handle-nil 'all) (member first handle-nil)) t
              (message "`handle' ran `%s' unsuccessfully." first)
              (handle--command-execute rest arg)))
        (error (message "`handle' failed to run `%s'." first)
               (handle--command-execute rest arg))))))

(defun handle--get (mode keyword)
  "Return handlers for KEYWORD in MODE."
  (plist-get (alist-get mode handle-alist) keyword))

(defun handle--mode-execute (modes keyword keyword-name arg)
  "Handle KEYWORD for MODES, passing prefix ARG."
  (let ((first (car modes))
        (rest (cdr modes)))
    (when modes
      (message "`handle' handling `%s' %s..." first keyword-name)
      (unless (handle--command-execute (handle--get first keyword) arg)
        (message "Couldn't `handle' `%s' %s." first keyword-name)
        (handle--mode-execute rest keyword keyword-name arg)))))

(dolist (keyword handle-keywords)
  (let ((keyword-name (handle--keyword-name keyword)))
    (defalias (intern (format "handle-%s" keyword-name))
      (lambda (arg)
        (interactive "P")
        (handle--mode-execute
         (reverse (parent-mode-list major-mode))
         keyword keyword-name arg))
      (format "`handle' for %s." keyword-name))))

(provide 'handle)
;;; handle.el ends here
