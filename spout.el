;;; spout.el --- speed keys for outline/foldout modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a "speed key" minor mode for outline/foldout
;; modes, where single keypresses activate commands.  It's inspired by
;; the speed key functionality in org-mode.

;;; Code:

(defcustom spout-keys
  '(("n" outline-next-visible-heading)
    ("p" outline-previous-visible-heading)
    ("u" outline-up-heading)
    ("f" outline-forward-same-level)
    ("b" outline-backward-same-level)
    ("<left>" outline-promote left-char)
    ("<right>" outline-demote right-char)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("<up>" outline-move-subtree-up previous-line)
    ("<down>" outline-move-subtree-down next-line)
    ("s" outline-show-subtree)
    ("d" outline-hide-subtree)
    ("a" outline-show-all)
    ("q" outline-hide-sublevels)
    ("t" outline-hide-body)
    ("k" outline-show-branches)
    ("l" outline-hide-leaves)
    ("i" outline-insert-heading)
    ("o" outline-hide-other)
    ("@" outline-mark-subtree)
    ("z" foldout-zoom-subtree)
    ("x" foldout-exit-fold))

  "List of key bindings for spout minor mode."
  :type '(alist :key-type string :value-type function)
  :group 'spout)

(defvar spout-mode-map (make-sparse-keymap))

(define-minor-mode spout-mode
  "Toggle spout mode."
  :lighter "SPT"
  (if spout-mode
      (spout-initialize)))

(defun spout-initialize ()
  "Set up the key bindings for the spout minor mode."
  ;; (setq spout-mode-map (make-sparse-keymap))
  (dolist (bind spout-keys)
    (apply #'spout--define-key (list spout-mode-map
				     (car bind) (cadr bind) (caddr bind)))))

(defun spout--insert-or (command1 command2)
  "Return lambda function with conditional behavior.

When the lambda function is called interactively, it first checks if
the point is at the beginning of a heading (using `outline-on-heading-p`),
and if so, calls the function represented by COMMAND1 (using `call-interactively`).
Otherwise, it calls the function represented by COMMAND2."
  `(lambda ()
     (interactive)
     (if (and (outline-on-heading-p) (bolp))
	 (call-interactively ',command1)
       (call-interactively ',command2))))

(defun spout--define-key (keymap key command1 command2)
  "In KEYMAP, bind KEY to a conditional command.

This function defines a new function that wraps COMMAND1 and
COMMAND2 with `spout-insert-or`, and then binds that function to
KEY in KEYMAP using `define-key`.

If COMMAND2 is nil, then it defaults to self-insert-command
"
  (let* ((command2 (or command2 #'self-insert-command))
	 (func
	  (defalias (intern (concat "spout--" (symbol-name command1)))
	    (spout--insert-or
	     command1 command2))))
    (define-key keymap (kbd key) func)))


(provide 'spout)
;;; spout.el ends here
