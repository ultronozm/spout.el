;;; spout.el --- Speed keys for outline/foldout modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/spout.el
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, outline

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
;; modes, inspired by the speed key functionality in org-mode.  The
;; idea is that when the point is at the beginning of a section
;; heading, the user can navigate or edit the document using single
;; keypresses.
;;
;; I use it when editing LaTeX.  Here's my basic setup:
;;
;; (use-package latex
;;   :ensure auctex
;;   :bind
;;   (:map LaTeX-mode-map
;;         ("s-n" . outline-next-heading)
;;         ("s-p" . outline-previous-heading)))
;; 
;; (use-package foldout
;;   :ensure t)
;; 
;; (use-package spout
;;   :vc (:url "https://github.com/ultronozm/spout.el.git"
;;             :rev :newest)
;;   :after latex
;;   :hook
;;   (LaTeX-mode . spout-mode)
;;   :config
;;   (require 'texmathp)
;;   (defun LaTeX-skip-verbatim (orig-fun &rest args)
;;     (if (eq major-mode 'latex-mode)
;;         (let ((n 100))
;;           (apply orig-fun args)
;;           (while (and (LaTeX-verbatim-p) (> n 0))
;;             (setq n (- n 1))
;;             (apply orig-fun args)))
;;       (apply orig-fun args)))
;;   (dolist (f '(outline-next-heading
;;                outline-previous-heading
;;                outline-up-heading
;;                outline-forward-same-level
;;                outline-backward-same-level))
;;     (advice-add f :around #'LaTeX-skip-verbatim)))
;;
;; The idea is that you can always navigate to a nearby section
;; heading using "s-n" or "s-p", after which you can use individual
;; keys to navigate.
;;
;; I suppose something similar could also be achieved using
;; `repeat-mode' (see for instance
;; https://karthinks.com/software/a-consistent-structural-editing-interface/).
;; See also the `latex-extra' package.

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
  :lighter nil
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
When the lambda function is called interactively, it first checks
if the point is at the beginning of a heading (using
`outline-on-heading-p'), and if so, calls the function
represented by COMMAND1 (using `call-interactively').  Otherwise,
it calls the function represented by COMMAND2."
  `(lambda ()
     (interactive)
     (if (and (outline-on-heading-p) (bolp))
	 (call-interactively ',command1)
       (call-interactively ',command2))))

(defun spout--define-key (keymap key command1 command2)
  "In KEYMAP, bind KEY to a conditional command.
This function defines a new function that wraps COMMAND1 and
COMMAND2 with `spout-insert-or', and then binds that function to
KEY in KEYMAP using `define-key'.

If COMMAND2 is nil, then it defaults to `'self-insert-command'."
  (let* ((command2 (or command2 #'self-insert-command))
	 (func
	  (defalias (intern (concat "spout--" (symbol-name command1)))
	    (spout--insert-or
	     command1 command2))))
    (define-key keymap (kbd key) func)))


(provide 'spout)
;;; spout.el ends here
