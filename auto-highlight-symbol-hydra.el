;;; auto-highlight-symbol-hydra.el --- A hydra for `auto-highlight-symbol' -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Brett Wines

;; Author: Brett Wines <bgwines@cs.stanford.edu>
;; Keywords: highlight face match convenience hydra symbol
;; URL: https://github.com/bgwines/auto-highlight-symbol-hydra/blob/master/auto-highlight-symbol-hydra.el
;; Version: 0.0.4

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package was inspired by the Spacemacs AHS Transient State, which didn't
;; exist for non-Spacemacs <gollum voice>Emacses</gollum voice>. It has a few
;; changes, however:
;;     * simultaneous display of all three overlay counts
;;     * a variable for hiding the legend
;;     * a variable for hiding the `DISPLAY' range since it doesn't seem useful
;;     * no support for the _b_ (`buffer') hydra head since it doesn't seem useful
;;
;; Happy coding! ^_^

;;; Code:

(defgroup auto-highlight-symbol-hydra nil
  "Automatic highlighting current symbol hydra"
  :group 'convenience
  :link `(url-link :tag "Download latest version"
                   ,(eval-when-compile (concat "https://github.com/bgwines/"
                                               "auto-highlight-symbol-hydra/"
                                               "blob/master/auto-highlight-symbol-hydra.el")))
  :link `(url-link :tag "Information"
                   ,(eval-when-compile (concat
                                        "https://github.com/bgwines/"
                                        "auto-highlight-symbol-hydra"))))

(defcustom ahs-hydra-display-legend nil
  "*Non-nil means suppress the KEY legend."
  :group 'auto-highlight-symbol-hydra
  :type 'boolean)


;;;###autoload (autoload 'ahs-hydra/body "auto-highlight-symbol-hydra.el" nil nil)
(defhydra ahs-hydra (:hint nil)
  "
%s(header)
^Navigation^       ^Search^          ^AHS Hydra^        ^Magic^
----------------------------------------------------------
_n_: next          _f_: folder       _r_: range         _e_: iedit
_N_/_p_: previous    _g_: project      _R_: reset         _s_: swoop
_d_: prevdef       ^ ^               _z_: recenter
_D_: nextdef       ^ ^               _q_: cancel
%s(footer)"
  ("n" quick-ahs-forward)
  ("N" quick-ahs-backward)
  ("p" quick-ahs-backward)
  ("d" ahs-forward-definition)
  ("D" ahs-backward-definition)
  ("r" ahs-change-range)
  ("R" ahs-back-to-start)
  ("z" (progn (recenter-top-bottom) (ahs)))
  ("e" ahs-to-iedit :exit t)
  ("s" (call-interactively 'helm-swoop) :exit t)
  ("f" (helm-projectile-ag-the-selection t) :exit t)
  ("g" (helm-projectile-ag-the-selection nil) :exit t)
  ("q" nil))

(defun header ()
  (let* ((i 0)
         (overlay-count (length ahs-overlay-list))
         (overlay (format "%s" (nth i ahs-overlay-list)))
         (current-overlay (format "%s" ahs-current-overlay))
         (st (ahs-stat))
         (plighter (ahs-current-plugin-prop 'lighter))
         (ahs-plugin-defalt-face-inactive
          '((t (:foreground "#eeeeee" :background "#3a2303"))))
         (ahs-plugin-whole-buffer-face-inactive
          '((t (:foreground "#eeeeee" :background "#182906"))))
         (ahs-plugin-bod-face-inactive
          '((t (:foreground "#eeeeee" :background "#0b2d5c"))))
         )

    (defun plugin-name (plugin)
      (cond ((string= plugin "HS")  "Display")
            ((string= plugin "HSA") "Buffer")
            ((string= plugin "HSD") "Function")))

    (defun is-active (plugin)
      (string= plugin plighter))

    (defun plugin-color (plugin)
      (if (is-active plugin)
          (cond ((string= plugin "HS")  ahs-plugin-defalt-face)
                ((string= plugin "HSA") ahs-plugin-whole-buffer-face)
                ((string= plugin "HSD") ahs-plugin-bod-face))
        (cond ((string= plugin "HS")  ahs-plugin-defalt-face-inactive)
              ((string= plugin "HSA") ahs-plugin-whole-buffer-face-inactive)
              ((string= plugin "HSD") ahs-plugin-bod-face-inactive))))

    (defun get-active-locator ()
      (while (not (string= overlay current-overlay))
        (setq i (1+ i))
        (setq overlay (format "%s" (nth i ahs-overlay-list))))
      (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count)))
        x/y))

    (defun plugin-component (plugin)
      (let* ((name (propertize (plugin-name plugin) 'face (plugin-color plugin)))
             (n-occurrences (get-n-occurences plugin))
             (locator (if (is-active plugin) (get-active-locator)
                        (format "[?/%s]" n-occurrences))))
        (concat name locator)))

    (concat
     (propertize "AHS Hydra" 'face `(:box t :weight bold)) "  "
     (plugin-component "HSD") "  "
     (plugin-component "HSA") "  "
     (plugin-component "HS")
     )
    )
  )

;;;;;;;;;;;;;;

(defun lighter-to-plugin-name (lighter)
  (cond ((string= lighter "HS") "display-area")
        ((string= lighter "HSA") "whole-buffer")
        ((string= lighter "HSD") "beginning-of-defun"))
  )

(defun lighter-to-plugin (lighter)
  (intern-soft (format "ahs-range-%s" (lighter-to-plugin-name lighter))))

(defun my-ahs-prop (lighter prop &optional arg)
  "Return value of the `PROP' property of the associated plugin."
  (ahs-get-plugin-prop prop (lighter-to-plugin lighter) arg))

(defun my-ahs-prepare-highlight (symbol plugin)
  "Prepare for highlight."
  (let ((before (my-ahs-prop plugin 'before-search symbol))
        (beg (my-ahs-prop plugin 'start))
        (end (my-ahs-prop plugin 'end)))
    (cond ((equal before 'abort) nil)
          ((not (numberp beg)) nil)
          ((not (numberp end)) nil)
          ((> beg end) nil)
          (t (cons beg end)))))

(defun my-ahs-search-symbol (symbol search-range)
  "Search `SYMBOL' in `SEARCH-RANGE'."
  (save-excursion
    (let ((case-fold-search ahs-case-fold-search)
          (regexp (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>" ))
          (beg (car search-range))
          (end (cdr search-range)))
      (goto-char end)
      (setq i 0)
      (while (re-search-backward regexp beg t)
        (setq i (+ i 1)))
      i
      )))

(defun get-n-occurences (plugin)
  (let* ((symbol (symbol-at-point))
         (search-range (my-ahs-prepare-highlight symbol plugin)))
    (if symbol
        (if (consp search-range)
            (my-ahs-search-symbol symbol search-range)
          ;; couldn't determine the number of occurrences in the range
          "?")
      ;; cursor is not on a symbol, so there are 0 occurrences
      0)))

;;;;;;;;;;;;;;

(defun footer ()
  (if ahs-hydra-display-legend
      (progn(setq guide
            (concat
             "[" (propertize "KEY" 'face 'hydra-face-blue) "] exits state "
             "[" (propertize "KEY" 'face 'hydra-face-red) "] will not exit"
             ))
            (add-face-text-property 0 (length guide) 'italic t guide)
            guide)
    ""
    )
  )

;;;###autoload
(defun ahs ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    )
  (ahs-highlight-now)
  (ahs-hydra/body))

(defun quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with `auto-highlight-symbol'"
  (interactive)
  (quick-ahs-move t))

(defun quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with `auto-highlight-symbol'"
  (interactive)
  (quick-ahs-move nil))

(defun quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with `auto-highlight-symbol'"
  (if forward
      (progn
        (ahs-highlight-now)
        (ahs-hydra/body)
        (ahs-forward))
    (progn
      (ahs-highlight-now)
      (ahs-hydra/body)
      (ahs-backward))))

(defun ahs-to-iedit ()
  "Trigger iedit from ahs."
  (interactive)
   (progn
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (ahs-edit-mode t))

(defun helm-projectile-ag-the-selection (current-folder)
  "helm-projectile-ag the selection

  if current-folder is t, then searches the current folder. Otherwise, searches
  from the projectile directory root"
  (interactive)
  (projectile-helm-ag current-folder (symbol-at-point))
)

(defun symbol-at-point () (thing-at-point 'symbol))

(defun projectile-helm-ag (arg query)
  "Run helm-do-ag relative to the project root.  Or, with prefix arg ARG, relative to the current directory."
  (interactive "P")
  (if arg
      (progn
        ;; Have to kill the prefix arg so it doesn't get forwarded
        ;; and screw up helm-do-ag
        (set-variable 'current-prefix-arg nil)

        (if dired-directory
            (helm-do-ag dired-directory nil query)
          (helm-do-ag (file-name-directory (buffer-file-name)) nil query)
          )
        )
    (helm-do-ag (projectile-project-root) nil query)
    ))

(provide 'auto-highlight-symbol-hydra)

;;; auto-highlight-symbol-hydra.el ends here
