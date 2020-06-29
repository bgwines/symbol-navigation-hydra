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
^Navigation^       ^Search^          ^AHS Hydra^        ^Multi^
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; displaying the hydra ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header ()
  (let* ((i 0)
         (overlay-count (length ahs-overlay-list))
         (overlay (format "%s" (nth i ahs-overlay-list)))
         (current-overlay (format "%s" ahs-current-overlay))
         )

    (defun is-active (plugin)
      (string= (ahs-get-plugin-prop 'lighter plugin) (ahs-current-plugin-prop 'lighter)))

    (defun darken-plugin-face (face)
        (cond ((eq face ahs-plugin-defalt-face) '((t (:foreground "#eeeeee" :background "#3a2303"))))
              ((eq face ahs-plugin-whole-buffer-face) '((t (:foreground "#eeeeee" :background "#182906"))))
              ((eq face ahs-plugin-bod-face) '((t (:foreground "#eeeeee" :background "#0b2d5c"))))))

    (defun plugin-color (plugin)
      (let ((face (ahs-get-plugin-prop 'face plugin)))
        (if (is-active plugin) face (darken-plugin-face face))))

    (defun get-active-x/y ()
      (while (not (string= overlay current-overlay))
        (setq i (1+ i))
        (setq overlay (format "%s" (nth i ahs-overlay-list))))
      (format "[%s/%s]" (- overlay-count i) overlay-count))

    (defun plugin-component (plugin)
      (let ((name (propertize (get-plugin-display-name plugin)
                               'face (plugin-color plugin)))
             (x/y (if (is-active plugin) (get-active-x/y) (get-plugin-x/y plugin)))
             )
        (concat name x/y)))

    (concat
     (propertize "AHS Hydra" 'face `(:box t :weight bold)) "  "
     (plugin-component 'ahs-range-beginning-of-defun) "  "
     (plugin-component 'ahs-range-whole-buffer) "  "
     (plugin-component 'ahs-range-display)
     ))
  )

(defun get-plugin-display-name (plugin)
  (cond
   ((eq plugin 'ahs-range-beginning-of-defun) "Function")
   ((eq plugin 'ahs-range-whole-buffer) "Buffer")
   ((eq plugin 'ahs-range-display) "Display")))

(defun get-plugin-search-range (symbol plugin)
  "Prepare for highlight."
  (let ((before (ahs-get-plugin-prop 'before-search plugin symbol))
        (beg (ahs-get-plugin-prop 'start plugin))
        (end (ahs-get-plugin-prop 'end plugin)))
    (cond ((equal before 'abort) nil)
          ((not (numberp beg)) nil)
          ((not (numberp end)) nil)
          ((> beg end) nil)
          (t (cons beg end)))))

(defun get-occurrences-within-range (symbol search-range)
  "Search `SYMBOL' in `SEARCH-RANGE'."
  (save-excursion
    (let ((case-fold-search ahs-case-fold-search)
          (regexp (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>" ))
          (beg (car search-range))
          (end (cdr search-range))
          (occurrences 'nil))
      (goto-char end)
      (while (re-search-backward regexp beg t)
          (push (list (match-beginning 1)
                      (match-end 1)) occurrences))
      occurrences)))

(defun get-occurrences (plugin)
  (let* ((symbol (symbol-at-point))
         (search-range (get-plugin-search-range symbol plugin)))
    (if symbol
        (if (consp search-range)
            (get-occurrences-within-range symbol search-range)
          nil) ;; couldn't determine the number of occurrences in the range
      nil))) ;; cursor is not on a symbol, so there are 0 occurrences

(defun get-occurrence-index (plugin occurrences)
  (let* ((i 0)
         (current-overlay (if ahs-current-overlay
                              (format "%s"
                                      (list
                                       (overlay-start ahs-current-overlay)
                                       (overlay-end ahs-current-overlay)))
                            nil))
         (overlay (format "%s" (nth i occurrences))))
    (while (and (< i (length occurrences))
                (not (string= overlay current-overlay)))
        (setq i (1+ i))
        (setq overlay (format "%s" (nth i occurrences))))
      i
  ))

(defun get-plugin-x/y (plugin)
  (let* ((occurrences (get-occurrences plugin))
         (occurrence-index
          (if occurrences
              (+ 1 (get-occurrence-index plugin occurrences))
            0))) ;; if 0 occurrences, don't increment 0
    (format "[%s/%s]" occurrence-index (length occurrences))))

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

;;;;;;;;;;;
;; heads ;;
;;;;;;;;;;;

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
