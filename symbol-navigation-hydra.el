;;; symbol-navigation-hydra.el --- A hydra for navigation (and more!) -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Brett Wines

;; Author: Brett Wines <bgwines@cs.stanford.edu>
;; Keywords: highlight face match convenience hydra symbol
;; Package-Requires: (auto-highlight-symbol hydra)
;; URL: https://github.com/bgwines/symbol-navigation-hydra
;; Version: 0.0.1

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

(defgroup symbol-navigation-hydra nil
  "The Symbol Navigation Hydra"
  :group 'convenience
  :link `(url-link :tag "Download latest version"
                   ,(eval-when-compile (concat "https://github.com/bgwines/"
                                               "symbol-navigation-hydra/"
                                               "blob/master/symbol-navigation-hydra.el")))
  :link `(url-link :tag "Information"
                   ,(eval-when-compile (concat
                                        "https://github.com/bgwines/"
                                        "symbol-navigation-hydra"))))

(defcustom sn-hydra-display-legend nil
  "*Non-nil means suppress the KEY legend."
  :group 'symbol-navigation-hydra
  :type 'boolean)

(defface ahs-plugin-display-face-dim
  '((t (:foreground "#eeeeee" :background "#3a2303")))
  "Dimmer version of the Display face."
  :group 'symbol-navigation-hydra)
(defvar ahs-plugin-display-face-dim 'ahs-plugin-display-face-dim)

(defface ahs-plugin-whole-buffer-face-dim
  '((t (:foreground "#eeeeee" :background "#182906")))
  "Dimmer version of the Buffer face."
  :group 'symbol-navigation-hydra)
(defvar ahs-plugin-whole-buffer-face-dim 'ahs-plugin-whole-buffer-face-dim)

(defface ahs-plugin-beginning-of-defun-face-dim
  '((t (:foreground "#eeeeee" :background "#0b2d5c")))
  "Dimmer version of the Function face."
  :group 'symbol-navigation-hydra)
(defvar ahs-plugin-beginning-of-defun-face-dim 'ahs-plugin-beginning-of-defun-face-dim)

;; Buffer-local variables
(defvar sn-hydra-point-at-invocation nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; displaying the hydra ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload (autoload 'sn-hydra/body "symbol-navigation-hydra.el" nil nil)
(defhydra sn-hydra (:hint nil)
  "
%s(header)
^ ^       Navigation ^ ^        ^^^^^^Search^%s(header-col-3-extra-spaces)         ^Multi^
^^^^^^^^^^^^---------------------------------%s(header-extra--s)------------------^^^^^^^^^^^^^^^^
_n_^^^^: next        _z_: recenter  _f_: %s(folder-header)      _e_: %s(iedit-header)
_N_/_p_: previous^^  _r_: range     _g_: %s(project-header)     _s_: %s(swoop-header)
_R_: reset       ^^^^_q_: cancel
%s(footer)"
  ("n" move-point-one-symbol-forward)
  ("N" move-point-one-symbol-backward)
  ("p" move-point-one-symbol-backward)
  ("r" ahs-change-range)
  ("R" back-to-start)
  ("z" (progn (recenter-top-bottom) (ahs-highlight-now) (sn-hydra/body)))
  ("e" engage-iedit :exit t)
  ("s" swoop :exit t)
  ("f" (projectile-helm-ag t (thing-at-point 'symbol)) :exit t)
  ("g" (projectile-helm-ag nil (thing-at-point 'symbol)) :exit t)
  ("q" nil :exit t))

(defface disabled-head-face
  '((t (:foreground "#777777")))
  "Face for disabled hydra heads."
  :group 'symbol-navigation-hydra)
(defvar disabled-head-face 'disabled-head-face)

(defun swoop-header ()
  (head-header (is-swoop-enabled) "swoop" (swoop-suffix)))

(defun iedit-header ()
  (head-header (is-iedit-enabled) "iedit" (iedit-suffix)))

(defun folder-header ()
  (head-header (and (is-helm-ag-enabled) (is-projectile-enabled)) "folder" (projectile-suffix)))

(defun project-header ()
  (head-header (and (is-helm-ag-enabled) (is-projectile-enabled)) "project" (projectile-suffix)))

(defun head-header (is-enabled name suffix)
  "Get the string for the head.

`IS-ENABLED' should be a boolean. `NAME' should be the name of the head."
  (if is-enabled name
    (format "%s%s"
            (propertize name 'face disabled-head-face) suffix)))

(defun header-extra--s ()
  "Returns a string with 0 or more '-' characters."
  (let ((col-3 (max (length (iedit-suffix)) (length (swoop-suffix))))
        (col-4 (length (projectile-suffix))))
    (make-string (+ col-3 col-4) ?-)))

(defun header-col-3-extra-spaces ()
  "Returns a string with 0 or more ' ' characters."
  (make-string (length (projectile-suffix)) ? ))

(defun swoop ()
  "Perform helm-swoop on the current symbol."
  (interactive)
  (if (is-swoop-enabled)
      (call-interactively 'helm-swoop)
    (error-not-installed "helm-swoop")))

(defun swoop-suffix ()
  "Indicate disabledness if necessary."
  (head-suffix (is-swoop-enabled)))

(defun iedit-suffix ()
  "Indicate disabledness if necessary."
  (head-suffix (is-iedit-enabled)))

(defun projectile-suffix ()
  "Indicate disabledness if necessary.

This function is an aggregation of two checks because they both guard the same
behavior in the UI."
  (head-suffix (and (is-projectile-enabled) (is-helm-ag-enabled))))


(defun head-suffix (is-enabled)
  "Indicate disabledness if necessary.

`IS-ENABLED' should be a boolean."
  (if is-enabled "" " (?)"))

(defun is-swoop-enabled ()
  "Determine whether the package is loaded."
  (fboundp 'helm-swoop))

(defun is-iedit-enabled ()
  "Determine whether the package is loaded."
  (fboundp 'iedit-mode))

(defun is-projectile-enabled ()
  "Determine whether the package is loaded."
  (fboundp 'projectile-mode))

(defun is-helm-ag-enabled ()
  "Determine whether the package is loaded."
  (fboundp 'helm-do-ag))

(defun header ()
  "This is the user-visible header at the top of the hydra.

It is comprised of
    * The title of the hydra
    * The three plugins, with the inactive ones dimmed, all with overlay counts"
  (let* ((i 0)
         (overlay-count (length ahs-overlay-list))
         (overlay (format "%s" (nth i ahs-overlay-list)))
         (current-overlay (format "%s" ahs-current-overlay))
         )

    (defun is-active (plugin)
      (string= (ahs-get-plugin-prop 'lighter plugin) (ahs-current-plugin-prop 'lighter)))

    (defun darken-plugin-face (face)
        (cond ((eq face ahs-plugin-defalt-face) 'ahs-plugin-display-face-dim)
              ((eq face ahs-plugin-whole-buffer-face) 'ahs-plugin-whole-buffer-face-dim)
              ((eq face ahs-plugin-bod-face) 'ahs-plugin-beginning-of-defun-face-dim)))

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
     (propertize "SN Hydra" 'face `(:box t :weight bold)) "   "
     (plugin-component 'ahs-range-beginning-of-defun) "  "
     (plugin-component 'ahs-range-whole-buffer) "  "
     (plugin-component 'ahs-range-display)
     ))
  )

(defun get-plugin-display-name (plugin)
  "Get the user-visible name for `PLUGIN'."
  (cond
   ((eq plugin 'ahs-range-beginning-of-defun) "Function")
   ((eq plugin 'ahs-range-whole-buffer) "Buffer")
   ((eq plugin 'ahs-range-display) "Display")))

(defun get-plugin-search-range (symbol plugin)
  "Compute the pair of integers within which to search for `SYMBOL'.

The range is dependent on the user-selected range, which is `PLUGIN'.

`PLUGIN' should be one of
  'ahs-range-beginning-of-defun
  'ahs-range-whole-buffer
  'ahs-range-display"
  (let ((before (ahs-get-plugin-prop 'before-search plugin symbol))
        (beg (ahs-get-plugin-prop 'start plugin))
        (end (ahs-get-plugin-prop 'end plugin)))
    (cond ((equal before 'abort) nil)
          ((not (numberp beg)) nil)
          ((not (numberp end)) nil)
          ((> beg end) nil)
          (t (cons beg end)))))

(defun get-occurrences-within-range (symbol search-range)
  "Search for `SYMBOL' in `SEARCH-RANGE'.

`SEARCH-RANGE' should be a pair of integers representing indexes of characters."
  (save-excursion
    (let ((case-fold-search ahs-case-fold-search)
          (regexp (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>" ))
          (beg (car search-range))
          (end (cdr search-range))
          (occurrences 'nil))
      (goto-char end)
      (while (re-search-backward regexp beg t)
        (let* ((symbol-beg (match-beginning 1))
               (symbol-end (match-end 1))
               (tprop (text-properties-at symbol-beg))
               (face (cadr (memq 'face tprop))))
          (unless (ahs-face-p face 'ahs-inhibit-face-list)
            (push (list symbol-beg symbol-end) occurrences))))
      occurrences)))

(defun get-occurrences (plugin)
  "Look up all instances of the currently focused symbol.

These will be instances only within the range specified by
`PLUGIN'. Instances of the symbol in comments or as substrings
are ignored. There are a number of other parameters to this
search (e.g. case-sensitivity); see the auto-highlight-symbol
package.

`PLUGIN' should be one of
    'ahs-range-beginning-of-defun
    'ahs-range-whole-buffer
    'ahs-range-display

The returnvalue is a list of pairs of integers. The integers are indexes
of characters, as in
https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Search.html"
  (let* ((symbol (thing-at-point 'symbol))
         (search-range (get-plugin-search-range symbol plugin)))
    (if symbol
        (if (consp search-range)
            (get-occurrences-within-range symbol search-range)
          nil) ;; couldn't determine the number of occurrences in the range
      nil))) ;; cursor is not on a symbol, so there are 0 occurrences

(defun get-occurrence-index (occurrences)
  "Compute the index of the occurrence of the currently focused symbol.

For example, for the code in this function, the string
\"occurrences\" appears a few (three) times. If the cursor is
on the first of these, this function returns 0, (not 1, since
it is an index).

`PLUGIN' should be one of
    'ahs-range-beginning-of-defun
    'ahs-range-whole-buffer
    'ahs-range-display

`OCCURRENCES' should be the list of all occurrences of the currently focused
symbol. It should be a list of pairs of integers. The integers should be
indexes of characters, as in
https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Search.html"
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
  "For plugin `PLUGIN', computes the overlay counts.

  The first number represents which occurrence of the currently focused symbol
  is selected. The second number represents the total number of occurrences of
  that symbol.

  `PLUGIN' should be one of
      'ahs-range-beginning-of-defun
      'ahs-range-whole-buffer
      'ahs-range-display"
  (let* ((occurrences (get-occurrences plugin))
         (occurrence-index
          (if occurrences
              (+ 1 (get-occurrence-index occurrences))
            0))) ;; if 0 occurrences, don't increment 0
    (format "[%s/%s]" occurrence-index (length occurrences))))

(defun footer ()
  "This is the string to be (optionally) displayed at the bottom of the hydra."
  (if sn-hydra-display-legend
      (let ((guide
            (concat
             "[" (propertize "KEY" 'face 'hydra-face-blue) "] exits state "
             "[" (propertize "KEY" 'face 'hydra-face-red) "] will not exit"
             )))
        (add-face-text-property 0 (length guide) 'italic t guide)
        guide)
    ""))

;;;###autoload
(defun engage-symbol-navigation-hydra ()
  "Trigger the hydra."
  (interactive)
  (setq sn-hydra-point-at-invocation (point))
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    )
  (ahs-highlight-now)
  (sn-hydra/body))

;;;;;;;;;;;
;; heads ;;
;;;;;;;;;;;

(defun back-to-start ()
  "Move `point' to the location it was upon user-initiated hydra invocation."
  (interactive)
  (goto-char sn-hydra-point-at-invocation)
  (ahs-highlight-now)
  (sn-hydra/body)
  )

(defun move-point-one-symbol-forward ()
  "Move to the next occurrence of symbol under point."
  (interactive)
  (move-point-one-symbol t))

(defun move-point-one-symbol-backward ()
  "Move to the previous occurrence of symbol under point."
  (interactive)
  (move-point-one-symbol nil))

(defun move-point-one-symbol (forward)
  "Move to the previous or next occurrence of the symbol under point.

  If `FORWARD' is non-nil, move forwards, otherwise, move backwards."
  (progn
    (ahs-highlight-now)
    (sn-hydra/body)
    (if forward (ahs-forward) (ahs-backward))))

(defun engage-iedit ()
  "Trigger iedit."
  (interactive)
  (if (is-iedit-enabled)
   (progn
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end))
    (ahs-edit-mode t))
   (error-not-installed "iedit")))

(defun projectile-helm-ag (arg query)
  "Run helm-do-ag relative to the project root, searching for `QUERY'.

  Or, with prefix arg `ARG', search relative to the current directory."
  (interactive "P")
  (if (is-projectile-enabled)
      (if (is-helm-ag-enabled)
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
            )
        (error-not-installed "helm-ag"))
    (error-not-installed "projectile")))

(defun error-not-installed (package-name)
  "Raise an error.

`PACKAGE-NAME' should be the name of the package that isn't installed."
  (error (format "%s not installed. See Auto-Highlight Symbol Hydra README.md" package-name)))

(provide 'symbol-navigation-hydra)

;;; symbol-navigation-hydra.el ends here
