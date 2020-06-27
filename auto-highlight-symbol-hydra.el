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


(defhydra hydra-auto-symbol-highlight (:hint nil)
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
      (setq name (propertize (plugin-name plugin) 'face (plugin-color plugin)))

      ;; `ahs-search-symbol'?
      (setq locator (if (is-active plugin) (get-active-locator) "[?/?]"))
      (concat name locator))

    (concat
     (propertize "AHS Hydra" 'face `(:box t :weight bold)) "  "
     (plugin-component "HSD") "  "
     (plugin-component "HSA") "  "
     (plugin-component "HS")
     )
    )
  )

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

(defun ahs ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    )
  (ahs-highlight-now)
  (hydra-auto-symbol-highlight/body))

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
        (hydra-auto-symbol-highlight/body)
        (ahs-forward))
    (progn
      (ahs-highlight-now)
      (hydra-auto-symbol-highlight/body)
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
  (projectile-helm-ag current-folder (thing-at-point 'symbol))
)

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
