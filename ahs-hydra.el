;;; ~/doom-config/ahs-hydra.el -*- lexical-binding: t; -*-

(defhydra hydra-auto-symbol-highlight (:hint nil)
  "
%s(header)
^Navigation^       ^Search^          ^AHS Hydra^        ^Magic^
----------------------------------------------------------
_n_: next          _f_: folder       _r_: range         _e_: iedit
_N_/_p_: previous    _g_: project      _R_: reset         _s_: swoop
_d_: prevdef       ^ ^               _q_: cancel
_D_: nextdef
%s(footer)
"
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
         (plugin (cond ((string= plighter "HS")  "Display")
                       ((string= plighter "HSA") "Buffer")
                       ((string= plighter "HSD") "Function")))
         (face (cond ((string= plighter "HS")  ahs-plugin-defalt-face)
                     ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                     ((string= plighter "HSD") ahs-plugin-bod-face)))
         )

    (while (not (string= overlay current-overlay))
      (setq i (1+ i))
      (setq overlay (format "%s" (nth i ahs-overlay-list))))
    (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
           (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
      (concat
       (propertize (format " %s " plugin) 'face face)
       (propertize (format " %s%s " x/y hidden) 'face
                   `(:foreground "#ffffff" :background "#000000")
                   )
       )
      )
    )

    (setq DEFUN t)
  )

(defun footer ()
  (setq guide
        (concat
         "[" (propertize "KEY" 'face 'hydra-face-blue) "] exits state "
          "[" (propertize "KEY" 'face 'hydra-face-red) "] will not exit"
          ))
  (add-face-text-property 0 (length guide) 'italic t guide)
  guide
  )

(defun ahs ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (setq x 5)
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    )
  (ahs-highlight-now)
  (hydra-auto-symbol-highlight/body))
(global-set-key (kbd "M-t") 'ahs)

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
