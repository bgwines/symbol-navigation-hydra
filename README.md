# auto-highlight-symbol-hydra

This package is an Emacs [hydra](https://github.com/abo-abo/hydra) inspired by the [Spacemacs AHS Transient State](https://develop.spacemacs.org/doc/DOCUMENTATION.html#highlight-current-symbol). **This package is under active development and has not yet been released to MELPA/ELPA/etc.**

The most prominent difference is simultaneous display of all three overlay counts, instead of just one:

![overlays](https://github.com/bgwines/auto-highlight-symbol-hydra/blob/master/simultaneous-overlays.png)

## Installation

For most Emacs distributions, clone this repo and put it in your Emacs `load-path`. Then, in your config:

```elisp
(require 'auto-highlight-symbol-hydra)
```

For Doom:

```elisp
;; in packages.el
(package! auto-highlight-symbol-hydra :recipe
  '(:host github
    :repo "bgwines/auto-highlight-symbol-hydra"))

;; in config.el (yes, you do need this -- otherwise, you will need to evaluate
;;               a function from the package in the buffer (e.g.
;;               `engage-auto-highlight-symbol-hydra`) in order for the package
;;               to actually be loaded. This seems to be something specific to
;;               hydras are loaded.)
(require 'auto-highlight-symbol-hydra)
```

## Recommended Settings

```elisp
;; You'll want a keystroke for bringing up the hydra.
(global-set-key (kbd "something") 'engage-auto-highlight-symbol-hydra)

;; Be case-sensitive, since you are probably using this for code.
(setq-default ahs-case-fold-search nil)

;; Personal preference -- set the default "range" of operation to be the entire buffer.
(setq-default ahs-default-range 'ahs-range-whole-buffer)

;; Disable symbol highlighting when the hydra is not active (yes, this is a hack 😅).
(setq-default ahs-idle-interval 999999999.0)

;; Many - but not all - languages are supported by default. You'll probably get pretty good
;; behavior by just opting one in if it's not already there.
(push 'haskell-mode ahs-modes) ;; e.g. if you want Haskell
```

If this is your first time using an Emacs hydra, you might want to set

```elisp
;; Once you know what the colors mean, you probably won't need this anymore.
(setq-default ahs-hydra-display-legend t)
```

You may also wish to customize some other variables from the [Auto Highlight Symbol package](https://github.com/mhayashi1120/auto-highlight-symbol-mode) itself.

## TODO

* P1
    * In Doom Emacs, some non-focused windows flicker during hydra navigation.
      This appears to be a bug with the hydra package, as it occurs even with
      a minimal hydra for adjusting text size.
    * Expose a variable to hide the `DISPLAY` range.
* P2
    * Fancier multi-cursor support (`iedit` only operates on all instances within
      the range).
    * The Spacemacs AHS hydra has another kind of search in addition to "files"
      and "project": "buffers", which searches open buffers. This isn't yet
      supported here since I didn't think it would be too useful.

## Feedback

I'd love to hear your feedback. Raise an Github issue here and I'll respond promptly.
