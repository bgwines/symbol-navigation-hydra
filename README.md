# auto-highlight-symbol-hydra

This package is an Emacs [hydra](https://github.com/abo-abo/hydra) inspired by the [Spacemacs AHS Transient State](https://develop.spacemacs.org/doc/DOCUMENTATION.html#highlight-current-symbol). **This package is under active development and has not yet been released to MELPA/ELPA/etc.**

The most prominent difference is simultaneous display of all three overlay counts, instead of just one:

![overlays](https://github.com/bgwines/auto-highlight-symbol-hydra/blob/master/simultaneous-overlays.png)

## Installation

for now, in Doom:

```elisp
(package! auto-highlight-symbol-hydra :recipe
  '(:host github
    :repo "bgwines/auto-highlight-symbol-hydra"))
```

(once this has been released) On most Emacs distributions, a simple `(require 'auto-highlight-symbol-hydra)` should suffice. For Doom, it is not sufficient to place the package in your `packages.el` -- you need to also add the `require`-statement to your `config.el`. Otherwise, you will need to evaluate a function in the buffer (e.g. `ahs`) for the package to actually be loaded.

## Recommended Settings

```elisp
;; You'll want a keystroke for bringing up the hydra.
(global-set-key (kbd "something") 'ahs)

;; Be case-sensitive, since you are probably using this for code.
(setq-default ahs-case-fold-search nil)

;; Personal preference
(setq-default ahs-default-range 'ahs-range-whole-buffer)

;; Disable symbol highlighting when the hydra is not active (yes, this is a hack 😅)
(setq-default ahs-idle-interval 999999999.0)

;; Many - but not all - languages are supported by default. You'll probably get pretty good
;; behavior by just opting one in if it's not already there.
(push 'haskell-mode ahs-modes) ;; e.g. if you want Haskell
```

If this is your first time using an Emacs hydra, you may also wish to set

```elisp
;; Once you know what the colors mean, you probably won't need this anymore.
(setq-default ahs-hydra-display-legend t)
```

You may also wish to customize some other variables from the [Auto Highlight Symbol package](https://github.com/mhayashi1120/auto-highlight-symbol-mode) itself.

## TODO

* p0
    * disable AHS idle highlighting
* p1
    * main buffer flickers during _p_ and _n_ (is this even fixable?)
    * hide `DISPLAY` range-mode by default (expose a variable to enable it)
    * fix _d_, _D_, and _R_
    * document code
* p2
    * support MC next/prev selection? (normal MC is not symbol-aware) (or just make my normal MC symbol-aware? Maybe there's a variable)
    * support _b_ (buffer) search

## Feedback

I'd love to hear your feedback. Raise an Github issue here and I'll respond promptly.
