# auto-highlight-symbol-hydra

This package was inspired by the Spacemacs AHS Transient State, which only exists for Spacemacs. It has a few changes, however:

* simultaneous display of all three overlay counts
* a variable for hiding the legend
* a variable for hiding the `DISPLAY` range since it doesn't seem useful
* no support for the _b_ (`buffer`) hydra head since it doesn't seem useful

**This package is under active development and has not yet been released to MELPA/ELPA/etc.**

## Dependencies

``` elisp
(package! auto-highlight-symbol)
(package! iedit)
(package! symbol-overlay)
```

## Installation

for now, in Doom:

```elisp
(package! auto-highlight-symbol-hydra :recipe
  '(:host github
    :repo "bgwines/auto-highlight-symbol-hydra"))
```

(once this has been released) On most Emacs distributions, a simple `(require 'auto-highlight-symbol-hydra)` should suffice. For Doom, it is not sufficient to place the package in your `packages.el` -- you need to also add the `require`-statement to your `config.el`. Otherwise, you will need to evaluate a function in the buffer (e.g. `ahs`) for the package to actually be loaded.

## Recommendations

```elisp
(global-set-key (kbd "something") 'ahs)
(setq-default ahs-case-fold-search nil)
(setq-default ahs-default-range 'ahs-range-whole-buffer)
(setq-default ahs-hydra-display-legend nil)
(push 'haskell-mode ahs-modes) ;; e.g. if you want Haskell
```

## TODO

* MC next/prev selection (normal MC is not symbol-aware)
    * or just make my normal MC symbol-aware? Maybe there's a variable
* don't enable AHS mode? Some people don't want that
* hide `DISPLAY` range-mode by default (expose a variable to enable it)
* only rerender what needs to change (is this even possible?)
* simultaneously display `overlay-count' for all three ranges
* fix _d_, _D_, and _R_
* formally list deps per https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Packages.html
* fix "Invalid face reference: t" (see messages buffer when opening hydra)
