# auto-highlight-symbol-hydra

This package was inspired by the Spacemacs AHS Transient State, which didn't
exist for non-Spacemacs Emacses. It has a few changes, however:

* simultaneous display of all three overlay counts
* a variable for hiding the legend
* a variable for hiding the `DISPLAY` range since it doesn't seem useful
* no support for the _b_ (`buffer`) hydra head since it doesn't seem useful


## Requirements

``` elisp
(package! auto-highlight-symbol)
(package! iedit)
(package! symbol-overlay)
```

## Recommendations

```elisp
(setq-default ahs-case-fold-search nil)
(setq-default ahs-default-range 'ahs-range-whole-buffer)
(setq-default ahs-hydra-display-legend nil)
(push 'haskell-mode ahs-modes) ;; e.g. if you want Haskell

```

## TODO

* hide `DISPLAY` range-mode by default (expose a variable to enable it)
* only rerender what needs to change (is this even possible?)
* simultaneously display `overlay-count' for all three ranges
* fix _d_, _D_, and _R_
