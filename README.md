# auto-highlight-symbol-hydra
The Spacemacs Auto-Highlight Symbol Hydra, ported over to vanilla Emacs

## Recommendations

```elisp
(setq-default ahs-case-fold-search nil)
(setq-default ahs-default-range 'ahs-range-whole-buffer)
(push 'haskell-mode ahs-modes) ;; e.g. if you want Haskell

```

## TODO

### do the `defconst`stuff or whatever to actually make it a package

### hide `DISPLAY` range-mode by default

expose a variable to enable it

is tehre something analogous to save-excursion for an existing variable? There's probably some pattern to support the customization f this in emacs and I should just like use it my guy

### only rerender what needs to change

### expose variable to hide legend

recommend hiding by default, but the default behavior should be to show it

### simultaneously display `overlay-count' for all three ranges

no idea how to do this

### Fix _d_, _D_, and _R_
