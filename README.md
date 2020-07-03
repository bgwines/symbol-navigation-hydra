# Symbol Navigation Hydra

This package is an Emacs [hydra](https://github.com/abo-abo/hydra) inspired by the [Spacemacs AHS Transient State](https://develop.spacemacs.org/doc/DOCUMENTATION.html#highlight-current-symbol). It is a tool for navigating around and acting upon instances of a symbol (e.g. a function name or a keyword) within a specified range.

-![demo](https://github.com/bgwines/symbol-navigation-hydra/blob/master/snhydra-demo.gif)

There are three available ranges:

1. The function surrounding your cursor
2. The entire buffer
3. The visible portion of the buffer

With your cursor anywhere on a symbol (no need to select it), bring up the hydra, and you have a number of pieces of functionality available:

* Navigation
    * _n_ and _p_ move the cursor to the next and previous instance of the symbol, respectively
                  For example, you may define multiple variables with the same name across multiple
                  functions.
    * _R_ moves the cursor back to where it was when you activated the hydra.
    * _r_ selects another range. There are only three, so at most you need to hit it twice.
    * _z_ vertically centers the current line. This is like the Emacs command `recenter-top-bottom` -
          usually `C-l` - but within the hydra.
* Search
    * _f_ and _g_ search for the symbol in the current directory and Projectile project, respectively.
                  This is not a function of the current range.
* Multi-occurrence
    * _e_ places multiple cursors on every instance of the symbol. This is range-aware.
    * _s_ activates [Helm Swoop](https://github.com/emacsorphanage/helm-swoop) on the symbol. This
          is not range-aware.

## Setup

### Installing (for most Emacs distributions)

Install this package via MELPA.

Alternately, clone a) this repo b) [hydra](https://github.com/abo-abo/hydra) c) [auto-highlight-symbol](https://github.com/mhayashi1120/auto-highlight-symbol-mode) and put them all in your Emacs `load-path`:

``` elisp
(add-to-list 'load-path "~/path/to/cloned/repo/")
...
```

### Installing (for Doom)

```elisp
;; in packages.el
(package! symbol-navigation-hydra :recipe
  '(:host github
    :repo "bgwines/symbol-navigation-hydra"))

```

### Installing (additional functionality)

By default, only the navigation functionality is enabled. The remaining heads are disabled so as to not bloat the user's setup by installing a bunch of dependencies when installing this package. The following packages are optional, but enable features that are disabled otherwise:

* [`iedit`](https://github.com/victorhge/iedit)
* [`helm-swoop`](https://github.com/emacsorphanage/helm-swoop)
* [`helm-ag`](https://github.com/emacsorphanage/helm-ag)
* [`projectile`](https://github.com/bbatsov/projectile)

### Activating

```elisp
;; in your .emacs file or equivalent
(require 'symbol-navigation-hydra)

;; if you're not using Doom, you also need the following
(require 'hydra)
(require 'auto-highlight-symbol)
```

## Recommended Settings

```elisp
;; You'll want a keystroke for bringing up the hydra.
(global-set-key (kbd "something") 'symbol-navigation-hydra-engage-hydra)

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

## Differences with the Spacemacs AHS Hydra

The most prominent difference is simultaneous display of all three overlay counts, instead of just one. The _d_ and _D_ keys are removed since the Auto Highlight Symbol implementation wasn't as accurate as I'd like. The _b_ key is not supported since I didn't find it useful (tracked in a Github issue).

## Licence

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.

## Other

I'd love to hear your feedback. Raise an Github issue here and I'll respond promptly.

Many thanks to [thomasjm](http://github.com/thomasjm) for contributing ideas, code, and debugging to this package.
