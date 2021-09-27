[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/alt-codes-badge.svg)](https://melpa.org/#/alt-codes)
[![MELPA Stable](https://stable.melpa.org/packages/alt-codes-badge.svg)](https://stable.melpa.org/#/alt-codes)

# alt-codes
> Insert alt codes using meta key.

[![CI](https://github.com/jcs-elpa/alt-codes/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/alt-codes/actions/workflows/test.yml)

Like many other editors you can insert [Alt-Codes](https://www.alt-codes.net/) 
using alt key. Notice this will only works with numpad keys.

* *P.S. Inspired by `Notepad++` preset behaviour.*
* *P.S. Inspired by `VSCode` preset behaviour.*

## Usage

### Insertion

If you want to enable insertion using alt key then you need to enable this.

```el
(alt-codes-mode t)
;; or enabled globally.
(global-alt-codes-mode t)
```

Or you can call this function to insert it manually.

```
M-x alt-codes-insert
```

## Contribution

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
