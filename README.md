[![Build Status](https://travis-ci.com/jcs090218/alt-codes.svg?branch=master)](https://travis-ci.com/jcs090218/alt-codes)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# alt-codes
> Insert alt codes using meta key.

Like many other editors you can insert [Alt-Codes](https://www.alt-codes.net/) 
using alt key. Notice this will only works with numpad keys.


* *P.S. Inspired by `Notepad++` presets.*
* *P.S. Inspired by `VSCode` presets.*


## Usage

### Insertion
If you want to enable insertion using alt key then you need to 
enable this.

```el
(alt-codes-mode t)
;; or enabled globally.
(global-alt-codes-mode t)
```

Or you can call this function to insert it manually.

```
M-x alt-codes-insert
```


### Table
Print out all the Alt-Codes.
```
M-x alt-codes-table
```


## Contribution

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
