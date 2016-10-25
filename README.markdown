# SBLint

SBLint is a linter for Common Lisp source code using SBCL.

## Usage

Just run `sblint` command in your project directory, then it loads all ASD files and runs lint for all CL source files of those.

```
$ sblint -h
Usage:
    $ sblint # runs on all ASD files in current directory
    $ sblint [directories or files...]
```

## Installation

As SBLint requires SBCL, make sure it is installed via [Roswell](https://github.com/roswell/roswell):

```
$ ros install sbcl
# if it's already installed
$ ros use sbcl
```

```
$ ros install fukamachi/sblint
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
