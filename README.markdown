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

## Using with reviewdog

[reviewdog](https://github.com/haya14busa/reviewdog) is an automated code review tool which provides a way to see review comments of diff before publishing it.

```
$ sblint | reviewdog -efm="%f:%l:%c: %m" -diff="git diff master"
```

cf. [reviewdog — A code review dog who keeps your codebase healthy – Medium](https://medium.com/@haya14busa/reviewdog-a-code-review-dog-who-keeps-your-codebase-healthy-d957c471938b#.tq51yfpy9)

## Run before 'git commit'

It can be done by using git-hook's pre-commit. Just add the following script to `~/.git/hooks/pre-commit`:

```
#!/bin/sh

# Copy this file to ./.git/hooks/pre-commit

if [ -z "$(which sblint)" ]; then
    echo "WARNING! sblint not found. Given up to run it."
    exit 0
fi

error_count=$(sblint | wc -l | tr -d '[[:space:]]')

if [ "$error_count" -gt "0" ]; then
    echo "Failed to commit because $error_count errors are found by SBLint."
    exit 1
fi
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
