#!/bin/sh

GOPATH=${GOPATH:-$HOME/go}
REVIEWDOG_VERSION=${REVIEWDOG_VERSION:-0.9.0}

install_reviewdog () {
    mkdir -p "$GOPATH/bin"
    curl -L https://github.com/haya14busa/reviewdog/releases/download/$REVIEWDOG_VERSION/reviewdog_linux_386 > "$GOPATH/bin/reviewdog"
    chmod u+x "$GOPATH/bin/reviewdog"
}

install_sblint () {
    ros install sblint
}

run_sblint () {
    ~/.roswell/bin/sblint | $GOPATH/bin/reviewdog -efm="%f:%l:%c: %m" -diff="git diff master" -ci="$1"
}

case "$LISP" in
    sbcl*)
        if [ "$CI_PULL_REQUEST" ]; then
            ci="circle-ci"
        elif [ "$TRAVIS_PULL_REQUEST" ]; then
            ci="travis"
        elif [ "$DRONE_PULL_REQUEST" ]; then
            ci="droneio"
        elif [ "CI_PULL_REQUEST" ]; then
            ci="common"
        fi

        if [ "$ci" ]; then
            install_reviewdog
            install_sblint
            run_sblint "$ci"
        fi
        ;;
    *)
        # ignore
        ;;
esac
