#!/bin/bash -x

set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

TMPDIR=/tmp/emacs-init-file-test
# rm -rf "${TMPDIR}"
# mkdir -p "${TMPDIR}"
mkdir -p "${TMPDIR}/quicklisp"
touch "${TMPDIR}/quicklisp/slime-helper.el"

ln -sf $(realpath ../lisp/.emacs) "${TMPDIR}/"

test "${EMACS:-t}" = "t" && EMACS=emacs

env HOME="${TMPDIR}" ${EMACS} --load exit-emacs.el
