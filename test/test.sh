#!/bin/bash -x

set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

TMPDIR=/tmp/emacs-init-file-test
# rm -rf "${TMPDIR}"
mkdir -p "${TMPDIR}"

ln -sf $(realpath ../lisp/.emacs) "${TMPDIR}/"


test "${EMACS:-t}" = "t" && EMACS=emacs

env HOME="${TMPDIR}" ${EMACS} --load exit-emacs.el
