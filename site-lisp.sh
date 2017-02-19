#!/bin/bash


#
SITE_LISP=$HOME/.emacs.d/site-lisp

# Proof General
PG=${SITE_LISP}/PG

git clone https://github.com/ProofGeneral/PG ${PG}
cd ${PG}
make clean
make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

if [ $? -gt 0 ];then
    echo "Install Proof General failed"
    exit 1
fi

# view-window.el
cd ${SITE_LISP}

VIEW_WINDOW=${SITE_LISP}/view-window
git clone https://github.com/ywata/view-window ${VIEW_WINDOW}


