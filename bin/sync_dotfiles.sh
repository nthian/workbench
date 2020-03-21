#!/bin/bash

FILES=( .emacs.d .tmux.conf )
EXCLUDE=( .emacs.d/elpa .emacs.d/auto-save-list .emacs.d/network-security.data .emacs.d/.org-id-locations )
DEST=$HOME/repos/.dotfiles

EXCLUDE_FLAGS=""
SEP=""
for f in ${EXCLUDE[@]}
do
    
    EXCLUDE_FLAGS="${EXCLUDE_FLAGS}${SEP}--exclude=${f}"
    SEP=" "
done

for i in ${FILES[@]}
do
    rsync -avp ${EXCLUDE_FLAGS} $HOME/$i $DEST/
done

cd ${DEST}
git add -A . && \
git commit -m "Automated repo sync $(date -Iminutes)." && \
git push
