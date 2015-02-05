emacsfull
=========

Emacs configuration files including compiled binaries.

Compilation
===========

The rebuilding of Emacs binary only works on Mac or Linux using:
./rebuild.sh

TAGS table setup
============

1. Create a symbolic in your home directory to the .emacs file

cd ~
ln -s /nfs/pdx/disks/qnr.disk.1/archive_qreSKIL/emacsfull/emacsfull/.emacs .
ln -s /nfs/pdx/disks/qnr.disk.1/archive_qreSKIL/emacsfull/emacsfull/.emacs.d .

After starting Emacs, type Alt+x and type: 'visit-tags-table', choose
the TAGS table file (wherever it is located)
