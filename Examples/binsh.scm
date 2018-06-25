#!/bin/sh
":";exec mzscheme -qr $0 "$@"
;; $Id: binsh.scm,v 1.2 2014-10-31 17:35:08-07 - - $

;;
;; This file shows how to execute mzscheme using the shell
;; Normally a hashbang would refer to the actual binary.
;; This file is then bilingual.  When seen by the shell,
;; it execs mzscheme which must be somewhere in your path.
;; when seen by Scheme, the semi-colon in the exec line
;; appears as a comment.
;;

(printf "~a~n" "Hello, world!")
(printf "~s~n" (current-command-line-arguments))

;;TEST: binsh.scm
