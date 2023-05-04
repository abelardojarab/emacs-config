;;; powerthesaurus-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "powerthesaurus" "powerthesaurus.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from powerthesaurus.el

(autoload 'powerthesaurus-lookup-dwim "powerthesaurus" "\
Wrapper function for general lookup commands.

When called interactively, optional argument ACTION-TYPE corresponds to
the prefix argument passed to this command, which is translated to an action
using `powerthesaurus-prefix-to-action'.  When called programmatically,
its value can either be nil or a symbol that can be possibly returned by
`powerthesaurus-prefix-to-action' (e.g., `action-insert' or `action-display').

The argument passed to QUERY-TYPE should be the same as in
`powerthesaurus-lookup' or nil; in the latter case,
the user will be prompt for a valid value.

\(fn &optional ACTION-TYPE QUERY-TYPE)" t nil)

(autoload 'powerthesaurus-lookup-synonyms-dwim "powerthesaurus" "\
Wrapper function for synonym lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'.

\(fn &optional ACTION-TYPE)" t nil)

(autoload 'powerthesaurus-lookup-antonyms-dwim "powerthesaurus" "\
Wrapper function for antonym lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'.

\(fn &optional ACTION-TYPE)" t nil)

(autoload 'powerthesaurus-lookup-related-dwim "powerthesaurus" "\
Wrapper function for related lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'.

\(fn &optional ACTION-TYPE)" t nil)

(autoload 'powerthesaurus-lookup-definitions-dwim "powerthesaurus" "\
Wrapper function for definition lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'.

\(fn &optional ACTION-TYPE)" t nil)

(autoload 'powerthesaurus-lookup-sentences-dwim "powerthesaurus" "\
Wrapper function for sentence lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'.

\(fn &optional ACTION-TYPE)" t nil)

(autoload 'powerthesaurus-lookup "powerthesaurus" "\
Retrieve the given QUERY-TERM's synonyms, antonyms, etc... online.

Argument QUERY-TYPE specifies the type of query and must be an element of
`powerthesaurus-supported-query-types'.
QUERY-TERM corresponds to the word/term/sentence to look up.

If specified, BEG and END specify the beginning and end positions of
the text in the buffer to be replaced by the selected result.
Particularly, if both BEG and END are both nil, then the results of the queries
will be displayed on a distinct buffer.  If only BEG is specified or
both BEG and END are the same, then the user will be prompted to select one of
the results to be inserted at BEG.  Finally, if both BEG and END are specified
and are different, then the user will be prompted to select a result
which will replace the text between these bounds.

\(fn QUERY-TERM QUERY-TYPE &optional BEG END)" nil nil)

(autoload 'powerthesaurus-lookup-word-dwim "powerthesaurus" "\
Wrapper function for powerthesaurus-lookup-word commands.

If a region is selected use powerthesaurus-lookup-word
if a thing at point is not empty use powerthesaurus-lookup-word-at-point
otherwise as for word using powerthesaurus-lookup-word" t nil)

(autoload 'powerthesaurus-lookup-word-at-point "powerthesaurus" "\
Find word at `WORD-POINT', look it up in powerthesaurs, and replace it.

\(fn WORD-POINT)" t nil)

(autoload 'powerthesaurus-lookup-word "powerthesaurus" "\
Find the given word's synonyms at powerthesaurus.org.

`BEGINNING' and `END' correspond to the selected text with a word to replace.
If there is no selection provided, additional input will be required.
In this case, a selected synonym will be inserted at the point.

\(fn &optional BEGINNING END)" t nil)

(when (require 'hydra nil :noerror) (eval '(defhydra powerthesaurus-hydra (:color blue :hint nil) "\n  Power Thesaurus\n  ^Similarity^           ^Information^\n  ---------------------------------------\n  _s_: Synonyms          _d_: Definitions\n  _a_: Antonyms          _e_: Example Sentences\n  _r_: Related Words\n  _q_: Quit\n  " ("s" powerthesaurus-lookup-synonyms-dwim) ("a" powerthesaurus-lookup-antonyms-dwim) ("r" powerthesaurus-lookup-related-dwim) ("d" powerthesaurus-lookup-definitions-dwim) ("e" powerthesaurus-lookup-sentences-dwim) ("q" nil))))

(when (require 'transient nil :noerror) (eval '(transient-define-prefix powerthesaurus-transient nil "Transient for Power Thesaurus." [["Similarity" ("s" "Synonyms" powerthesaurus-lookup-synonyms-dwim) ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim) ("r" "Related Words" powerthesaurus-lookup-related-dwim)] ["Information" ("d" "Definitions" powerthesaurus-lookup-definitions-dwim) ("e" "Example Sentences" powerthesaurus-lookup-sentences-dwim)]])))

(register-definition-prefixes "powerthesaurus" '("powerthesaurus-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; powerthesaurus-autoloads.el ends here
