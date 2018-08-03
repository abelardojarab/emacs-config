;;; spice-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "spice-mode" "spice-mode.el" (0 0 0 0))
;;; Generated autoloads from spice-mode.el

(defvar spice-mode-hook nil "\
*List of hook functions run by `spice-mode' (see `run-hooks').")

(custom-autoload 'spice-mode-hook "spice-mode" t)

(defvar spice-standard '(spice2g6 (hspice eldo eldorf eldovloga fasthenry)) "\
*Spice standards used.
Basic standard:
  Spice2g6    : Original Berkeley Spice (leave this always on!)
Additional standards:
  Hspice (TM) : Commercial Spice, formerly Meta Software, now owned by Synopsys
  Eldo (TM)   : Commercial Spice, formerly Anacad, now owned by Mentor Graphics
  EldoRf (TM) : RF Steady State analyses of Eldo (also turn on Eldo!)
  EldoVlogA   : Verilog-A extensions to Eldo netlist language (also turn on Eldo!)
  FastHenry   : Multipole-accelerated inductance analysis program from MIT
  Layla       : KULeuven LAYLA (layout synthesis) extensions to Spice format
  Mondriaan   : KULeuven MONDRIAAN (layout synthesis) extensions (also turn on Layla!)
  DracCDL (TM): Dracula CDL extensions (Cadence LOGLVS netlists)
  Spectre (TM): Spice compatibility of Spectre language (simulator language=spice decks)
")

(custom-autoload 'spice-standard "spice-mode" nil)

(defvar spice-imenu-add-to-menubar t "\
*Spice mode adds imenu (Index) item to menubar")

(custom-autoload 'spice-imenu-add-to-menubar "spice-mode" nil)

(defvar spice-show-describe-mode nil "\
*Spice mode runs `describe-mode' once at start of spice-mode")

(custom-autoload 'spice-show-describe-mode "spice-mode" t)

(defvar spice-echo-intro t "\
*Spice mode echos introductory message on entry to spice-mode")

(custom-autoload 'spice-echo-intro "spice-mode" t)

(defvar spice-initialize-empty-file nil "\
*Spice initialize empty/new file setting")

(custom-autoload 'spice-initialize-empty-file "spice-mode" t)

(defvar spice-initialize-template-file "~/.spice-default" "\
*File containing the default header that is inserted when opening
an empty file (ie. a new file), see also `spice-initialize-empty-file'")

(custom-autoload 'spice-initialize-template-file "spice-mode" t)

(defvar spice-default-header nil "\
*Default header for new Spice netlists, see also `spice-initialize-empty-file'")

(custom-autoload 'spice-default-header "spice-mode" t)

(defvar spice-initialize-file-function 'spice-initialize-empty-file "\
*Optional initialize function for empty/new files, see also
`spice-initialize-empty-file'. If a different function is specified it
should insert a default header/template in the current buffer. This
function should check which submode is in use with `spice-standard-p'
and adapt its output accordingly. It may also use the `spice-default-header'
variable and insert its contents into the buffer.")

(custom-autoload 'spice-initialize-file-function "spice-mode" t)

(defvar spice-simulator nil "\
*Spice command, used when compiling buffer with `compile-mode',
see also `spice-simulator-switches'.")

(custom-autoload 'spice-simulator "spice-mode" t)

(defvar spice-simulator-switches "" "\
*Spice command switches, used when compiling buffer with `compile-mode',
see also `spice-simulator'.")

(custom-autoload 'spice-simulator-switches "spice-mode" t)

(defvar spice-waveform-viewer nil "\
*Spice command, used when starting waveform viewer,
see also `spice-waveform-viewer-switches'.")

(custom-autoload 'spice-waveform-viewer "spice-mode" t)

(defvar spice-waveform-viewer-switches "" "\
*Spice waveform viewer command switches,
see also `spice-waveform-viewer'.")

(custom-autoload 'spice-waveform-viewer-switches "spice-mode" t)

(defvar spice-shell (if (memq system-type '(ms-dos emx windows-nt)) shell-file-name "/bin/sh") "\
*Name of shell used to parse spice commands.")

(custom-autoload 'spice-shell "spice-mode" t)

(defvar spice-shell-command-option (cond ((memq system-type '(ms-dos emx windows-nt)) (cond ((boundp 'shell-command-option) shell-command-option) ((boundp 'shell-command-switch) shell-command-switch) (t "/c"))) (t "-c")) "\
*Shell argument indicating that next argument is the command.")

(custom-autoload 'spice-shell-command-option "spice-mode" t)

(defvar spice-hide-line-prefix '(eval-after-load "newcomment" '(concat (regexp-quote (concat comment-start (if (boundp 'comment-padding) (if (integerp comment-padding) (make-string comment-padding 32) comment-padding) " "))) "[a-z\\*!$0-9+\\.]")) "\
*Regexp string describing lines that are commented out and will be
hidden. The regexp is matched to the beginning of a line, the ^ is
added automatically. The initialization of this variable is handled
in `spice-hide-init', which is after the setting of `comment-start'
and `comment-padding' variables.")

(custom-autoload 'spice-hide-line-prefix "spice-mode" t)

(defvar spice-auto-hide-comments nil "\
*Boolean indicating automatic hiding of all commented regions at load time.")

(custom-autoload 'spice-auto-hide-comments "spice-mode" t)

(defvar spice-section-alist '(("Libraries" "LIBRARIES" nil) ("Netlist" "NETLIST" nil) ("Main Circuit" "MAIN CIRCUIT" nil) ("Options" "SIMULATION OPTIONS" nil) ("Supplies" "SUPPLIES/REFERENCES" nil) ("Input Signals" "INPUT SIGNALS" nil) ("DC Analysis" "DC ANALYSIS" nil) ("AC Analysis" "AC ANALYSIS" nil) ("Transient Analysis" "TRANSIENT ANALYSIS" nil)) "\
*List of valid sections in a Spice file and their options.
Each list entry specifies the following items for a section:
Section:
  Section Name     : name used in to select/create find section, make this
                     name short and descriptive.
  Section String   : string used in file to start section (usually all
                     uppercase variant of name).
  Extra switches   : extra switches for a section, unspecified for now.")

(custom-autoload 'spice-section-alist "spice-mode" nil)

(defvar spice-highlight-keywords t "\
*Non-nil means highlight SPICE keywords and other standardized words.
The following faces are used:
  `spice-title-face'		: title (first line in a spice file)
  `spice-doc-face'		: doc strings
  `spice-analysis-face'		: analyses
  `spice-instance-name-face'	: instance/element names
  `spice-model-name-face'	: subckt model names
  `spice-layla-function-name-face': layla function names
  `spice-include-file-face'	: include files and libraries
  `font-lock-keyword-face'	: keywords
  `font-lock-warning-face'	: warnings
  `font-lock-comment-face'	: comment
  `font-lock-function-name-face': subcircuit references / names of objects
  `font-lock-type-face'		: types
  `font-lock-string-face'	: strings & include files
  `font-lock-constant-face'	: simulator's options
  `font-lock-variable-name-face': names of .param's & variables
NOTE: Activate the new setting in a spice buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").")

(custom-autoload 'spice-highlight-keywords "spice-mode" t)

(defvar spice-output-filename-alist '((eldo (concat (file-name-sans-extension (buffer-file-name)) ".chi")) (hspice (concat (file-name-sans-extension (buffer-file-name)) ".lis")) (hspice (concat (file-name-sans-extension (buffer-file-name)) ".spout")) (hspice (concat (file-name-sans-extension (buffer-file-name)) ".hspout")) (spice2g6 (concat (file-name-sans-extension (buffer-file-name)) ".out"))) "\
*List of valid output names depending on selected spice standard:
  Spice Standard   : one of spice2g6, hspice, eldo or layla
  Expression       : expression calculating the output filename
")

(custom-autoload 'spice-output-filename-alist "spice-mode" t)

(autoload 'spice-mode "spice-mode" "\
Major mode for editing spice decks in (X)Emacs.

Entry to Spice mode calls the value of the variable `spice-mode-hook'
with no args, if that value is non-nil after initialization is finished.

Usage & Features:
-----------------

   - Comprehensive menu

   - Highlighting of (extended) SPICE syntax, with (limited) ERROR notification
     Please setup spice-mode to recognize the correct syntax through
     customization of the `spice-standard' variable. You can use the menu
     entry Spice->Customize->Spice Standard to do this interactively.

   - Template insertion (abbrev/electrification) for many spice constructs,
     two alternatives are available: Abbrev minor mode and `tempo-complete-tag'
       + Abbrevs can be turned on and off via the Settings submenu.
         To see the available abbrevs, use `M-x list-abbrevs' or use the menu
         after enabling abbrev minor mode. To find out what key sequence
         triggers an expand do 'C-h w expand-abbrev'.
       + `tempo-complete-tag' is bound to <tab> - for example, type 'M<tab>'
         at the beginning of a line and you will be prompted with a complete
         Mosfet template. Most tags are pretty straightforward i.e 'C' for a
         capacitor, 'L' for an inductance etc...
         You can type `C-h v tempo-tags'for a complete list of tags and
         associated templates. Note: to insert a real <TAB>, use <C-q TAB> or
         <shift TAB>.

   - Comment & documentation string handling
       + the '*' symbol is used to comment out a whole line - that symbol has
         to be at the beginning of a line
       + the '!' and '$' symbols are used to document your netlist in eldo
         and hspice/layla mode respectively
       + menu entry to comment out region/uncomment region
       + key bindings for commenting/uncommenting a region as in `auctex-mode'.

   - Comment hiding support
       + Can hide all commented out regions in a buffer to improve readability
       + prefix string is customizable: `spice-hide-line-prefix'.
       + custom variable can be set to automatically hide all commented regions
         at load time (`spice-auto-hide-comments')
       + requires use of doc strings, otherwise also documentation might be
         hidden
       + When parts of the deck are hidden the string \"H+\" appears in the
         modeline.

   - Imenu (Index menu and/or shift right click in emacs if configured)
       + shows subcircuit definitions
       + shows .end statements in submenu
       + shows device models in submenus
       + shows libraries (.lib/.inc) in submenu
       + shows analyses in submenu
       + shows sections in submenu
       + shows output file sections in menu
       + shows LAYLA objects in submenus if layla submode has been enabled
       + can be added to the menubar by setting `spice-imenu-add-to-menubar'
         (uses `imenu-add-to-menubar' to add an Imenu entry to the menubar).

   - File browser using Speedbar (`speedbar') and/or index/sources menu

   - .inc/.include/.lib/.libfas access
       + through mouse-2 click (`ffap-at-mouse' or `spice-load-file-at-mouse')
       + using \\r (ie. <return>), (`ffap')
       + through menu entry all include/lib files of a deck can be loaded at
         once (`spice-load-include-files'), recursively.

   - Searching for .subckt defs: `spice-search-subckt' or `C-c C-s'
       + extracts subcircuit name from context
       + search history
       + mark is set where search has been started if the definition is found
         in the same file. Return to mark with `C-u C-<SPC>' (or `C-u C-@')
         as with interactive searches (fi `isearch-forward')
       + be careful when starting the search from an included file,
         correctness can not be guaranteed. Starting a search from
         a top-level .cir file gives correct results. The latest used
         top-level file is stored (a top-level file contains a .end
         statement !), and also searched if the subckt def is not found in
         a first pass (for instance when starting from an included file).

   - Postscript printing with fontification (through `ps-print' package).

   - Addition of Spice Deck submenu in msb mode, see `msb', `spice-msb-fix'.

   - Section support (as in eldo-mode):
       + add section headers, goto section through menu entries or interactive
       + customizable through `spice-section-alist', you can add your
         own section headers, alter the list of predefined sections, ...
       + Changelog addition through `spice-add-changelog-entry', or
         use `C-c a c'.

   - Simulator support
       + use `spice-simulator' and `spice-simulator-switches' to
         select your simulator from `spice-simulator-alist'.
       + Support for error parsing of spice3 (batch-mode, -b),
         hspice, eldo and spectre. Add your own in spice-mode or through
         customization in `.emacs'.
       + use local file variables to customize per file as follows:
<<< test.cir >>>
.op
.end

* Local Variables:
* mode: spice
* spice-simulator: \"Hspice\"
* spice-simulator-switches: \"\"
* eval: (spice-set-command)
* End:
<<< test.cir ends here >>>

   - Waveform viewer support (beta)
       + use `spice-waveform-viewer' and `spice-waveform-viewer-switches' to
         select your waveform from `spice-waveform-viewer-alist'.
       + Support for interactive (command-line) waveform viewer such as
         nutmeg, or batch (GUI) type waveform viewer such as xelga or gsi.

   - Output file support
       + can load output file from menu (checks if file exists and is readable)
       + imenu in output files shows output file sections
       + for eldo .chi files: can create <file>_guess.cir files automatically
         for inclusion in <file>.cir file through
         `spice-create-guess-nodeset-file'
         or `spice-replace-with-guess-nodeset-statements'; this speeds up DC
         convergence for AC analyses.

   - (Fully) customizable
       + can select spice compatibility mode:
          * spice2g6/3 (default)
          * hspice (default)
          * eldo (default), RF and verilog-A extensions
          * fasthenry (default)
          * Dracula CDL
          * LAYLA, Mondriaan extensions
          * any combination of the above (there are conflicts however, so not
            all keyword fontification is correct in the latter case)
       + spice-mode font-lock faces
       + spice-mode default initialization of empty/new files determined by
         `spice-initialize-empty-file', `spice-initialize-template-file',
         `spice-default-header' and `spice-initialize-file-function'.
       + see the customization examples in the `spice-mode.el' file header
       + You can adapt `spice-simulator-alist' for your local site in
         `spice-mode.el' or in `.emacs' through customization.
       + You can adapt `spice-waveform-viewer-alist' for your local site in
         `spice-mode.el' or in `.emacs' through customization.
       + You can adapt `spice-section-alist' for your local site in
         `spice-mode.el' or in `.emacs' through customization.

   - Auto fill minor mode support
       + can be turned on from the Settings menu
       + auto fill works both for element where the continuation character
         is a '+', as well as in comment/doc mode where the continuation
         character is a '*', '$' or a '!'.
       + uses `fill-column' to determine where to wrap the line
       + doesn't use `fill-prefix'. A context dependent prefix is calculated
         through the `spice-comment-indent-new-line' function. This function
         is used as `comment-line-break-function' instead of the default line
         break function (in emacs: `comment-indent-new-line').

   - Paragraph support: [a-z] starts dev lines, '+' continues dev lines,
     [*!$] start paragraphs.

   - Works under GNU Emacs20.6/21.[123].


Do not use a -*- Mode -*- line in a spice deck as the first card in
the deck is defined to be the title card. Rather, autoload spice-mode
through your .emacs file:

 (autoload 'spice-mode \"spice-mode\" \"Spice/Layla Editing Mode\" t)
 (setq auto-mode-alist (append (list (cons \"\\\\.sp$\"  'spice-mode)
 				     (cons \"\\\\.cir$\" 'spice-mode)
 				     (cons \"\\\\.cdl$\" 'spice-mode)
 				     (cons \"\\\\.chi$\" 'spice-mode) ; output
 				     (cons \"\\\\.mod$\" 'spice-mode)); models
 			       auto-mode-alist))

Alternative methods are provided in the spice-mode.el file header.

Key bindings in highlighted include file lines:
-----------------------------------------------

\\{spice-mode-mouse-map}

Key bindings for other parts in the file:
-----------------------------------------

\\{spice-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spice-mode" '("set-spice-name" "spice-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; spice-mode-autoloads.el ends here
