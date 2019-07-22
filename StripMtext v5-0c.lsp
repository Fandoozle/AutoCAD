;;;;  StripMtext Version 5.0c for AutoCAD 2000 and above
;;;;  Removes embedded Mtext formatting
;;;;
;;;;  Copyright© Steve Doman and Joe Burke 2010
;;;;
;;;;  The authors grant permission to use, copy, and modify this routine
;;;;  for personal use only and for the use of other AutoCAD users within
;;;;  your organization. Selling, modifying, or exchanging this software
;;;;  for a fee, or incorporation within a commercial software product, is
;;;;  expressly prohibited. All other rights are reserved by the authors.
;;;;
;;;;  Please send comments, wish lists, or bug reports to:
;;;;  cadabyss@gmail.com or lowercase@hawaii.rr.com
;;;; 
;;;;  Look for new stable releases at:
;;;;  http://cadabyss.wordpress.com/
;;;; 
;;;;  More information may also be found at:
;;;;  http://www.theswamp.org/
;;;;  Subforum: "Show your stuff", Subject: "StripMtext v5"
;;;;
;;;;
;;;;  DESCRIPTION
;;;;
;;;;  This AutoLISP program creates a command "StripMtext" (shortcut
;;;;  "SMT"), that will enable the user to quickly remove selected
;;;;  formatting codes from selected Mtext, Mleaders, Dimensions, Tables,
;;;;  and Multiline Attributes.
;;;;
;;;;  StripMtext can remove the following types of formatting:
;;;;
;;;;  Alignment
;;;;  Background Masks
;;;;  Color
;;;;  Columns
;;;;  Fields     (converts fields to static text)
;;;;  Font
;;;;  Height
;;;;  Line Feed  (newline, line break, carriage return)
;;;;  Non-breaking Space
;;;;  Obliquing
;;;;  Overline
;;;;  Paragraph  (embedded justification, line spacing, indents)
;;;;  Stacking
;;;;  Tabs
;;;;  Tracking
;;;;  Underline
;;;;  Width
;;;;
;;;;
;;;;  CAVEATS
;;;;
;;;;  Acad Versions -
;;;;  If your version of AutoCAD does not support a formatting code
;;;;  introduced in a latter year, that format will be disabled and appear
;;;;  grayed-out in the dialog.
;;;;
;;;;  Locked Table Cells -
;;;;  If locked cells are found in a table while processing, they will be
;;;;  skipped and the message "Some table cells are locked" will be
;;;;  printed at the commnand prompt. This is by design and intended to
;;;;  protect cell contents from accidental stripping.
;;;;
;;;;  Reformatting Alignment -
;;;;  It has been observed that after running StripMtext to remove
;;;;  alignment formats from dimension objects, AutoCAD will sometimes
;;;;  automatically add back the alignment format ("\\A1;").  AutoCAD's
;;;;  apparent reformatting behavior makes it appear that there is a bug
;;;;  in this routine.  However tests indicate that the dimension mtext
;;;;  string was indeed stripped correctly but AutoCAD, for what ever
;;;;  reason, put it back.  A similar situation occurs with Multiline
;;;;  Attributes.
;;;;
;;;;  Reformatting Fonts -
;;;;  AutoCAD will automatically add back font formatting around
;;;;  certain symbols characters after stripping, e.g. Isocpeur font
;;;;  is automatically reapplied to the centerline symbol.
;;;;
;;;;  Dimension Fractions -
;;;;  StripMtext does not unstack fractions that are a part of the displayed
;;;;  measurement value, i.e. "<>".  It will remove any formatting
;;;;  applied before, to, and after the measurement value.
;;;;
;;;;  Fields Updating -
;;;;  StripMtext uses the UPDATEFIELD command prior to removing formatting
;;;;  from Fields embedded in Mtext and Multiline Attributes.
;;;;
;;;;
;;;;  HOW TO LOAD (for the newbie)
;;;;
;;;;  There are a few different methods to load an AutoLISP program.
;;;;  Perhaps the easiest method is to type APPLOAD at the command prompt.
;;;;  Then browse to the location of this file. Highlight the file name,
;;;;  and then hit "Load". Hit the "Close" button to dismiss the APPLOAD
;;;;  dialog. This procedure loads the program into the current drawing.
;;;;
;;;;  To automatically load this file each time you open a drawing, add
;;;;  the filename to APPLOAD's Startup Suite: APPLOAD > Contents > Add >
;;;;  Browse to file > Load.
;;;;
;;;;
;;;;  HOW TO USE
;;;;
;;;;  (1) When you first start StripMtext, you will be asked to select
;;;;      objects. When you have finished selecting, hit ENTER.
;;;;
;;;;      Alternatively, if you pre-select (grip) objects and then issue
;;;;      the StripMtext command, the pre-selected objects will be
;;;;      accepted and the routine will move on to the next step without
;;;;      further prompting.  This so called "noun/verb" selection
;;;;      behavior is dependent on the system variable PICKFIRST being set
;;;;      to 1.
;;;;
;;;;      With either selection method you choose to use, StripMtext will
;;;;      remove from your selection any unsupported objects and any
;;;;      objects that reside on locked layers.
;;;;
;;;;  (2) Next, a dialog window will appear that displays a list of the
;;;;      names of each formatting code with a corresponding check box.
;;;;      Turn on the check box for each type of formatting you wish to
;;;;      remove.  You can quickly turn on or off all check boxes by using
;;;;      the "Select All" or "Clear All" buttons.
;;;;
;;;;  (3) If you would like StripMtext to save your checked marked
;;;;      settings as a your default, turn on the "Remember Settings"
;;;;      check box.  StripMtext will store your default settings in the
;;;;      Windows Registry.
;;;;
;;;;  (4)  Hit the "Ok" button to proceed with removing formats or the
;;;;      "Cancel" button to exit without making changes.
;;;;      
;;;;  (5)  Enjoy!
;;;;
;;;;
;;;;  You are encouraged to spend a few minutes experimenting with
;;;;  different format removal settings using a temporary drawing. If for
;;;;  any reason you do not like the results, you can immediately issue an
;;;;  UNDO command to restore your drawing to its prior condition.
;;;;
;;;;
;;;;  HOW TO USE BY SCRIPT OR AUTOLISP
;;;;
;;;;  When the StripMtext file loads into the drawing, it purposely
;;;;  exposes the StripMtext function for your use during scripts and/or
;;;;  your own AutoLISP routines.
;;;;
;;;;  This function by-passes the user interface and therefore is an
;;;;  excellent method to remove formatting from a batch of drawings
;;;;  without user input, or to use in your own custom commands where you
;;;;  need to remove Mtext formatting.
;;;;
;;;;  To do this, your script or AutoLISP routine must load the StripMtext
;;;;  file into the current drawing and then call StripMtext with valid
;;;;  arguments.
;;;;
;;;;  Syntax:
;;;;
;;;;    (StripMtext SS Formats)
;;;;
;;;;    SS       A pickset containing entities to process. StripMtext will
;;;;             ignore entities in the pickset that it does not support.
;;;;
;;;;              Supported entities
;;;;              ------------------
;;;;              Dimensions
;;;;              Mleaders
;;;;              Mtext
;;;;              Multiline Attributes (embedded in block inserts)
;;;;              Tables
;;;;
;;;;    Formats  A string or a list of strings containing format "key code"
;;;;             options. Each key code is mapped to a particular type of
;;;;             format as listed below. A caret "^" preceding a format
;;;;             code negates that format code, i.e. it explicitly means
;;;;             not to remove that particular format.
;;;;
;;;;             Available format key codes
;;;;             --------------------------
;;;;             "A" = Alignment
;;;;             "B" = taBs
;;;;             "C" = Color
;;;;             "D" = fielDs      (converts fields to static text)
;;;;             "F" = Font
;;;;             "H" = Height
;;;;             "L" = Linefeed    (newline, line break, carriage return)
;;;;             "M" = background Mask
;;;;             "N" = columNs
;;;;             "O" = Overline
;;;;             "P" = Paragraph   (embedded justification, line spacing, indents)
;;;;             "Q" = obliQue
;;;;             "S" = Stacking
;;;;             "T" = Tracking
;;;;             "U" = Underline
;;;;             "W" = Width
;;;;             "~" = non-breaking space
;;;;             "*" = all formats
;;;;
;;;;
;;;;  Example 1:
;;;;
;;;;  Load the StripMText file from script or AutoLISP.  Assumes
;;;;  StripMtext file resides in an AutoCAD support file search folder:
;;;;
;;;;  (load "StripMtext v5-0a") ;_ check and update file name
;;;;
;;;;
;;;;  Example 2:
;;;;
;;;;  Prompt the user to select objects and remove only color, font, &
;;;;  height formatting.  There will not be a dialog or any other prompt
;;;;  for choosing formats.
;;;;
;;;;  (if (setq ss (ssget)) (StripMtext ss "CFH"))
;;;;   - OR -
;;;;  (if (setq ss (ssget)) (StripMtext ss '("C" "F" "H")))
;;;;
;;;;
;;;;  Example 3:
;;;;
;;;;  Remove all formatting except hard returns from all supported
;;;;  entitites without a prompt:
;;;;
;;;;  (StripMtext (ssget "x") "*^L")
;;;;  - OR -
;;;;  (StripMtext (ssget "x") '("*" "^L"))
;;;;
;;;;  Caution:
;;;;
;;;;  Never run the above function on a batch of drawings without a
;;;;  thorough understanding of how the format removal options work and
;;;;  how removing them affects the end results. Experiment to become
;;;;  familiar with the options before using on a batch of drawings.
;;;;
;;;;
;;;;  HISTORY
;;;;
;;;;  v1.0 06-14-1999  "The DSAKO Years" R14
;;;;  A first attempt of dealing with the problem of removing Mtext
;;;;  formatting came while writing a routine named "DSAKO" (short for
;;;;  "Dimstyle Apply Keep Overrides"). It was discovered that Mtext
;;;;  formatting was overriding the text style height and font. Wrote a
;;;;  subfunction called ClearMtext which stripped font, height, and
;;;;  stacked fraction formatting from Mtext. sd
;;;;
;;;;  v2.0 08-25-2001  "First stand alone StripMtext version"
;;;;  Faster speed and removes all current formatting possibilities,
;;;;  except linefeeds. sd
;;;;
;;;;  v3.0 05-26-2003 "The Uhden Unformat Version" Vlisp
;;;;  Powered by the new Unformat parser function written by John Uhden,
;;;;  which provided much better, faster, and more reliable format
;;;;  removing than previous versions. Added support for dimensions
;;;;  objects and introduced a new DCL allowing users to choose individual
;;;;  formats and save defaults. sd
;;;;
;;;;  v3.05 01-14-04
;;;;  "Quit/Exit" bug fixed. sd
;;;;
;;;;  v3.06 03-21-04
;;;;  Only changes to comments, otherwise same as v3.05. sd
;;;;
;;;;  v3.07 04-15-04
;;;;  Fixed a "Unknown dimension" bug when drawing contained 2LineAngular
;;;;  dimensions. Thanks to Keith Kempker for reporting this error and for
;;;;  helping with debugging. sd
;;;;
;;;;  v3.08 03-22-06
;;;;  Per request from Paul Muti, exposed subfunctions such that
;;;;  StripMtext may be run from a script or another lisp. sd
;;;;
;;;;  v3.09 01-17-07
;;;;  Fixed "Error: bad argument value: positive 0" This bug was reported
;;;;  by Joe Burke when the routine processes an mtext object which begins
;;;;  with a return, example "\\Ptest". Joe also found the bug and
;;;;  provided code to fix the problem! This version incorporates his
;;;;  solution. Thanks Joe! sd
;;;;
;;;;  v4.0 Beta - "The Lost Version"
;;;;  This version was never released to the public due to programming
;;;;  difficulties which I could not overcome. Since a few copies went
;;;;  out for beta testing, I felt it necessary to include version 4 in
;;;;  the history list so as to bump the next version up and avoid any
;;;;  confusion with the so called lost version. sd
;;;;
;;;; 
;;;;  v5.0 01-01-10 "The Joe Burke RegExp Version"
;;;;  The stripping functions in this version have been completely
;;;;  rewritten by Joe Burke and make use of the search and replace power
;;;;  of regular expressions via the RegExp object.  Joe Burke's coding
;;;;  added support to remove all current Mtext formatting codes including
;;;;  new format codes for tabs, indents, embedded justification, fields,
;;;;  columns, and background masks.  Joe also added support for
;;;;  processing new entity objects that contain mtext: Mleaders, Tables,
;;;;  and Multiline Attributes.  Other changes are the elimination of the
;;;;  external DCL file by creating a temporary DCL written "on the fly". 
;;;;  Comments have been rewritten and expanded to make it easier for
;;;;  new user to understand how to load and run.  I also wish to thank
;;;;  Lee Mac for creating animated GIFs demonstrating StripMtext in
;;;;  action.  sd
;;;;
;;;;  v5.0a 02-01-10
;;;;  1.) Changed handling of dimensions objects to preserve
;;;;  associativity of measurement value.  2.) Fixed compatibility
;;;;  issue when processing locked Table cells prior to AutoCAD 2008.
;;;;  3.) Fixed failure to remove columns when Textstyle is
;;;;  annotative.  4.) Added work around for AutoCAD problem when
;;;;  user issues an UNDO after stripping Fields.  5.) Improved
;;;;  handling of stacked fractions to preserve readability.
;;;;  Thanks to Ian Bryant for his IsAnnotative function.
;;;;
;;;;  v5.0b 02-10-10
;;;;  Corrected wrong AutoCAD version number used to determine if ssget
;;;;  filter should include Mleaders and Inserts objects.
;;;;
;;;;  v5.0c 07-05-10
;;;;  Revised regular expression for Height format to include either upper or lower case x's
;;;;  e.g. "\\H1.5x" or "\\H1.5X"
;;;;
;;;;  GLOBALS LIST
;;;;
;;;;  *REX*         (blackboard)
;;;;  *smt-acad*    (blackboard)
;;;;  *smt-doc*
;;;;  *smt-blocks*
;;;;  *smt-layers*
;;;;  *smt-dclfilename*
;;;;  *smt-smtver*
;;;;  *sbar*
;;;;
;;;;  C:SMT
;;;;  C:StripMtext
;;;;  StripMtext
;;;;  StripMtextDCL
;;;;  smt-acad
;;;;  smt-doc
;;;;  smt-blocks
;;;;  smt-layers
;;;;
(vl-load-com)
(setq *smt-smtver* "5.0c")
;; How globals to objects are defined may change in future version
(defun smt-acad ()
  ;; Sets and returns global var referencing Acad ojbect
  ;; Stores var in blackboard namespace
  (cond ((vl-bb-ref '*smt-acad*))
        (t (vl-bb-set '*smt-acad* (vlax-get-acad-object)))
  )
)
(defun smt-doc ()
  ;; Sets and returns global var referencing doc object
  (cond (*smt-doc*)
        (t (setq *smt-doc* (vla-get-activedocument (smt-acad))))
  )
)
(defun smt-blocks ()
  ;; Sets and returns global var referencing the blocks collection
  (cond (*smt-blocks*)
        (t (setq *smt-blocks* (vla-get-blocks (smt-doc))))
  )
)
(defun smt-layers ()
  ;; Sets and returns global var referencing the layers collection
  (cond (*smt-layers*)
        (t (setq *smt-layers* (vla-get-layers (smt-doc))))
  )
)

;;
(defun c:StripMtext (/ *error* ss formats count acadver ssfilter)
  ;;
  ;; User command
  ;;
  (defun *error* (msg)
    (vla-endundomark (smt-doc))
    (cond ((vl-position
             msg
             '("Function cancelled" "quit / exit abort" "console break")
           )
          )
          ((princ (strcat "\nStripMtext Error: " msg)))
    )
    ;; SD 12-20-09 vl-filename-mktemp not consistently deleting temp files
    (if *smt-dclfilename*
      (vl-file-delete *smt-dclfilename*)
    )
    ;; Added JB 11/16/2009 Cmdecho is set to 0 in the StripMLeader function.
    (setvar "cmdecho" 1)
    (princ)
  )
  ;; added version specific ssget filter SD 2-2-10
  (setq acadver (atof (getvar "acadver")))
  (setq ssfilter "MTEXT,DIMENSION")
  (if (>= acadver 16.1) ;_Acad2005
    (setq ssfilter (strcat ssfilter ",ACAD_TABLE"))
  )
  (if (>= acadver 17.1) ;_Acad2008 corrected ver num 2-10-10
    (setq ssfilter (strcat ssfilter ",MULTILEADER,INSERT"))
  )
  (setq ssfilter (list (cons 0 ssfilter)))
  ;;
  (vla-startundomark (smt-doc))
  (setvar "cmdecho" 0) ;_ SD 2-0-10
  (prompt (strcat "\nStripMtext v" *smt-smtver*))
  (if (and (setq ss (ssget ;_ get selection
                      ":L"
                      ssfilter
                    )
           )
           (setq formats (StripMtextDCL)) ;_ get options
           (setq count (StripMtext ss formats)) ;_ process
      )
    (princ (strcat "\nStripMtext completed. " ;_ print report
                   (itoa count)
                   " objects processed."
           )
    )
    (princ "\t*Cancel*")
  )
  (setvar "cmdecho" 1)
  (vla-endundomark (smt-doc))
  (princ)
)
(defun c:SMT () (c:StripMtext)) ;_shortcut
;;;
(defun StripMtextDCL (/ acadver dcl_id formats
                        keylist user regkey
                        _AcceptButton _ClearAllButton
                        _dclWrite _KeyToggle _RunDialog
                        _SelectAllButton
                       )
  ;;
  ;; Function to create the DCL for StripMtext
  ;; Arguments: None
  ;; Returns: User input from DCL or nil
  ;;
  (defun _dclWrite (/ dclcode filename filehandle)
    ;; Makes a temporary DCL file at runtime
    ;; Returns name of the file or NIL
    (setq dclcode
           (list ;_ tilenames are case sensitive
             "// Temporary DCL file"
             (strcat "stripmtext"
                     ":dialog {label = \"StripMtext v"
                     *smt-smtver*
                     "\";"
             )
             (strcat ":text { value = \"Removes formatting from "
                     "Mtext, Mleaders, Dimensions, Tables, & "
                     "Multiline Attributes\";}"
             )
             "spacer_1;                                                   "
             ":toggle {key = \"save\"; label = \"Remember Settings\";}    "
             "spacer_1;                                                   "
             ":boxed_row {label = \"Select type of formatting to remove\";"
             "  :column {                                                 "
             "    :toggle {key = \"A\"; label = \"Alignment\";}           "
             "    :toggle {key = \"C\"; label = \"Color\";}               "
             "    :toggle {key = \"F\"; label = \"Font\";}                "
             "    :toggle {key = \"H\"; label = \"Height\";}              "
             "    :toggle {key = \"L\"; label = \"Linefeed\";}            "
             "    :toggle {key = \"~\"; label = \"Nonbreaking~Space\";}   "
             "    :toggle {key = \"Q\"; label = \"Oblique\";}             "
             "  }                                                         "
             "  :column {                                                 "
             "    :toggle {key = \"O\"; label = \"Overline\";}            "
             "    :toggle {key = \"P\"; label = \"Paragraph\";}           "
             "    :toggle {key = \"S\"; label = \"Stacking\";}            "
             "    :toggle {key = \"B\"; label = \"Tabs\";}                "
             "    :toggle {key = \"T\"; label = \"Tracking\";}            "
             "    :toggle {key = \"U\"; label = \"Underline\";}           "
             "    :toggle {key = \"W\"; label = \"Width\";}               "
             "  }                                                         "
             "  :column {                                                 "
             "    :toggle {key = \"M\"; label = \"Background Masks\";}    "
             "    :toggle {key = \"D\"; label = \"Fields\";}              "
             "    :toggle {key = \"N\"; label = \"Columns\";}             "
             "    :spacer {height = 6.0;}                                 "
             "    }                                                       "
             "  :column {                                                 "
             "    :button {key = \"selectall\"; label = \"Select All\";}  "
             "    :button {key = \"clearall\"; label = \"Clear All\";}    "
             "    :spacer {height = 6.0;}                                 "
             "    }                                                       "
             "}                                                           "
             "errtile;                                                    "
             "ok_cancel;                                                  "
             "}                                                           "
           )
    )
    ;; Revised temp file name 12-20-09 sd
    (if (and (setq filename (vl-filename-mktemp "SMT" nil ".tmp"))
             (setq filehandle (open filename "w"))
        )
      (progn (foreach line dclcode (write-line line filehandle))
             (close filehandle)
      )
    )
    filename
  )
  (defun _SelectAllButton ()
    ;; Turn "on" all format toggle keys
    ;; Requires global variable 'keylist
    (mapcar '(lambda (key) (set_tile key "1")) keylist)
    (set_tile "error" "")
    (mode_tile "accept" 0) ;_ enable
    (mode_tile "accept" 2) ;_ focus
  )
  (defun _ClearAllButton ()
    ;; Turn "off" all format toggle keys
    ;; Requires global variable 'keylist
    (mapcar '(lambda (key) (set_tile key "0")) keylist)
    (set_tile
      "error"
      "Select one or more formats to remove or press \"Cancel\" to exit"
    )
    (mode_tile "accept" 1) ;_ disable
  )
  (defun _AcceptButton (/ formats)
    ;; Get and save user settings and exit dialog
    ;; Requires global variables 'keylist and 'regkey
    ;; Returns list of user chosen format keys
    (setq formats (vl-remove-if
                    '(lambda (key) (= (get_tile key) "0"))
                    keylist
                  )
    )
    (vl-registry-write regkey "Save" (get_tile "save"))
    (if (= (get_tile "save") "1")
      (vl-registry-write regkey "Settings" (apply 'strcat formats))
    )
    (done_dialog 1)
    formats
  )
  (defun _KeyToggle ()
    ;; Turn on/off error message and enable/disable "ok" button
    ;; Requires global variable 'keylist
    (if (vl-some '(lambda (key) (= (get_tile key) "1")) keylist)
      (progn (mode_tile "accept" 0) (set_tile "error" ""))
      (progn
        (mode_tile "accept" 1)
        (set_tile
          "error"
          "Select one or more formats to remove or press \"Cancel\" to exit"
        )
      )
    )
  )
  (defun _RunDialog (/ status formats)
    ;; Display DCL with toggle preset with user's saved settings
    ;; Creates default settings when routine is run on first time
    ;; Requires global variables 'keylist, 'regkey, 'acaver, 'dcl_id
    ;; Requires functions '_ClearAllButton, _SelectAllButton, _AcceptButton
    ;; Returns list of chosen toggle/format keys if user exits DCL using Okay button
    ;; Returns NIL if user exits using Cancel button
    (set_tile "save"
              (cond ((vl-registry-read regkey "Save"))
                    ((vl-registry-write regkey "Save" "1"))
              )
    )
    (mapcar '(lambda (key) (set_tile key "1"))
            (mapcar 'chr
                    (vl-string->list
                      (cond ((vl-registry-read regkey "Settings"))
                            ((vl-registry-write regkey "Settings" "CFH")) ;_ default
                      )
                    )
            )
    )
    (if (> 16.1 acadver) ;_ disable fields & mask toggle keys
      (progn (mode_tile "M" 1) (mode_tile "D" 1))
    )
    (if (> 17.1 acadver) ;_ disble mtext columns toggle key
      (mode_tile "N" 1)
    )
    ;; Define button callbacks and run dialog
    (mapcar '(lambda (key) (action_tile key "(_KeyToggle)"))
            keylist
    )
    (action_tile "clearall" "(_ClearAllButton)")
    (action_tile "selectall" "(_SelectAllButton)")
    (action_tile "accept" "(setq formats (_AcceptButton))")
    (action_tile "cancel" "(done_dialog 0)")
    (setq status (start_dialog))
    (unload_dialog dcl_id)
    ;; Added 12-20-09 sd Despite what the manual says, vl-filename-mktemp
    ;; files were not always being automatically deleted
    (vl-file-delete *smt-dclfilename*)
    ;; If status = 1 , then Accept button hit
    (if (= status 1)
      formats
    )
  ) ;_ RunDialog
  ;;
  ;; Begin main DCL routine
  ;;
  (setq regkey  "HKEY_CURRENT_USER\\SOFTWARE\\StripMtext\\"
        acadver (atof (getvar "acadver"))
        keylist (append (if (<= 15.0 acadver) ;_ vlisp required 2000
                          '("A"   "B"   "C"   "F"   "H"   "L"   "O"
                            "Q"   "P"   "S"   "T"   "U"   "W"   "~"
                           )
                        )
                        (if (<= 16.1 acadver) ;_ fields, mask, tables 2005
                          '("M" "D")
                        )
                        (if (<= 17.1 acadver) ;_ mtext columns added 2008
                          '("N")
                        )
                )
  )
  (cond ;; Exit routine if not running in Acad 2000 or above
        ((not keylist)
         (alert "StripMtext Error:\nRequires AutoCAD 2000 or higher")
        )
        ;; Create DCL file
        ((null (setq *smt-dclfilename* (_dclwrite)))
         (alert "StripMtext Error:\nUnable to write DCL file")
        )
        ;; Exit if cannot find DCL file
        ((< (setq dcl_id (load_dialog *smt-dclfilename*)) 0)
         (alert (strcat "StripMtext Error:\nCannot load DCL file:\n"
                        *smt-dclfilename*
                )
         )
        )
        ;; Exit if DCL fails to load
        ((not (new_dialog "stripmtext" dcl_id))
         (alert "StripMtext Error:\nCannot display dialog")
        )
        ;; Run DCL and return user's chosen formats
        ((_RunDialog))
  )
)
;;;
(defun StripMtext
       (ss formats / mtextobjlst mldrobjlst dimobjlst tableobjlst layers
                     mattobjlst obj objname str cnt spinflag lockedcellflag
                     ;; functions
                     Spinbar FormatsToList StripFormat StripColumn StripMask
                     StripField StripTableFields StripTable StripMLeader
                     StripMAttribute RowsColumns CellFieldOwner SymbolString
                     GetFields IsAnnotative GetAnnoScales)

  ;;;
  ;;; StripMtext
  ;;;
  ;;; Parses supplied list of format keys and selection set to determine which
  ;;; Strip* function to operate on which entities. Iterates through selected
  ;;; objects and passes appropriate arguments to appropriate Strip* function
  ;;;
  ;;; Returns count of entities processed
  ;;;
  ;;; 'ss argument is a pickset containing valid entities
  ;;; 'formats argument is a list of format keys: '("A" "C" ... "F")
  ;;;                   or a string of format keys: "ACF"
  ;;;
  ;;;  For more info on syntax and valid arugments, please refer to 
  ;;; "HOW TO USE BY SCRIPT OR AUTOLISP" in header comments at top of file,
  ;;;  or read through comments in subs below.
  ;;;
  ;;;  Powered by Joe Burke's stripping functions:
  ;;; 
  ;;;    StripColumn
  ;;;    StripField
  ;;;    StripFormat
  ;;;    StripMask
  ;;;    StripMAttribute
  ;;;    StripMLeader
  ;;;    StripTable
  ;;;    StripTableFields
  ;;;    SymbolString
  ;;;    CellFieldOwner
  ;;;    FormatsToList
  ;;;    GetFields
  ;;;    RowsColumns
  ;;;    IsAnnotative
  ;;;    GetAnnoScales

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Define Stripping functions ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Argument: either a list of strings or a string.
  ;; Given a list, ensure formats are uppercase.
  ;; Given a formats string, convert it to a list of uppercase strings.
  ;; Examples: (FormatsToList "fOU") > ("F" "O" "U")
  ;;           (FormatsToList "f^OU") > ("F" "^O" "U")
  (defun FormatsToList (arg / lst)
    (cond
      ((= (type arg) 'LIST)
        (mapcar 'strcase arg)
      )
      ((= (type arg) 'STR)   
        (while (not (eq "" (substr arg 1)))
          (if (eq "^" (substr arg 1 1))
            (setq lst (cons (strcat "^" (substr arg 2 1)) lst)
                  arg (substr arg 3)
            )
            (setq lst (cons (substr arg 1 1) lst)
                  arg (substr arg 2)
            )
          )
        )
        (mapcar 'strcase (reverse lst))
      )
    )
  ) ; end FormatsToList  

  ;; Arguments:
  ;; str - an mtext string.
  ;; formats - a list of format code strings or a string.
  ;; Format code arguments are not case sensitive.

  ;; Examples:
  ;; Remove Font, Overline and Underline formatting.
  ;; (StripFormat <mtext string> (list "f" "O" "U"))
  ;; Or a quoted list:
  ;; (StripFormat <mtext string> '("f" "O" "U"))
  ;; Or a string:
  ;; (StripFormat <mtext string> "fOU")

  ;; Remove all formatting except Overline and Underline.
  ;; (StripFormat <mtext string> (list "*" "^O" "^U"))
  ;; Or a quoted list:
  ;; (StripFormat <mtext string> '("*" "^O" "^U"))
  ;; Or a string:
  ;; (StripFormat <mtext string> "*^O^U")

  ;; Available codes:
  ;; A (^A) - Alignment
  ;; B (^B) - taBs
  ;; C (^C) - Color
  ;; F (^F) - Font
  ;; H (^H) - Height
  ;; L (^L) - Linefeed (newline, line break, carriage return)
  ;; O (^O) - Overline
  ;; Q (^Q) - obliQuing
  ;; P (^P) - Paragraph (embedded justification, line spacing and indents)
  ;; S (^S) - Stacking
  ;; T (^T) - Tracking
  ;; U (^U) - Underline
  ;; W (^W) - Width
  ;; ~ (^~) - non-breaking space
  ;; * - all formats

  (defun StripFormat (str formats / text slashflag lbrace rbrace
                      RE:Replace RE:Execute Alignment Tab Color
                      Font Height Linefeed Overline Paragraph Oblique
                      Stacking Tracking Underline Width Braces HardSpace)

    (setq formats (FormatsToList formats))

    ;; Access the RegExp object from the blackboard.
    ;; Thanks to Steve for this idea.
    (or
      (vl-bb-ref '*REX*)
      (vl-bb-set '*REX* (vlax-create-object "VBScript.RegExp"))
    )
    (defun RE:Replace (newstr pat string)
      (vlax-put (vl-bb-ref '*REX*) 'Pattern pat)
      (vlax-put (vl-bb-ref '*REX*) 'Global actrue)
      (vlax-put (vl-bb-ref '*REX*) 'IgnoreCase acfalse)
      (vlax-invoke (vl-bb-ref '*REX*) 'Replace string newstr)
    ) ;end
    (defun RE:Execute (pat string / result match idx lst)
      (vlax-put (vl-bb-ref '*REX*) 'Pattern pat)
      (vlax-put (vl-bb-ref '*REX*) 'Global actrue)
      (vlax-put (vl-bb-ref '*REX*) 'IgnoreCase acfalse)
      (setq result (vlax-invoke (vl-bb-ref '*REX*) 'Execute string))
      (vlax-for x result
        (setq match (vlax-get x 'Value)
              idx   (vlax-get x 'FirstIndex)
              ;; position within string - zero based - first position is zero
              lst   (cons (list match idx) lst)
        )
      )
      lst
    ) ;end

    ;; Replace linefeeds using this format "\n" with the AutoCAD
    ;; standard format "\P". The "\n" format occurs when text is
    ;; copied to ACAD from some other application.
    (setq str (RE:Replace "\\P" "\\n" str))

    ;;;;; Start remove formatting sub-functions ;;;;;
    ;; A format
    (defun Alignment (str) (RE:Replace "" "\\\\A[012];" str))
    ;; B format (tabs)
    (defun Tab (str / lst origstr tempstr)
      (setq lst (RE:Execute "\\\\P\\t|[0-9]+;\\t" str))
      (foreach x lst
        (setq origstr (car x)
              tempstr (RE:Replace "" "\\t" origstr)
              str     (vl-string-subst tempstr origstr str)
        )
      )
      (RE:Replace " " "\\t" str)
    )
    ;; C format
    (defun Color (str)
      ;; True color and color book integers are preceded
      ;; by a lower case "c". Standard colors use upper case "C".
      (RE:Replace "" "\\\\[Cc][0-9]?[.]?[0-9]+;" str)
    )
    ;; F format
    (defun Font (str) (RE:Replace "" "\\\\[Ff].*?;" str))
    ;; H format
    (defun Height (str)
      ;; revised 6/6/2010
      ;(RE:Replace "" "\\\\H[0-9]?[.]?[0-9]+x;" str)
      (RE:Replace "" "\\\\H[0-9]*?[.]?[0-9]*?(x|X)+;" str)
    )
    ;; L format
    ;; Leading linefeeds are not converted to spaces.
    (defun Linefeed (str / teststr)
      ;; Remove formatting from test string other than linefeeds.
      ;; Seems there's no need to check for stacking
      ;; because a linefeed will always come before stack formatting.
      (setq teststr (Alignment str)
            teststr (Color teststr)
            teststr (Font teststr)
            teststr (Height teststr)
            teststr (Overline teststr)
            teststr (Paragraph teststr)
            teststr (Oblique teststr)
            teststr (Tracking teststr)
            teststr (Underline teststr)
            teststr (Width teststr)
            teststr (Braces teststr)
      )
      ;; Remove leading linefeeds.
      (while (eq "\\P" (substr teststr 1 2))
        (setq teststr (substr teststr 3)
              str (vl-string-subst "" "\\P" str)
        )
      )
      (RE:Replace " " " \\\\P|\\\\P |\\\\P" str)
    )
    ;; O format
    (defun Overline (str) (RE:Replace "" "\\\\[Oo]" str))
    ;; This option is effectively the same as the Remove Formatting >
    ;; Remove Paragraph Formatting option avaiable in the 2008 Mtext editor.
    (defun Paragraph (str) (RE:Replace "" "\\\\p.*?;" str))
    ;; Q format - numeric value may be negative.
    (defun Oblique (str)
      ;; Any real number including negative values.
      (RE:Replace "" "\\\\Q[-]?[0-9]*?[.]?[0-9]+;" str)
    )
    ;; S format
    (defun Stacking (str / lst tempstr pos origstr teststr testpos numcheck)
      (setq lst (RE:Execute "\\\\S(.*?)(\\;)" str))
      (foreach x lst
        (setq tempstr (car x)
              pos     (cadr x)
              origstr tempstr
        )
        ;; Remove formatting from test string other than stacking.
        (setq teststr (Alignment str)
              teststr (Color teststr)
              teststr (Font teststr)
              teststr (Height teststr)
              teststr (Linefeed teststr)
              teststr (Overline teststr)
              teststr (Paragraph teststr)
              teststr (Oblique teststr)
              teststr (Tracking teststr)
              teststr (Underline teststr)
              teststr (Width teststr)
              teststr (Braces teststr)
        )
        ;; Remove all "{" characters if present. Added JB 2/1/2010.
        (setq teststr (RE:Replace "" "[{]" teststr))
        ;; Get the stacked position within test string.
        (setq testpos (cadar (RE:Execute "\\\\S(.*?)(\\;)" teststr)))
        ;; Avoid an error with substr if testpos is zero.
        ;; A space should not be added given a stacked
        ;; fraction string which is simply like this 1/2" anyway.
        (if (/= 0 testpos)
          (setq numcheck (substr teststr testpos 1))
        )
        ;; Check whether the character before a stacked string/fraction 
        ;; is a number. Add a space if it is.
        (if
          (and
            numcheck
            (<= 48 (ascii numcheck) 57)
          )
          (setq tempstr (RE:Replace " " "\\\\S" tempstr))
          (setq tempstr (RE:Replace "" "\\\\S" tempstr))
        )
        (setq tempstr (RE:Replace "/" "[#]" tempstr)
              tempstr (RE:Replace "" "[;]" tempstr)
              tempstr (RE:Replace "" "\\\\A(.*?)[;]" tempstr)
              tempstr (RE:Replace "" "\\^" tempstr)
              str     (vl-string-subst tempstr origstr str pos)
        )
      )
      str
    )
    ;; T format
    (defun Tracking (str) (RE:Replace "" "\\\\T[0-9]?[.]?[0-9]+;" str))
    ;; U format
    (defun Underline (str) (RE:Replace "" "\\\\[Ll]" str))
    ;; W format
    (defun Width (str) (RE:Replace "" "\\\\W[0-9]?[.]?[0-9]+;" str))
    ;; ~ format
    ;; In 2008 a hard space includes font formatting.
    ;; In 2004 it does not, simply this \\~.
    (defun HardSpace (str) (RE:Replace " " "{\\\\[Ff](.*?)\\\\~}|\\\\~" str))
    ;; Remove curly braces. Called after other formatting is removed.
    (defun Braces (str / lst origstr tempstr len teststr)
      (setq lst (RE:Execute "{[^\\\\]+}" str))
      (foreach x lst
        (setq origstr (car x)
              tempstr (RE:Replace "" "[{}]" origstr)
              str     (vl-string-subst tempstr origstr str)
        )
      )
      ;; Added JB 12/20/2009
      ;; Last ditch attempt at remove braces from start and end of string.
      (setq len (strlen str))
      (if
        (and
          (= 123 (ascii (substr str 1 1)))
          (= 125 (ascii (substr str len 1)))
          (setq teststr (substr str 2))
          (setq teststr (substr teststr 1 (1- (strlen teststr))))
          (not (vl-string-search "{" teststr))
          (not (vl-string-search "}" teststr))
        )
        (setq str teststr)
      )
      str
    )

    ;;;;; End remove formatting sub-functions ;;;;;
    ;;;;; Start primary function ;;;;;
    ;; Temporarily replace literal backslashes with a unique string.
    ;; Literal backslashes are restored at end of function. By Steve Doman.
    (setq slashflag (strcat "<" (substr (rtos (getvar "CDATE") 2 8) 14) ">"))
    (setq text (RE:Replace slashflag "\\\\\\\\" str))
    ;; Temporarily replace literal left curly brace.
    (setq lbrace (strcat "<L" (substr (rtos (getvar "CDATE") 2 8) 14) ">"))
    (setq text (RE:Replace lbrace "\\\\{" text))
    ;; Temporarily replace literal right curly brace.
    (setq rbrace (strcat "<" (substr (rtos (getvar "CDATE") 2 8) 14) "R>"))
    (setq text (RE:Replace rbrace "\\\\}" text))

    (if (or (vl-position "A" formats)
            (and (vl-position "*" formats) (not (vl-position "^A" formats)))
        )
      (setq text (Alignment text))
    )
    (if (or (vl-position "B" formats)
            (and (vl-position "*" formats) (not (vl-position "^B" formats)))
        )
      (setq text (Tab text))
    )
    (if (or (vl-position "C" formats)
            (and (vl-position "*" formats) (not (vl-position "^C" formats)))
        )
      (setq text (Color text))
    )
    (if (or (vl-position "F" formats)
            (and (vl-position "*" formats) (not (vl-position "^F" formats)))
        )
      (setq text (Font text))
    )
    (if (or (vl-position "H" formats)
            (and (vl-position "*" formats) (not (vl-position "^H" formats)))
        )
      (setq text (Height text))
    )
    (if (or (vl-position "L" formats)
            (and (vl-position "*" formats) (not (vl-position "^L" formats)))
        )
      (setq text (Linefeed text))
    )
    (if (or (vl-position "O" formats)
            (and (vl-position "*" formats) (not (vl-position "^O" formats)))
        )
      (setq text (Overline text))
    )
    (if (or (vl-position "P" formats)
            (and (vl-position "*" formats) (not (vl-position "^P" formats)))
        )
      (setq text (Paragraph text))
    )
    (if (or (vl-position "Q" formats)
            (and (vl-position "*" formats) (not (vl-position "^Q" formats)))
        )
      (setq text (Oblique text))
    )
    (if (or (vl-position "S" formats)
            (and (vl-position "*" formats) (not (vl-position "^S" formats)))
        )
      (setq text (Stacking text))
    )
    (if (or (vl-position "T" formats)
            (and (vl-position "*" formats) (not (vl-position "^T" formats)))
        )
      (setq text (Tracking text))
    )
    (if (or (vl-position "U" formats)
            (and (vl-position "*" formats) (not (vl-position "^U" formats)))
        )
      (setq text (Underline text))
    )
    (if (or (vl-position "W" formats)
            (and (vl-position "*" formats) (not (vl-position "^W" formats)))
        )
      (setq text (Width text))
    )
    (if (or (vl-position "~" formats)
            (and (vl-position "*" formats) (not (vl-position "^~" formats)))
        )
      (setq text (HardSpace text))
    )
    (setq text (Braces (RE:Replace "\\\\" slashflag text))
          text (RE:Replace "\\{" lbrace text)
          text (RE:Replace "\\}" rbrace text)
    )
    text
  ) ; end StripFormat

  ;; Added JB 1/27/2010. Used in the StripColumn function below.
  ;; by Ian Bryant
  ;; Return T if ename is annotative, otherwise nil.
  (defun IsAnnotative (e)
    (and e
      (setq e (cdr (assoc 360 (entget e))))
      (setq e (dictsearch e "AcDbContextDataManager"))
      (setq e (dictsearch (cdr (assoc -1 e)) "ACDB_ANNOTATIONSCALES"))
      (assoc 350 e)
    )
  ) ;end IsAnnotative

  ;; Added JB 1/27/2010. Used in the StripColumn function below.
  ;; Argument: the ename of an annotative object.
  ;; Returns: a list of annotative scales or nil if the object is 
  ;; not annotative.
  (defun GetAnnoScales (e / dict lst rewind res)
    (if
      (and
        e
        (setq dict (cdr (assoc 360 (entget e))))
        (setq lst (dictsearch dict "AcDbContextDataManager"))
        (setq lst (dictsearch (cdr (assoc -1 lst)) "ACDB_ANNOTATIONSCALES"))
        (setq dict (cdr (assoc -1 lst)))
      )
      (progn
        (setq rewind T)
        (while (setq lst (dictnext dict rewind))
          (setq e (cdr (assoc 340 lst))
                res (cons (cdr (assoc 300 (entget e))) res)
                rewind nil
          )
        )
      )
    )
    (reverse res)
  ) ; end GetAnnoScales

  ;; Mtext columns were added in AutoCAD 2008.
  ;; Remove column formatting from an mtext object.
  ;; Argument: mtext vla-object.
  ;; Note: Though the DXF 75 code referenced here does not appear in an
  ;; entget mtext ename call, it can be used to removed column formatting.
  ;; See DXF Reference for mtext objects in 2008 or later.
  (defun StripColumn (obj / ename sclst)
    (if
      (and
        (>= (atof (getvar "AcadVer")) 17.1)
        (eq "AcDbMText" (vlax-get obj 'ObjectName))
        (setq ename (vlax-vla-object->ename obj))
      )
      (cond
        ;; Added JB 1/26/2010.
        ;; Allows columns to be removed from annotative objects.
        ((and
           (IsAnnotative ename)
           (setq sclst (GetAnnoScales ename))
          )
          (setvar "cmdecho" 0)
          (command "._chprop" ename "" "_Annotative" "_No" "")
          (entmod (append (entget ename) '((75 . 0))))
          (command "._chprop" ename "" "_Annotative" "_Yes" "")
          (foreach x sclst
            (command "._objectscale" ename "" "_Add" x "")
          )
          (setvar "cmdecho" 1)
        )
        ;; For non-annotative objects.
        (T
          (entmod (append (entget ename) '((75 . 0))))
        )
      )
    )
  ) ; end StripColumn

  ;; Background mask for mtext objects was added in AutoCAD 2005.
  ;; Remove background mask from mtext and multileader objects.
  ;; Argument: an mtext or multileader ename or vla-object.
  ;; Added support for dimensions.
  (defun StripMask (obj / frame elst maskcode str mbw)
    (cond
      ((and
        (eq "AcDbMText" (vlax-get obj 'ObjectName))
        (vlax-property-available-p obj 'BackgroundFill)
       )
       (vlax-put obj 'BackgroundFill 0)
      )
      ((and
        (wcmatch (vlax-get obj 'ObjectName) "*Dimension*")
        (vlax-property-available-p obj 'TextFill)
       )
       (vlax-put obj 'TextFill 0)
      )
      ((and
        (eq "AcDbMLeader" (vlax-get obj 'ObjectName))
        (vlax-property-available-p obj 'TextFrameDisplay)
        (setq frame (vlax-get obj 'TextFrameDisplay))
        (setq elst (entget (vlax-vla-object->ename obj)))
        (setq maskcode (assoc 292 elst))
        (/= 0 (cdr maskcode))
        (entmod (subst (cons 292 0) maskcode elst))
       )
       (vlax-put obj 'TextFrameDisplay frame)
      )
      ;; Preserve fields.
      ((and
         (eq "AcDbAttribute" (vlax-get obj 'ObjectName))
         ;; check for 90 mask code
         (assoc 90 (entget (vlax-vla-object->ename obj)))
        )
        (if
          ;; If the attribute does not have an extension dictionary or
          ;; the dictionary can be deleted because it is empty.
          (or
            (= 0 (vlax-get obj 'HasExtensionDictionary))
            (not
              (vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-invoke
                  (list (vlax-invoke obj 'GetExtensionDictionary) 'Delete)
                )
              )
            )
          )
          (setq str (SymbolString obj))
          (setq str (GetFields obj nil))
        )
        (setq mbw (vlax-get obj 'MTextBoundaryWidth))
        (vlax-put obj 'MTextAttribute 0)
        (vlax-put obj 'MTextAttribute -1)
        (vlax-put obj 'TextString str)
        (vlax-put obj 'MTextBoundaryWidth mbw)
      )
    )
  ) ; end StripMask

  ;; Fields were added in AutoCAD 2005.
  ;; Remove the fields dictionary from supported object types if it exists.
  ;; Argument: mtext, multiline attribute, mleader or dimension vla-object.
  ;; Returns: the object TextString with symbols intact.
  (defun StripField (obj / typ str dict)
    (setq typ (vlax-get obj 'ObjectName))
    (if
      (or
        (eq typ "AcDbMText")
        (eq typ "AcDbAttribute")
      )
      (setq str (SymbolString obj))
    )
    ;; Added JB 1/29/2008 to fix a problem with fields in multiline
    ;; attributes which do not update correctly when undo is called 
    ;; afer running StripMtext.
    (if (eq typ "AcDbAttribute")
      (command "._updatefield" (vlax-vla-object->ename obj) "")
    )
    (and (= -1 (vlax-get obj 'HasExtensionDictionary))
         (not
           (vl-catch-all-error-p
             (setq dict (vl-catch-all-apply 'vlax-invoke 
               (list obj 'GetExtensionDictionary))
             )
           )
         )
         (not
           (vl-catch-all-error-p
             (vl-catch-all-apply 'vlax-invoke (list dict 'Remove "ACAD_FIELD"))
           )
         )
         (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-invoke (list dict 'Delete))
              )
         )
         str
         (vl-catch-all-apply 'vlax-put (list obj 'TextString str))
    )
    ;; Added 11/14/2009. Return str to StripTableField function.
    str
  ) ; end StripField

  (defun StripTableFields (obj / rows columns rclst row col mtxtobj str)
    (setq rows (vlax-get obj 'Rows)
          columns (vlax-get obj 'Columns)
          rclst (RowsColumns rows columns)
    )
    (vla-put-RegenerateTableSuppressed obj :vlax-true)
    (foreach x rclst
      (setq row (car x) col (cadr x))
      (cond
        ;; Revised JB 1/4/2010.
        ;; Cell is not a text cell.
        ((/= 1 (vlax-invoke obj 'GetCellType row col)))
        ;; Revised JB 1/21/2010
        ;; Cell is locked in 2008 or later. Apparently cells cannot
        ;; be locked in versions prior to 2008.
        ((and
           (vlax-method-applicable-p obj 'GetCellState)
           (/= 0 (vlax-invoke obj 'GetCellState row col))
          )
          (setq lockedcellflag T)
        )
        ((and
           (setq mtxtobj (CellFieldOwner obj row col))
           (setq str (StripField mtxtobj))
          )
          (vlax-invoke obj 'SetText row col str)
        )
      )
    )
    (vla-put-RegenerateTableSuppressed obj :vlax-false)
  ) ; end StripTableFields

  (defun StripTable (obj formats / blocks blkname blkobj rclst row col
                                   str getstr mtxtobjlst temprclst)
    (setq blocks (smt-blocks))
    (setq blkname (cdr (assoc 2 (entget (vlax-vla-object->ename obj)))))
    (setq blkobj (vla-item blocks blkname))
    (vlax-for x blkobj
      (if
        (and
          (eq "AcDbMText" (vlax-get x 'ObjectName))
          (not (eq "" (vlax-get x 'TextString)))
        )
        (setq mtxtobjlst (cons x mtxtobjlst))
      )
    )
    (setq rclst (RowsColumns (vlax-get obj 'Rows) (vlax-get obj 'Columns)))
    (foreach x rclst
      (setq row (car x) col (cadr x))
      (if 
        (and 
          (vlax-method-applicable-p obj 'GetCellState)
          (/= 0 (vlax-invoke obj 'GetCellState row col))
        )
        (setq lockedcellflag T)
      )
      (if (not (eq "" (vlax-invoke obj 'GetText row col)))
        (setq temprclst (cons x temprclst))
      ) 
    )
    (vla-put-RegenerateTableSuppressed obj acTrue)
    ;; The equal test may be temporary. Not sure yet.
    ;; Revised JB 1/24/2010.
    (if (= (length mtxtobjlst) (length temprclst))
      (foreach x mtxtobjlst
        (setq str (SymbolString x))
        (setq row (caar temprclst) col (cadar temprclst))
        (setq str (StripFormat str formats))
        (vlax-put x 'TextString str)
        (setq str (vlax-invoke x 'FieldCode))
        (vl-catch-all-apply 'vlax-invoke
          (list obj 'SetText row col str)
        )
        ;; Step through the list.
        (setq temprclst (cdr temprclst))
      )
    )
    (vla-put-RegenerateTableSuppressed obj acFalse)
  ) ; end StripTable

  (defun StripMLeader (obj formats)
    (if
      ;; If the mleader does not have an extension dictionary or
      ;; the dictionary can be deleted because it is empty.
      (or
        (= 0 (vlax-get obj 'HasExtensionDictionary))
        (not
          (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-invoke
              (list (vlax-invoke obj 'GetExtensionDictionary) 'Delete)
            )
          )
        )
      )
      (vlax-put obj 'TextString (StripFormat (SymbolString obj) formats))
      (progn
        (vlax-put obj 'TextString (GetFields obj formats))
        (setvar "cmdecho" 0)
        (vl-cmdf "._updatefield" (vlax-vla-object->ename obj) "")
        (setvar "cmdecho" 1)
        (vla-update obj)
        (vlax-put obj 'TextFrameDisplay (vlax-get obj 'TextFrameDisplay))
      )
    )
  ) ; end StripMLeader

  ;; Arguments: multiline attribute vla-object and a list of formats to remove.
  (defun StripMAttribute (obj formats)
    (if
      ;; If the attribute does not have an extension dictionary or
      ;; the dictionary can be deleted because it is empty.
      (or
        (= 0 (vlax-get obj 'HasExtensionDictionary))
        (not
          (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-invoke
              (list (vlax-invoke obj 'GetExtensionDictionary) 'Delete)
            )
          )
        )
      )
      (vlax-put obj 'TextString (StripFormat (SymbolString obj) formats))
      (progn
        (vlax-put obj 'TextString (GetFields obj formats))
        (vla-update obj)
      )
    )
  ) ; end StripMAttribute

  ;; Arguments: number of rows and columns in a table.
  ;; Example: (rowscolumns 2 3) > ((0 0) (1 0) (0 1) (1 1) (0 2) (1 2))
  ;; Revised 11/13/2009 to return the list first reading left to right and
  ;; then top to bottom like this ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2))
  (defun RowsColumns (r c / n clst rlst lst)
    (setq n 0)
    (while (< n r)
      (setq rlst (cons n rlst))
      (setq n (1+ n))
    )
    (setq n 0)
    (while (< n c)
      (setq clst (cons n clst))
      (setq n (1+ n))
    )
    (foreach r rlst
      (foreach c clst
        (setq lst (cons (list r c) lst))
      )
    )
  ) ; end RowsColumns

  ;; Thanks to James Allen for pointing out the GetFieldID method.
  ;; Arguments: table vla-object, row and column.
  ;; Returns: the mtext object if the cell contains a field, otherwise nil.
  (defun CellFieldOwner (tblobj row col / doc id owner)
    (setq doc (smt-doc))
    (and
      (setq id (vlax-invoke tblobj 'GetFieldID row col))
      (/= 0 id)
      (setq owner (vlax-invoke doc 'ObjectIDtoObject id))
      (repeat 3
        (setq owner
          (vlax-invoke doc 'ObjectIDtoObject (vlax-get owner 'OwnerID))
        )
      )
    )
    owner
  ) ; end CellFieldOwner

  ;; Argument: ename or vla-object.
  ;; Object types: mtext, attribute, mleader or dimension.
  ;; Returns: a string with symbols intact.
  (defun SymbolString (obj / e typ str name String blocks)
    ;; A multiline attributue may contain two 1 DXF codes and multiple
    ;; 3 DXF codes. In either case the first code 1 should be ingored
    ;; since it contains a string which is not displayed on screen.
    ;; Apparently this odd condition occurs when text is pasted on top
    ;; of existing text. The old text is stored in the first DXF code 1
    ;; and the text displayed on screen is stored in the second DXF code 1.
    (defun String (ename / str lst)
      (setq str "")
      (setq lst
        (vl-remove-if-not
          '(lambda (x) (or (= 3 (car x)) (= 1 (car x)))) (entget ename)
        )
      )
      (if (and (< 1 (length lst)) (= 1 (caar lst)))
        (setq lst (cdr lst))
      )
      (foreach x lst
        (setq str (strcat str (cdr x)))
      )
    ) ; end String

    (if (= (type obj) 'VLA-OBJECT)
      (setq e (vlax-vla-object->ename obj))
      (progn
        (setq e obj)
        (setq obj (vlax-ename->vla-object obj))
      )
    )
    (setq typ (vlax-get obj 'ObjectName))
    (cond
      ((or
         (eq typ "AcDbMText")
         (eq typ "AcDbAttribute")
        )
        (setq str (String e))
      )
      ((eq typ "AcDbMLeader")
        (setq str (cdr (assoc 304 (entget e))))
      )
      ;; Revised SD 1/15/2010. Looks good JB 1/19/2010.
      ((wcmatch typ "*Dimension*")      
        (setq str (cdr (assoc 1 (entget e))))
      )                  
    )
    str
  ) ; end SymbolString

  ;; Argument: multiline attribute or mleader vla-object.
  ;; Called by StripMAttribute and StripMLeader sub-functions.
  ;; Also called by StripMask to preserve fields in a multiline attribute.
  ;; Those functions check the the object has a dictionary or not.
  ;; This is a revised version of a St:GetFields from SwapText.lsp.
  ;; Returns: the same string as the FieldCode method with formatting
  ;; removed. Returns the source text string with formatting removed
  ;; if no fields are found in an attribute or mleader.
  ;; Note, FieldCode does not work with attributes or mleaders.
  ;; Create a new temporary mtext object. Apply source field dictionaries
  ;; to it. Then get the FieldCode from temp object and erase it.
  (defun GetFields (obj formats / srcdict srcdictename srcTEXTdict
                                  srcfieldename targdict targdictename
                                  fieldelst fielddict dicts actlay
                                  tempobj lockflag res doc)
    (setq doc (smt-doc))
    (if
      (and
        (= -1 (vlax-get obj 'HasExtensionDictionary))
        (setq srcdict (vlax-invoke obj 'GetExtensionDictionary))
        (setq srcdictename (vlax-vla-object->ename srcdict))
        (setq srcTEXTdict (dictsearch srcdictename "ACAD_FIELD"))
        (setq srcfieldename (cdr (assoc 360 srcTEXTdict)))
      )
      (progn
        ;; Check for active layer locked.
        (setq actlay (vlax-get doc 'ActiveLayer))
        (if (= -1 (vlax-get actlay 'Lock))
          (progn
            (vlax-put actlay 'Lock 0)
            (setq lockflag T)
          )
        )
        (setq tempobj
          (vlax-invoke
            (vlax-get (vla-get-ActiveLayout doc) 'Block)
              'AddMText '(0.0 0.0 0.0) 0.0 "x"
          )
        )
        (setq targdict (vlax-invoke tempobj 'GetExtensionDictionary)
              targdictename (vlax-vla-object->ename targdict)
              fieldelst (entget srcfieldename)
              ;; not sure about the need for these
              fieldelst (vl-remove (assoc 5 fieldelst) fieldelst)
              fieldelst (vl-remove (assoc -1 fieldelst) fieldelst)
              fieldelst (vl-remove (assoc 102 fieldelst) fieldelst)
              fieldelst (vl-remove-if '(lambda (x) (= 330 (car x))) fieldelst)
        )
        (foreach x fieldelst
          (if (= 360 (car x))
            (progn
              (setq dicts (cons (cdr x) dicts))
            )
          )
        )
        ;; remove all 360s from fieldelst
        (setq fieldelst (vl-remove-if '(lambda (x) (= 360 (car x))) fieldelst))
        (foreach x (reverse dicts)
          (setq fieldelst
            (append fieldelst (list (cons 360 (entmakex (entget x)))))
          )
        )
        (setq fielddict
          (dictadd targdictename "ACAD_FIELD"
            (entmakex
              '(
                (0 . "DICTIONARY")
                (100 . "AcDbDictionary")
                (280 . 1)
                (281 . 1)
              )
            )
          )
        )
        (dictadd fielddict "TEXT"
          (entmakex fieldelst)
        )
        ;; Revised 11/23/2009.
        (vlax-put tempobj 'TextString
          (StripFormat (SymbolString tempobj) formats)
        )
        (setq res (vlax-invoke tempobj 'FieldCode))
        (vla-delete tempobj)
        (if lockflag (vlax-put actlay 'Lock -1))
      ) ; progn
      ;; Else return the text string with formatting removed.
      ;; Unlikely this would be used.
      (setq res (StripFormat (SymbolString obj) formats))
    ) ; if
    res
  ) ; end GetFields

  ;; Author unknown.
  (defun Spinbar (sbar)
    (cond ((= sbar "\\") "|")
          ((= sbar "|") "/")
          ((= sbar "/") "-")
          (t "\\")
    )
  ) ;_end spinbar

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Begin Main StripMtext function ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (vl-load-com)
  (setq formats (FormatsToList formats))
  (setq layers (smt-layers))

  ;; Sort the selection set to lists by object type.
  (setq cnt 0)
  (repeat (sslength ss)
    (setq obj     (vlax-ename->vla-object (ssname ss cnt))
          objname (vlax-get-property obj "ObjectName")
          cnt     (1+ cnt)
    )
    (cond
      ((eq objname "AcDbMText") ;_ Mtext AutoCAD R13+
       (setq mtextobjlst (cons obj mtextobjlst))
      )
      ((and (eq objname "AcDbMLeader") ;_ Mleader AutoCAD 2008+
            (vlax-property-available-p obj 'ContentType)
            (= 2 (vlax-get obj 'ContentType))
       )
       (setq mldrobjlst (cons obj mldrobjlst))
      )
      ((and (eq objname "AcDbBlockReference") ;_ Multiline Atts AutoCAD 2008+
            (vlax-property-available-p obj 'HasAttributes)
            (= -1 (vlax-get obj 'HasAttributes))
            (vlax-method-applicable-p obj 'GetAttributes)
       )
       (foreach x (vlax-invoke obj 'GetAttributes)
         (if
           (and (vlax-property-available-p x 'MTextAttribute)
                (= -1 (vlax-get x 'MTextAttribute))
                (= 0
                   (vlax-get (vla-item layers (vlax-get x 'Layer)) 'Lock)
                )
           )
            (setq mattobjlst (cons x mattobjlst))
         )
       )
      )
      ((vl-position
         objname
         '("AcDbAlignedDimension"
           "AcDbRotatedDimension"
           "AcDbOrdinateDimension"
           "AcDsbAngularDimension"
           "AcsDb2LineAngularDimension"
           "AcDb3PointAngularDimension"
           "AscDbDiametricDimension"
           "AcDbRadialDimension"
           "AcDbRadialDimensionLarge"
           "AcDbArcDimension"
          )
       )
       (setq dimobjlst (cons obj dimobjlst))
      )
      ((eq objname "AcDbTable") ;_ AutoCAD 2005+
       (setq tableobjlst (cons obj tableobjlst))
      )
    )
  )
  ;;
  ;; Parse format list and invoke Strip* functions w/ appropriate arguments
  ;;
  (if (or (vl-position "*" formats) (vl-position "D" formats))
    (progn (foreach x mtextobjlst (StripField x))
           (foreach x mldrobjlst (StripField x))
           (foreach x dimobjlst (StripField x))
           (foreach x mattobjlst (StripField x))
           (foreach x tableobjlst (StripTableFields x))
    )
  )
  (if (or (vl-position "*" formats) (vl-position "N" formats))
    (foreach x mtextobjlst (StripColumn x))
  )
  (if (or (vl-position "*" formats) (vl-position "M" formats))
    (progn (foreach x mtextobjlst (StripMask x))
           (foreach x mldrobjlst (StripMask x))
           (foreach x dimobjlst (StripMask x))
           (foreach x mattobjlst (StripMask x))
    )
  )
  (if (setq formats (vl-remove-if
                      '(lambda (key)
                         (vl-position key '("M" "D" "N" "^M" "^D" "^N"))
                       )
                      formats
                    )
      )
    (progn 
           (setq spinflag (> (length mtextobjlst) 100))
           (foreach x mtextobjlst
             (setq str (StripFormat (SymbolString x) formats))
             (vlax-put x 'TextString str)
             (if spinflag
               (princ (strcat "\rProcessing... "
                              (setq *sbar* (Spinbar *sbar*))
                              "\t"
                      )
               )
             )
           )
           (setq spinflag (> (length mldrobjlst) 100))
           (foreach x mldrobjlst
             (StripMLeader x formats)
             (if spinflag
               (princ (strcat "\rProcessing... "
                              (setq *sbar* (Spinbar *sbar*))
                              "\t"
                      )
               )
             )
           )
           (setq spinflag (> (length dimobjlst) 100))
           (foreach x dimobjlst
             (setq str (StripFormat (SymbolString x) formats))
             (vlax-put-property x 'TextOverride str)
             ;; Added JB 1/19/2010. Updates the dimension object
             ;; which is needed in some cases.
             (entget (vlax-vla-object->ename x))
             (if spinflag
               (princ (strcat "\rProcessing... "
                              (setq *sbar* (Spinbar *sbar*))
                              "\t"
                      )
               )
             )
           )
           (setq spinflag (> (length mattobjlst) 100))
           (foreach x mattobjlst
             (StripMAttribute x formats)
             (if spinflag
               (princ (strcat "\rProcessing... "
                              (setq *sbar* (Spinbar *sbar*))
                              "\t"
                      )
               )
             )
           )
           (setq spinflag (> (length tableobjlst) 25))
           (foreach x tableobjlst
             (StripTable x formats)
             (if spinflag
               (princ (strcat "\rProcessing... "
                              (setq *sbar* (Spinbar *sbar*))
                              "\t"
                      )
               )
             )
           )
    )
  )
  (if lockedcellflag  ;_ this var is created in StripTable
    (princ "\nSome table cells are locked. ")
  )
  ;; calculate count
  (+ (length mtextobjlst)
     (length mldrobjlst)
     (length dimobjlst)
     (length mattobjlst)
     (length tableobjlst)
  )
) ;;; End StripMtext
;;
(princ
  (strcat "\nStripMtext v" *smt-smtver* " by Steve Doman and Joe Burke")
)
(princ "\nStart routine by typing \"STRIPMTEXT\" or \"SMT\" for short.")
(princ)

