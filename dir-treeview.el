;;; dir-treeview.el --- A directory tree browser and simple file manager -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Tilman Rassy

;; Author: Tilman Rassy <tilman.rassy@googlemail.com>
;; URL: https://github.com/tilmanrassy/emacs-dir-treeview
;; Version: 1.4.0
;; Package-Requires: ((emacs "25.1") (treeview "1.3.0"))
;; Keywords: tools, convenience, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Displays the file system as a tree; subtrees can be folded and unfolded
;; A click on a filename loads the file in Emacs
;; A right-click opens a context menu with further actions
;; Can open files by external programs or Lisp functions
;; Can open terminal in directory
;; Can copy, delete, and move files
;; Can change file modes (i.e., permissions, executability, etc.) and file ownerships
;; Provides selection of multiple files to operate on them
;; Works in text mode, too
;; Supports file notifications
;; Highly customizable
;; Themable
;; Ships with one theme called "Pleasant" using Font Awesome icons

;;; Code:

(require 'treeview)
(require 'filenotify)

(defconst dir-treeview-version "1.4.0"
  "Version of the Dir Treeview' package.
See URL `https://github.com/tilmanrassy/emacs-dir-treeview' for more information
on this package.")

(defun dir-treeview-show-version ()
  "Display the Dir Treeview' version in the echo area.
See URL `https://github.com/tilmanrassy/emacs-dir-treeview' for more information
on Dir Treeview."
  (interactive)
  (message "%s" dir-treeview-version))

(defun dir-treeview-join-path (&rest components)
  "Join COMPONENTS to a file path.
COMPONENTS must be a list of strings.  Each element is interpreted as a path
component.  All components are concatenated, separated by the system-specific
file path separator (slash on Linux and Unix)."
  (let ( (path (car components)) )
    (dolist (part (cdr components))
      (setq path (concat (file-name-as-directory path) part)))
    path))

(defgroup dir-treeview nil
  "Customizaton group for Dir Treeview.

See URL `https://github.com/tilmanrassy/emacs-dir-treeview' for more information
on Dir Treeview.

Note that the theme cannot be set by the customizaton system.  To change and
save the theme, use the menu bar or \[treeview-load-theme] and
\[dir-treeview-save-theme]."
  :group 'emacs)

(defcustom dir-treeview-default-root "~/"
  "Default root directory of the tree."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-show-in-side-window nil
  "Whether the tree is shown in a side window."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-indent-unit "  |  "
  "Symbol to indent directories when the parent is not the last child."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-indent-last-unit "     "
  "Symbol to indent directories when the parent is the last child of its parent."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-folded-dir-control "[+]"
  "Control symbol for folded directories."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-expanded-dir-control "[-]"
  "Control symbol for expanded directories."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-file-control " "
  "Control symbol for non-directory files."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-control-margin-left " "
  "Left margin of a control symbol."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-control-margin-right " "
  "Left margin of a control symbol."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-icon-dir-list
  (list (dir-treeview-join-path user-emacs-directory "icons" "dir-treeview"))
  "List of directories to search icons."
  :group 'dir-treeview
  :type '(repeat directory))

(defcustom dir-treeview-default-icon ""
  "Default icon, as an icon specifier.

An icon specifier is a string of the form

  \"char HEXCODE\"

or

  \"image PATH\"

The first from defines a symbol in an icon font (like Font Awesome).  HEXCODE is
the hexadecimal code of the symbol.

The second form defines an image.  PATH is the path of the image file.  The path
should be relative.  The image file is searched with this relative path in the
directories of `dir-treeview-icon-dir-list'."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-folded-dir-icon ""
  "Icon for folded directories, as an icon specifier.
See `dir-treeview-default-icon' for a description of icon specifiers."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-expanded-dir-icon ""
  "Icon for expanded directories, as an icon specifier.
See `dir-treeview-default-icon' for a description of icon specifiers."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-file-icon ""
  "Icon for non-directory files, as an icon specifier.
See `dir-treeview-default-icon' for a description of icon specifiers."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-icon-margin-left ""
  "Left margin of a icon symbol."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-icon-margin-right ""
  "Left margin of a icon symbol."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-dir-label-margin-left ""
  "Left margin of a directory label."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-file-label-margin-left ""
  "Left margin of a file label."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-parent-dir-control ".."
  "Control symbol for the link to the parent directory."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-mode-hook nil
  "Hook which is run at the end of `dir-treeview-mode'.
The latter is automatically called from `dir-treeview-create-buffer' after the
tree has been initialized."
  :group 'dir-treeview
  :type 'hook)

(defcustom dir-treeview-control-keymap
  '(("<mouse-1>" . treeview-toggle-node-state-at-event)
    ("<mouse-2>" . treeview-toggle-node-state-at-event)
    ("<mouse-3>" . dir-treeview-popup-node-menu-at-mouse)
    ("RET" . treeview-toggle-node-state-at-point)
    ("SPC" . treeview-toggle-node-state-at-point))
  "Keymap of the control symbols.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :group 'dir-treeview
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command"))))

(defcustom dir-treeview-label-keymap
  '(("<mouse-1>" . dir-treeview-open-file-at-event)
    ("<mouse-2>" . dir-treeview-open-file-at-event)
    ("<mouse-3>" . dir-treeview-popup-node-menu-at-mouse)
    ("RET" . dir-treeview-open-file-at-point)
    ("<C-down-mouse-1>" . ignore)
    ("<C-mouse-1>" . treeview-toggle-select-node-at-event)
    ("<S-down-mouse-1>" . ignore)
    ("<S-mouse-1>" . treeview-select-gap-above-node-at-event))
  "Keymap of the labels.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :group 'dir-treeview
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command"))))

(defcustom dir-treeview-show-hidden-files
  nil
  "Whether hidden files are shown or not."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-show-backup-files
  t
  "Whether backup files (i.e, files whose names end with a \"~\") are shown or not."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-accept-filename-function
  'dir-treeview-default-accept-filename
  "Function that decides which files are shown and which are not shown.
Called with one argument, the absolute filename.  If returns non-nil, the
corresponding file is shown, otherwise not."
  :group 'dir-treeview
  :type 'function)

(defcustom dir-treeview-show-dirs-first
  t
  "Wether directories are shown before other files."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-compare-filenames-function
  'dir-treeview-default-compare-filenames
  "Function used to sort filenames.
Called with two filenames.  If it returns non-nil, the first filename is listed
after the second."
  :group 'dir-treeview
  :type 'function)

(defcustom dir-treeview-image-regexp
  "\\.\\(?:gif\\|jpe?g\\|png\\|tiff\\|xcf\\|xpm\\|svgz?\\)$"
  "Regular expression for image filenames.
This is used by `dir-treeview-is-image-p'."
  :group 'dir-treeview
  :type 'regexp)

(defcustom dir-treeview-audio-regexp
  "\\.\\(?:mp3\\|ogg\\|wav\\|mid\\|aiff?\\|webm\\|flac\\)$"
  "Regular expression for audio filenames.
This is used by `dir-treeview-is-audio-p'."
  :group 'dir-treeview
  :type 'regexp)

(defcustom dir-treeview-video-regexp
  "\\.\\(?:mp4\\|mpg\\|mov\\|\\|ogv\\|wmv\\|avi\\|mts\\)$"
  "Regular expression for video filenames.
This is used by `dir-treeview-is-video-p'."
  :group 'dir-treeview
  :type 'regexp)

(defcustom dir-treeview-use-file-dialog nil
  "Whether a graphical file dialog is used for selecting files."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-archive-regexp
  "\\.\\(?:tar\\.gz\\|tgz\\|zip\\)$"
  "Regular expression for archive filenames.
This is used by `dir-treeview-is-archive-p'."
  :group 'dir-treeview
  :type 'regexp)

(defcustom dir-treeview-is-text-p-function
  'dir-treeview-is-text-by-file-cmd-p
  "Function to check if a file is a text file.
Called with one argument, the absolute filename.  If returns non-nil, the
corresponding file is regarded as a text file, otherwise as a binary file."
  :tag "Dir-Treeview Is-Text Predicate Function"
  :group 'dir-treeview
  :type 'function)

(defcustom dir-treeview-is-openable-in-editor-p-function
  (lambda (_filename) t)
  "Function to recommend whether a file can be opened in EXmacs or not."
  :tag "Dir-Treeview Is-Openable-In-Editor Predicate Function"
  :group 'dir-treeview
  :type 'function)

(defcustom dir-treeview-special-icons
  ()
  "Icons for special types of files.

This variable is an alist, thus, a list of key-value pairs.  The keys are
testers, the values are icon specifiers.

See `dir-treeview-default-icon' for a description of icon specifiers.

Each tester must be either a regular expression or a Lisp function.  It
decides whether the corresponding icon should be used for a given node or not.
If the tester is a regular expression, the icon is used if, and only if, the
absolute filename of the node matches that regular expression.  If the tester
is a Lisp function, it is called with the absolute filename of the node as
argument.  The icon is used if, and only if, the function returns a non-nil
value.

When determining the icon for a given node, Emacs iterates through the list and
applies the testers.  If a tester yields a positive result, the corresponding
icon is used and the iteration is stopped.  If no tester yields a positive
result, `dir-treeview-default-icon' is used as the icon provided it is not nil.
If `dir-treeview-default-icon' is nil, the node doesn't get an icon."
  :group 'dir-treeview
  :type '(repeat (cons :tag "Icon Symbol"
                       (choice :tag "Tester"
                               :format "%t: %[Select Type%] %v"
                               regexp function)
                       (string :tag "Symbol  "
                               :format "%t: %v\n") )))

(defcustom dir-treeview-icon-faces
  '((file-directory-p . dir-treeview-directory-icon-face)
    (file-executable-p . dir-treeview-executable-icon-face)
    (dir-treeview-is-archive-p . dir-treeview-archive-icon-face)
    (dir-treeview-is-image-p . dir-treeview-image-icon-face)
    (dir-treeview-is-audio-p . dir-treeview-audio-icon-face)
    (dir-treeview-is-video-p . dir-treeview-video-icon-face))
  "Faces to highlight icons."
  :group 'dir-treeview
  :type '(repeat (cons :tag "Icon Face"
                       (choice :tag "Tester"
                               :format "%t: %[Select Type%] %v"
                               regexp function)
                       (face :tag "Face  "
                             :format "%t: %[Select Face%] %v\n") )))

(defcustom dir-treeview-filename-faces
  '((file-symlink-p . dir-treeview-symlink-face)
    (file-directory-p . dir-treeview-directory-face)
    (file-executable-p . dir-treeview-executable-face)
    (dir-treeview-is-archive-p . dir-treeview-archive-face)
    (dir-treeview-is-image-p . dir-treeview-image-face)
    (dir-treeview-is-audio-p . dir-treeview-audio-face)
    (dir-treeview-is-video-p . dir-treeview-video-face))
  "Faces to highlight filenames.

This variable is an alist, thus, a list of key-value pairs.  The keys are
'testers', the values are faces (symbols denoting face names).

Each tester must be either a regular expression or a Lisp function.  It
decides whether the corresponding face is applied to the filename of a given
node or not.  If the tester is a regular expression, the face is applied if,
and only if, the absolute filename of the node matches that regular expression.
If the tester is a Lisp function, it is called with the absolute filename of
the node as argument.  The face is applied if, and only if, the function returns
a non-nil value.

When determining the face for a given node, Emacs iterates through the list and
applies the testers.  If a tester yields a positive result, the corresponding
face is used and the iteration is stopped.  If no tester yields a positive
result, `dir-treeview-default-filename-face' is used."
  :group 'dir-treeview
  :type '(repeat (cons :tag "Filename Face"
                       (choice :tag "Tester"
                               :format "%t: %[Select Type%] %v"
                               regexp function)
                       (face :tag "Face  "
                             :format "%t: %[Select Face%] %v\n") )))

(defcustom dir-treeview-terminal-program ""
  "The program to open a terminal.
Used in `dir-treeview-open-terminal'."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-split-path-regexp (if (memq system-type '(ms-dos 'windows-nt)) "[\\/]" "/")
  "Regular expression to split a file path into its components.
On Unix-like systems including Linux and Mac OS X, path components are separated
by a slash.  Thus, the regular expression should be \"/\".  On Windows, path
components can be separated by either a slash or a backslash.  Thus, the regular
expression should be \"[\\/]\"."
  :group 'dir-treeview
  :type 'regexp)

(defcustom dir-treeview-open-commands
  
  '((dir-treeview-is-image-p "gimp" ("gimp"))
    (dir-treeview-is-image-p "gwenview" ("gwenview"))
    ("\\.svgz?$" "inkscape" ("inkscape"))
    ("\\.x?html$" "Default browser" (browse-url))
    ("\\.x?html$" "Chrome" ("google-chrome"))
    ("\\.x?html$" "Chrome" ("google-chrome-stable"))
    ("\\.x?html$" "Chromium" ("chromium"))
    ("\\.x?html$" "Firefox" ("firefox"))
    ("\\.\\(?:pdf\\|dvi\\|ps\\)$" "okular" ("okular"))
    ("\\.\\(?:pdf\\|dvi\\|ps\\)$" "evince" ("evince"))
    ("\\.pdf$" "acroread" ("acroread"))
    ("\\.\\(?:od[tsgmpfb]\\|ot[thsgp]\\|oxt\\|doc[xm]?\\|xls[xm]?\\|ppt[xm]?\\)$" "libreoffice" ("libreoffice"))
    ("\\.\\(?:ogg\\|mp[34]\\|mpe?g\\|avi\\|mov\\|qt\\|mts\\)$" "vlc" ("vlc")))
  
  "List of programs and functions to open files.

This list controls the entries of the 'Open with ...' submenu of the popup menu
of a node.  Each entry consists of a 'tester' and an 'action'.  The tester
controls whether the submenu entry is shown, and the action controls what
happens when the entry is clicked.

The tester must be either a regular expression or a Lisp function.  If a regular
expression, the entry is excluded from the submenu when the absolute filename of
the node doesn't match the regular expression.  If the tester is a Lisp
function, the function is called with the absolute filename of the node as
argument, and the entry is excluded from the submenu if the function returns
nil.

The action may be either an external program or a Lisp function.  An external
program consists of an executable name and, optionally, a list of parameters.
If the action is an external program, the corresponding executable is called
with the specified parameters (if any) and the absolute filename of the node
added as the last parameter.  If the action is a Lisp function, the function
is called with one argument, the absolute filename of the node.

If the action is an external program, but the executable doesn't exist, the
entry is excluded from the submenu."
  
  :group 'dir-treeview
  :type '(repeat (list (choice :format                 "Tester Type      : %[Select Type%]\n            %v"
                               (regexp :tag            "Test Regexp      ")
                               (function :tag          "Test Function    "))
                       (string :tag                    "Name             ")
                       (choice :format                 "Command Type     : %[Select Type%]\n            %v"
                               (list :tag              "External Program"
                                     :format           "-- External Program --\n%v"
                                     (string :tag      "Program Name     ")
                                     (repeat :tag      "Parameters       "
                                             (string :tag "     ")))
                               (list :tag              "Function"
                                     :format           "-- Function --\n%v"
                                     (function :tag    "Function Name    ")
                                     (repeat :tag      "Parameters       "
                                             (sexp :tag"     ")))))))

(defcustom dir-treeview-get-node-menu-function
  'dir-treeview-get-default-node-menu
  "Function which creates the popup menu of a node.
Called with one argument, the node for which to popup the menu.  Should return
a menu-specifying object as described in the GNU Emacs Lisp Reference Manual."
  :group 'dir-treeview
  :type 'function)

(defcustom dir-treeview-use-file-watch t
  "Whether file watch is switched on automatically.

If file watch is activated, Emacs will be notified about file system changes,
and update all Dir Treeview buffers accordingly.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-log-file-notify-events nil
  "Whether file notification events are logged.

If enabled, each file notification event is logged.
See `dir-treeview-log-file-notify-event' for more information."
  :group 'dir-treeview
  :type 'boolean)

(defcustom dir-treeview-time-format "%Y-%m-%d %H:%M:%S %3N"
  "Format string for displaying timestamps."
  :group 'dir-treeview
  :type 'string)

(defcustom dir-treeview-sudo-function 'dir-treeview-sudo-tramp
  "Function to run an external program as root.

Dir Treeview provides three predefined functions for this, which differ in the
way they handle prompting for the password:

`dir-treeview-sudo-tramp' - Uses Tramp with method \"sudo\".  The password is
read in the  minibuffer.

`dir-treeview-sudo-nopasswd' - Uses `sudo´ without any arrangements to provide a
password.  Choose this only if `sudo´ is configured in such a way that it doesn't
ask for a password.

`dir-treeview-pkexec' - Uses `pkexec'.  The password is read in a graphical popup
window.

See the documentations of the functions for more information.  The first two
functions doesn't require a graphical display, and are therefore possible
values for `dir-treeview-sudo-function-textmode', too.

If you want to write your own function, it must accept the following arguments:

CMD - The command to run, possibly with path.  A string.

DIR - Directory of the external process.  A string or nil.  If nil, the value of
`default-directory' should be used as the directory. of the process.

ARGS - The arguments passed to the executable.  A list of strings.  May be
defined as a &rest argument.

The function should run the command in a synchronious external process."
  :group 'dir-treeview
  :type 'function)

(defcustom dir-treeview-sudo-function-textmode 'dir-treeview-sudo-tramp
  "Function to run an external program as root when in text mode.
This variable is used instead of `dir-treeview-sudo-function' if no graphical
display is available, or, more precisely, if (`display-graphic-p') returns nil.
The documentation of `dir-treeview-sudo-function' also applies to this variable,
except that only functions suitable for text mode should be set als value."
  :group 'dir-treeview
  :type 'function)

(defface dir-treeview-default-icon-face
  ()
  "Default face to highlight icons."
  :group 'dir-treeview)

(defface dir-treeview-directory-icon-face
  '((t (:inherit dir-treeview-default-icon-face)))
  "Face to highlight directory icons."
  :group 'dir-treeview)

(defface dir-treeview-executable-icon-face
  '((t (:inherit dir-treeview-default-icon-face)))
  "Face to highlight executable icons."
  :group 'dir-treeview)

(defface dir-treeview-archive-icon-face
  '((t (:inherit dir-treeview-default-icon-face)))
  "Face to highlight archive icons."
  :group 'dir-treeview)

(defface dir-treeview-image-icon-face
  '((t (:inherit dir-treeview-default-icon-face)))
  "Face to highlight image icons."
  :group 'dir-treeview)

(defface dir-treeview-audio-icon-face
  '((t (:inherit dir-treeview-default-icon-face)))
  "Face to highlight audio file icons."
  :group 'dir-treeview)

(defface dir-treeview-video-icon-face
  '((t (:inherit dir-treeview-default-icon-face)))
  "Face to highlight video icons."
  :group 'dir-treeview)

(defface dir-treeview-indent-face
  ()
  "Face to highlight the indentation."
  :group 'dir-treeview)

(defface dir-treeview-control-face
  ()
  "Face to highlight control symbols."
  :group 'dir-treeview)

(defface dir-treeview-control-mouse-face
  '((t (:background "#C1FFC1")))
  "Face to highlight control symbols when the mouse is over them."
  :group 'dir-treeview)

(defface dir-treeview-label-mouse-face
  '((t (:background "#C1FFC1")))
  "Face to highlight labels when the mouse is over them."
  :group 'dir-treeview)

(defface dir-treeview-start-dir-face
  '((t (:background "#D9D9D9")))
  "Face to highlight the start directory name."
  :group 'dir-treeview)

(defface dir-treeview-default-filename-face
  ()
  "Default face to highlight filenames."
  :group 'dir-treeview)

(defface dir-treeview-directory-face
  '((t (:inherit dir-treeview-default-filename-face :foreground "#27408B")))
  "Face to highlight directories."
  :group 'dir-treeview)

(defface dir-treeview-executable-face
  '((t (:inherit dir-treeview-default-filename-face :foreground "#CD3333")))
  "Face to highlight executables."
  :group 'dir-treeview)

(defface dir-treeview-symlink-face
  '((t (:inherit dir-treeview-default-filename-face :underline t)))
  "Face to highlight symbolic links."
  :group 'dir-treeview)

(defface dir-treeview-archive-face
  '((t (:inherit dir-treeview-default-filename-face :foreground "#8B0000")))
  "Face to highlight archives."
  :group 'dir-treeview)

(defface dir-treeview-image-face
  '((t (:inherit dir-treeview-default-filename-face :foreground "#8B008B")))
  "Face to highlight images."
  :group 'dir-treeview)

(defface dir-treeview-audio-face
  '((t (:inherit dir-treeview-default-filename-face :foreground "#0f6464")))
  "Face to highlight audio files."
  :group 'dir-treeview)

(defface dir-treeview-video-face
  '((t (:inherit dir-treeview-default-filename-face :foreground "#56338a")))
  "Face to highlight video files."
  :group 'dir-treeview)

(defface dir-treeview-select-face
  '((t (:background "DarkSlateGray1")))
  "Face to highlight selected nodes.")

(defface dir-treeview-highlight-face
  '((t (:background "GreenYellow")))
  "Face to highlicht a node.")
(defface dir-treeview-link-target-face
  ()
  "Face to highlight link targets."
  :group 'dir-treeview)

(defface dir-treeview-file-mode-face
  '((t (:foreground "#663399")))
  "Face to highlight file mode characters."
  :group 'dir-treeview)

(defface dir-treeview-file-owner-face
  '((t (:foreground "DeepPink4")))
  "Face to highlight file owner."
  :group 'dir-treeview)

(defface dir-treeview-file-group-face
  '((t (:foreground "DeepPink4")))
  "Face to highlight file group."
  :group 'dir-treeview)

(defface dir-treeview-file-size-face
  '((t (:foreground "RoyalBlue")))
  "Face to highlight file size."
  :group 'dir-treeview)

(defface dir-treeview-file-time-face
  '((t (:foreground "OrangeRed3")))
  "Face to highlight file times."
  :group 'dir-treeview)

(defvar dir-treeview-start-node nil
  "The root node of the visible part of the tree.")

(make-variable-buffer-local 'dir-treeview-start-node)

(define-error 'dir-treeview-invalid-major-mode-error
  "Invalid major mode (expected dir-treeview-mode)" 'treeview-error)

(define-error 'dir-treeview-no-parent-dir-error
              "No parent directory" 'treeview-error)

(defun dir-treeview-get-node-absolute-name (node)
  "Return the absolute filename of NODE."
  (treeview-get-node-prop node 'absolute-name))

(defun dir-treeview-user-confirm-y-or-n (prompt)
  "Ask the user for confirmation in the minibuffer.
PROMPT is the prompt.  It should end with a whitspace.  The input must by \"y\"
or \"n\".  If the input is something else, the user is requested to repeat the
input, until the result is \"y\" or \"n\".  No <RET> is required to terminate
the input.  The input is automatically terminated after the first character,
more precisely, after the first input event.  The function returns non-nil
if the (final) input is \"y\" and nil if the final input is \"n\"."
  (interactive)
  (let ( (input (read-event (concat prompt "(y or n) "))) )
    (while (not (member input '(?y ?n)))
      (setq input (read-event (concat "Please answer y or n - " prompt))))
    (equal input ?y)))

(defun dir-treeview-user-confirm-y-n-Y-N (prompt)
  "Ask the user for confirmation in the minibuffer.
Similar to `dir-treeview-user-confirm-y-or-n', but the user can in addition
type \"Y\" or \"N\", meaning \"yes to all\" or \"no to all\", respectively.
This is useful if the same yes-no-question is asked for a series of items, and
the user wants to give the same answer to all of them.  A typical example would
be the deletion of a series of files.
As with `dir-treeview-user-confirm-y-or-n', PROMPT is the prompt.
Unlike `dir-treeview-user-confirm-y-or-n', this function returns the answer as
a character (\"y\", \"n\", \"Y\", or \"N\")."
  (interactive)
  (let ( (input (read-event (concat prompt "(y, n, Y [for y to all], N [for n to all]) "))) )
    (while (not (member input '(?y ?n ?Y ?N)))
      (setq input (read-event (concat "Please answer y, n, Y, or N - " prompt))))
    input))

(defun dir-treeview-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Read a filename, either in the minibuffer or a graphical dialog.
The arguments PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and PREDICATE
have the same meaning as the respective arguments of `read-file-name'.  The
latter is called by this function.  The only difference to `read-file-name' is
the way how it is controlled whether a graphical file dialog is used or not.
With this function, if `dir-treeview-use-file-dialog' is non-nil and the
function was invoked by a mouse command, a graphical file dialog is used.
Otherwise, the filename is read in the minibuffer."
  (interactive)
  (let ( (use-file-dialog dir-treeview-use-file-dialog) )
    (read-file-name prompt dir default-filename mustmatch initial predicate)))

(defun dir-treeview-read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  "Read a directory, either in the minibuffer or a graphical dialog.
The arguments PROMPT, DIR, DEFAULT-DIRNAME, MUSTMATCH, and INITIAL have the
same meaning as the respective arguments of `read-directory-name'.  The latter
is called by this function.  The only difference to `read-directory-name' is
the way how it is controlled whether a graphical file dialog is used or not.
With this function, if `dir-treeview-use-file-dialog' is non-nil and the
function was invoked by a mouse command, a graphical file dialog is used.
Otherwise, the directory is read in the minibuffer."
  (interactive)
  (let ( (use-file-dialog dir-treeview-use-file-dialog) )
    (file-name-as-directory
     (read-directory-name prompt dir default-dirname mustmatch initial))))

(defun dir-treeview-read-new-file-name (prompt dir)
  "Read a name that is not an existing filename in the directory DIR.
Prompts with PROMPT.  Intended for reading the name of a new file to be created.
Uses `dir-treeview-read-file-name' internally."
  (setq dir (file-name-as-directory dir))
  (let( (filename (dir-treeview-read-file-name prompt dir "" nil "")) )
    (setq filename (expand-file-name filename dir))
    (when (file-exists-p filename)
      (user-error "File \"%s\" already exists" filename))
    (unless (equal (file-name-directory filename) dir)
      (user-error "Path \"%s\" doesn't denote a file in this directory" filename))
    filename))

(defun dir-treeview-char-code-to-symbol (code)
  "Return the symbol for the hexadecimal character code CODE.
CODE should be a string describing an hexadecimal number.  The return value is
a string containing a single character, namely the character corresponing to the
numerical code specified by CODE."
  (string (string-to-number code 16)))

(defun dir-treeview-is-image-p (filename)
  "Return non-nil if file FILENAME is an image, otherwise nil.
Uses `dir-treeview-image-regexp', i.e., returns non-nil if FILENAME matches that
regular expression."
  (string-match dir-treeview-image-regexp filename))

(defun dir-treeview-is-audio-p (filename)
  "Return non-nil if file FILENAME is an audio file, otherwise nil.
Uses `dir-treeview-audio-regexp', i.e., returns non-nil if FILENAME matches that
regular expression."
  (string-match dir-treeview-audio-regexp filename))

(defun dir-treeview-is-video-p (filename)
  "Return non-nil if file FILENAME is an video file, otherwise nil.
Uses `dir-treeview-video-regexp', i.e., returns non-nil if FILENAME matches that
regular expression."
  (string-match dir-treeview-video-regexp filename))

(defun dir-treeview-is-archive-p (filename)
  "Return non-nil if file FILENAME is an archive, otherwise nil.
Uses `dir-treeview-archive-regexp', i.e., returns non-nil if FILENAME matches
that regular expression."
  (string-match dir-treeview-archive-regexp filename))

(defun dir-treeview-is-text-by-file-cmd-p (filename)
  "Return non-nil if FILENAME is a text file according to the \"file\" command.
Otherwise, return nil.  Runs the external program \"file\", which must be
installed on your computer.  Calls

  file --brief FILENAME

and tests the output for the word \"text\".  If found, returns non-nil,
otherwise nil (see also the manpage of the \"file\" program). This function is
the default implementation of `dir-treeview-is-text-p-function'."
  (with-current-buffer (generate-new-buffer "*dir-treeview-is-text-p*")
    (let ( (is-text-p (and (eq (call-process "file" nil t nil "--brief" filename) 0)
                           (progn (goto-char 0) (re-search-forward "\\btext\\b" nil t)))) )
      (kill-this-buffer)
      is-text-p)))

(defun dir-treeview-is-text-p (filename)
  "Return non-nil if FILENAME is a text file.
Calls `dir-treeview-is-text-p-function' with FILENAME as argument."
  (funcall dir-treeview-is-text-p-function filename))

(defun dir-treeview-local-filename (filename)
  "Return the local part of FILENAME.
This is the part of FILENAME without the path of the directory FILENAME is in.

Recall that filenames consist of several parts, each of which denotes a
directory, except the last one, which denotes a directory or a (non-directory)
file.  The parts are seprated by \"path part separators\".  On Unix or Linux,
the path part separator is a slash.

This function returns the last path part.  A terminating path part separator is
ignored and stripped.

Examples (Unix/Linux):

filename              | local part
----------------------+-----------
/foo/bar/dir/file.xml | file.xml
/foo/bar/dir/         | dir
dir/file.xml          | file.xml"
  (file-name-nondirectory (directory-file-name filename)))

(defun dir-treeview-relative-filename (filename dirname)
  "Return the path of FILENAME relative to DIRNAME.
Both FILENAME and DIRNAME may be absolute or relative paths.  If DIRNAME denotes
an ancestor directory of FILENAME, the function returns the path of FILENAME
relative to DIRNAME.  Otherwise, the function returns nil.

Examples (Unix/Linux):

filename                  | dirname    | return value
--------------------------+------------+-------------
/foo/bar/bazz/aaa/bbb.txt | /foo/bar   | bazz/aaa/bbb.txt
/foo/bar/bazz/aaa/bbb.txt | /foo/bar/  | bazz/aaa/bbb.txt
/foo/bar/bazz/aaa/bbb.txt | /x/y       | <nil>"
  (setq dirname (file-name-as-directory dirname))
  (if (string-prefix-p dirname filename) (substring filename (length dirname))))

(defun dir-treeview-parent-filename (filename)
  "Return the parent of FILENAME.
This is the filename of the directory containing FILENAME.  If FILENAME doesn't
specify a parent, return nil.  This happens only if FILENAME is a local filename
with no additional parts."
  (directory-file-name (file-name-directory (directory-file-name filename))))

(defun dir-treeview-new-node (absolute-name &optional parent children)
  "Create and return a new node.
The arguments have the following meanings:

ABSOLUTE-NAME: The name of the new node.  Should be a string denoting an
               absolute filename.
PARENT:        The parent of the new node.  Should be another node.  If omitted,
               the parent of the new node is nil.
CHILDREN:      The children of the new node.  Should be a list of nodes.
               If omitted, the children list of the new node is nil."
  (let ( (node (treeview-new-node)) )
    (treeview-set-node-name node (dir-treeview-local-filename absolute-name))
    (treeview-set-node-prop node 'absolute-name (directory-file-name absolute-name))
    (treeview-set-node-parent node parent)
    (treeview-set-node-children node children)
    node))

(defun dir-treeview-directory-p (node)
  "Return non-nil if NODE represends a directory."
  (file-directory-p (dir-treeview-get-node-absolute-name node)))

(defun dir-treeview-node-leaf-p (node)
  "Return non-nil if NODE is a leaf node.
This function is the implementation of `treeview-node-leaf-p-function' in
Dir Treeview.  It returns non-nil if, and only if, NODE represends a file which
is not a directory.  Note that this function considers empty directories
not as leaf nodes."
  (not (file-directory-p (dir-treeview-get-node-absolute-name node))))

(defun dir-treeview-get-indent (node)
  "Return the indentation of NODE."
  (let ( (indent ())
         (parent nil) )
    (while (setq parent (treeview-get-node-parent node))
      (setq indent (cons (if (treeview-last-child-p parent)
                             dir-treeview-indent-last-unit
                             dir-treeview-indent-unit)
                         indent)
            node parent))
    indent))

(defun dir-treeview-get-control (node)
  "Return the control symbol for NODE.
If NODE represents a directory, `dir-treeview-folded-dir-control' or
`dir-treeview-expanded-dir-control' is returned depending on whether the
directory is folded or not.  Otherwise, nil (meaning: no control) is returned."
  (if (dir-treeview-directory-p node)
      (if (treeview-node-folded-p node) dir-treeview-folded-dir-control
        dir-treeview-expanded-dir-control)))

(defun dir-treeview-query-tester-value-list (node tester-value-list)
  "Return the value for NODE according TESTER-VALUE-LIST."
  (let ( (filename (dir-treeview-get-node-absolute-name node))
         (value nil) )
    (while (and (not value) tester-value-list)
      (let* ( (item (car tester-value-list))
              (tester (car item))
              (candidate (cdr item)) )
        (if (or (and (functionp tester) (funcall tester filename))
                (and (stringp tester) (string-match tester filename)))
            (setq value candidate))
        (setq tester-value-list (cdr tester-value-list))))
    value))

(defun dir-treeview-add-icon-dir-by-load-file-name ()
  "Register the icon directory deduced from `load-file-name'.
If `load-file-name' is set, the path \"LOAD_FILE_DIRECTORY/icons\" is added to
`dir-treeview-icon-dir-list' provided it is not already contained, where
LOAD_FILE_DIRECTORY is the dirfectory part of `load-file-name'.

Note that `load-file-name', if set, contains the path of the currently loaded
Lisp file.  If the latter is part of a package, and the package provides icons
in an \"icons\" subfolder, this function can be called to add the subfolder
to `dir-treeview-icon-dir-list' so the icons are found by
`dir-treeview-resolve-icon-image-path'."
  (when load-file-name
    (add-to-list 'dir-treeview-icon-dir-list (concat (file-name-as-directory (file-name-directory load-file-name)) "icons"))))

(defun dir-treeview-resolve-icon-image-path (icon-path)
  "Return the absolute path of the icon with the relative path ICON-PATH.
For each directory in `dir-treeview-icon-dir-list', it is checked if the file
with the path ICON-PATH relative to that directory exists.  The absolute path of
the first existing file is returned.  If no existing file is found, nil is
returned."
  (let ( (icon-path-abs (when (file-name-absolute-p icon-path) icon-path)) (dirs dir-treeview-icon-dir-list) )
    (while (and (not (and icon-path-abs (file-exists-p icon-path-abs))) dirs)
      (setq icon-path-abs (dir-treeview-join-path (car dirs) icon-path) dirs (cdr dirs)))
    icon-path-abs))

(defun dir-treeview-get-icon (node)
  "Return the icon symbol for NODE.
If no icon is specified for NODE, return nil.

If not nil, the return value is either a string or an image.  If a string, it
has exactly one character.  This character is displayed in the face returned by
`dir-treeview-get-icon-face', which should specify an icon font (e.g., Font
Awesome), so that the character corresponds to the desired icon.  If an image,
that image is the icon.

The output of this function is controlled by the customizable variables
`dir-treeview-folded-dir-icon', `dir-treeview-expanded-dir-icon',
`dir-treeview-default-icon', and `dir-treeview-special-icons'.  If NODE
represents a directory, the icon specified by `dir-treeview-folded-dir-icon'
or `dir-treeview-expanded-dir-icon' is returned, depending on whether the
directory is folded or not.  Otherwise, if the alist
`dir-treeview-special-icons' specifies an icon for NODE, that is returned.
Otherwise, if `dir-treeview-default-icon' is not nil, that icon is returned.
Otherwise, nil is returned, in which case the node doesn't get an icon.

Note that all the customizable variables mentioned above don't specify the
icons directly, but as strings containing the hexadecimal character codes."
  (let (icon-def type value)
    (setq icon-def
          (if (dir-treeview-directory-p node)
              (if (treeview-node-folded-p node) dir-treeview-folded-dir-icon dir-treeview-expanded-dir-icon)
            (or (dir-treeview-query-tester-value-list node dir-treeview-special-icons) dir-treeview-default-icon)))
    (when (treeview-not-nil-or-empty-string-p icon-def)
      (setq icon-def (string-trim icon-def))
      (if (string-match "^\\(char\\|image\\)[[:space:]]+\\([^[:space:]].*\\)$" icon-def)
          (setq type (match-string 1 icon-def) value (match-string 2 icon-def))
        ;; Legacy support:
        (setq type "char" value icon-def))
      (if (string-equal type "char")
          (dir-treeview-char-code-to-symbol value)
        (create-image (dir-treeview-resolve-icon-image-path value) nil nil :ascent 'center)) )) )

(defun dir-treeview-get-label-margin-left (node)
  "Return the left margin of the label of NODE.
This function is the Dir Treeview implementation of
`treeview-get-label-margin-left-function'.  It returns
`dir-treeview-file-label-margin-left' if NODE is a leaf node according to
`dir-treeview-node-leaf-p', otherwise it returns
`dir-treeview-dir-label-margin-left'."
  (if (dir-treeview-node-leaf-p node)
      dir-treeview-file-label-margin-left dir-treeview-dir-label-margin-left))
 
(defun dir-treeview-get-label (node)
  "Return the string suitable as the label of NODE.
This function is the Dir Treeview implementation of
`treeview-get-label-function'.  It simply returns the name of NODE."
  (treeview-get-node-name node))

(defun dir-treeview-get-icon-face (node)
  "Return the face for the icon of NODE.

If `dir-treeview-icon-faces' specifies an icon face for NODE, that face is
returned.  Otherwise, `dir-treeview-default-icon-face' is returned.

See also the documentation of `dir-treeview-icon-faces' for more information"
  (or (dir-treeview-query-tester-value-list node dir-treeview-icon-faces) 'dir-treeview-default-icon-face))

(defun dir-treeview-get-control-keymap (node)
  "Return the keymap for the control symbol of NODE.
If NODE belongs to a directory, returns the keymap defined by
`dir-treeview-control-keymap'.  Otherwise, returns nil."
  (if (dir-treeview-directory-p node) (treeview-make-keymap dir-treeview-control-keymap)))

(defun dir-treeview-get-label-keymap (_node)
  "Return the keymap for the label of _NODE.
The keymap is defined by `dir-treeview-label-keymap'."
  (treeview-make-keymap dir-treeview-label-keymap))

(defun dir-treeview-get-label-face (node)
  "Return the face for the label of NODE.
The face is determined by `dir-treeview-filename-faces'.  See the documentation
of `dir-treeview-filename-faces' for more information."
    (or (dir-treeview-query-tester-value-list node dir-treeview-filename-faces) 'dir-treeview-default-filename-face))

(defun dir-treeview-default-accept-filename (filename)
  "Return non-nil if FILENAME should be shown in the tree, otherwise nil.
Default implementation of `dir-treeview-accept-filename-function'.
Accepts all filenames except those which fullfill at least one of the following
conditions:
- The local name is \".\"
- The local name is \"..\"
- `dir-treeview-show-hidden-files' is nil and the local name starts with a
                              dot (\".\")
- `dir-treeview-show-backup-files' is nil and the local name ends with \"~\""
  (let ( (local-name (dir-treeview-local-filename filename)) )
    (not (or (equal local-name ".")
             (equal local-name "..")
             (and (not dir-treeview-show-hidden-files) (string-match "^\\." local-name))
             (and (not dir-treeview-show-backup-files) (string-match "~$" local-name)) ))))

(defun dir-treeview-accept-filename (filename)
  "Return non-nil if FILENAME should be shown in the tree, otherwise nil.
Calls the function stored in the customizable variable
`dir-treeview-accept-filename-function'."
  (funcall dir-treeview-accept-filename-function filename))

(defun dir-treeview-filter-dir-contents (contents)
  "Return the elements of CONTENTS that should be shown in the tree.
CONTENTS should be a list of filenames.  Typically it is a list of all filenames
in  a directoy.  The function returns a list of all elements of CONTENTS for
which `dir-treeview-accept-filename-function' returns true."
  (let ( (filtered-contents ()) )
    (while contents
      (let ( (filename (car contents)) )
        (if (dir-treeview-accept-filename filename)
              (setq filtered-contents (cons filename filtered-contents)))
        (setq contents (cdr contents))))
    filtered-contents))

(defun dir-treeview-default-compare-filenames (filename-1 filename-2)
  "Return non-nil if FILENAME-1 is less than FILENAME-2 in default sort order.
This is the default implementation of `dir-treeview-compare-filenames-function'.
The sort order implied by this function depends on the value of the boolean
variable `dir-treeview-show-dirs-first'.  If it is nil, the sorting is simply
lexicographic.  In fact, it is done by `string<'.
If `dir-treeview-show-dirs-first' is non-nil, directories always come first,
and the non-directories follow after them.  The directories and non-directories
among each other are sorted lexicographically by `string<' again."
  (if dir-treeview-show-dirs-first
      (if (file-directory-p filename-1)       ;; filename-1 | filename-2
          (if (file-directory-p filename-2)   ;; -----------+-----------
              (string< filename-2 filename-1) ;;   dir      |   dir
            nil)                              ;;   dir      |   non-dir
        (if (file-directory-p filename-2)     ;;            |
            t                                 ;;   non-dir  |   dir
          (string< filename-2 filename-1)) )  ;;   non-dir  |   non-dir
    (string< filename-2 filename-1)))


(defun dir-treeview-compare-filenames (filename-1 filename-2)
  "Return non-nil if FILENAME-1 is less than FILENAME-2 in actual sort order.
Applies the function stored in `dir-treeview-compare-filenames-function' to the
two filenames."
  (if (funcall dir-treeview-compare-filenames-function filename-1 filename-2) t nil))

(defun dir-treeview-compare-nodes (node-1 node-2)
  "Return non-nil if NODE-1 is less than NODE-2 in actual sort order.
The function simply applies `dir-treeview-compare-filenames' to the absolute
filenames of the nodes."
  (dir-treeview-compare-filenames
   (dir-treeview-get-node-absolute-name node-1)
   (dir-treeview-get-node-absolute-name node-2)))

(defun dir-treeview-get-dir-contents (dir)
  "Return the contents of DIR, filtered and sorted.
Filtering is done by `dir-treeview-filter-dir-contents'.
Sorting is done according to `dir-treeview-compare-filenames'."
  (sort (dir-treeview-filter-dir-contents (directory-files dir t nil t))
        'dir-treeview-compare-filenames))

(defun dir-treeview-get-child-with-absolute-name (parent absolute-name)
  "Return the child of PARENT (a node) with absolute filename ABSOLUTE-NAME."
  (let ( (children (treeview-get-node-children parent))
         (child nil) )
    (while (and (not child) children)
      (setq child (if (equal (treeview-get-node-prop (car children) 'absolute-name) absolute-name)
                      (car children))
            children (cdr children)))
    child))

(defun dir-treeview-find-node-with-relative-name (relative-name &optional base-node)
  "Return the node with name RELATIVE-NAME relative to BASE-NODE.
If no such node exists, return nil.
BASE-NODE defaults to `dir-treeview-start-node'."
  (let ( (node (or base-node dir-treeview-start-node))
         (name-parts (split-string relative-name dir-treeview-split-path-regexp)) )
    (while (and node name-parts)
      (let ( (name (car name-parts))
             (children (treeview-get-node-children node))
             (child nil) )
        (while (and (not child) children)
          (setq child (if (equal (treeview-get-node-name (car children)) name) (car children))
                children (cdr children)))
        (setq node child
              name-parts (cdr name-parts)) ))
    (unless name-parts node)))

(defun dir-treeview-find-node-with-absolute-name (absolute-name &optional base-node)
  "Search and return the node with the absolute name ABSOLUTE-NAME.
Start the search at BASE-NODE.  Return nil if no node was found.
BASE-NODE defaults to `dir-treeview-start-node'."
  (unless base-node (setq base-node dir-treeview-start-node))
  (let ( (relative-name (dir-treeview-relative-filename absolute-name (dir-treeview-get-node-absolute-name base-node))) )
    (if relative-name (dir-treeview-find-node-with-relative-name relative-name base-node))))

(defun dir-treeview-find-child-with-absolute-name (absolute-name node)
  "Search and return the child of NODE with the absolute name ABSOLUTE-NAME.
Return nil if no node was found."
  (let ( (children (treeview-get-node-children node)) (found-child nil) )
    (while (and children (not found-child))
      (let ( (child (car children)) )
        (when (equal (dir-treeview-get-node-absolute-name child) absolute-name)
          (setq found-child child))
        (setq children (cdr children))))
    found-child))

(defun dir-treeview-update-node-children (node)
  "Update the children of NODE.

If NODE represents a directory, and NODE is expanded, the following steps
are performed:  First, the contents of the directory are read be means of
`dir-treeview-get-dir-contents'.  Then, a new list of children is created which
contains one node for each content item in the directory.  For each item it
is checked if a corresponding node exists in the old list of children.  If
this is the case, the node in the old list is inserted in the new list (this
is necessary to preserve the folding status of the subtree starting with that
node).  If no node exists in the old list, the node is created.  Finally, the
new list is set as the actual list of children of NODE.

If NODE does not represent a directory, or NODE is not expanded, nothing is
done.

In both cases, NODE is returned."
  (if (and (dir-treeview-directory-p node) (treeview-node-expanded-p node))
      (let ( (contents (dir-treeview-get-dir-contents
                        (dir-treeview-get-node-absolute-name node)))
             (children ()) )
        (while contents
          (let* ( (absolute-name (car contents))
                  (child (or (dir-treeview-get-child-with-absolute-name node absolute-name)
                             (dir-treeview-new-node absolute-name node))) )
            (setq children (cons child children)
                  contents (cdr contents))))
        (treeview-set-node-children node children) ))
  node)

(defun dir-treeview-add-node-for-absolute-name (absolute-name)
  "Add a node for ABSOLUTE-NAME to the tree.
If ABSOLUTE-NAME is a descendant of the root directory of the tree and the tree
does not already contain a node for ABSOLUTE-NAME, such a node is created, added
to the tree, and displayed if the node is visible.  Otherwise, the function
does nothing."
  (let ( (abs-parent-name (dir-treeview-parent-filename absolute-name)) parent node )
    (when abs-parent-name
      (setq parent (dir-treeview-find-node-with-absolute-name abs-parent-name))
      (when (and parent (not (dir-treeview-get-child-with-absolute-name parent absolute-name)))
        (setq node (dir-treeview-new-node absolute-name parent))
        (treeview-add-child parent node 'dir-treeview-compare-nodes)))))

(defun dir-treeview-remove-node-with-absolute-name (absolute-name)
  "Remove the node with the specified ABSOLUTE-NAME from the tree.
If there is no such node, does nothing.  The function only acts on the node, the
respective file is not removed in the file system."
  (let ( (node (dir-treeview-find-node-with-absolute-name absolute-name)) )
    (when node
      (treeview-remove-node node))))

(defun dir-treeview-move-node-by-absolute-name (old-absolute-name new-absolute-name)
  "Move the node specified by OLD-ABSOLUTE-NAME to NEW-ABSOLUTE-NAME.

Removes the node with the absolute filename OLD-ABSOLUTE-NAME and adds a new
node with the absolute filename NEW-ABSOLUTE-NAME.

If the node with the absolute filename OLD-ABSOLUTE-NAME does not exist, the
first step (removing node with absolute filename OLD-ABSOLUTE-NAME) is skipped.

If a node with the absolute filename NEW-ABSOLUTE-NAME already exists, it is
removed before.

If NEW-ABSOLUTE-NAME is outside the tree, the second step (creating node with
absolute filename NEW-ABSOLUTE-NAME) is skipped.

The function only acts on the nodes, the respective file is not moved in the
file system."
  (let* ( (old-node (dir-treeview-find-node-with-absolute-name old-absolute-name))
          (new-node (dir-treeview-find-node-with-absolute-name new-absolute-name))
          (new-parent-absolut-name (dir-treeview-parent-filename new-absolute-name))
          (new-parent (when new-parent-absolut-name (dir-treeview-find-node-with-absolute-name new-parent-absolut-name))) )
    (when old-node (treeview-remove-node old-node))
    (when new-node (treeview-remove-node new-node))
    (when new-parent (treeview-add-child new-parent (dir-treeview-new-node new-absolute-name new-parent) 'dir-treeview-compare-nodes)) ))

(defun dir-treeview-redisplay-node-with-absolute-name (absolute-name)
  "Redisplay the node specified by ABSOLUTE-NAME.
If no such node exists, does nothing."
  (let ( (node (dir-treeview-find-node-with-absolute-name absolute-name)) )
    (when (and node (treeview-node-not-hidden-p node))
      (treeview-redisplay-node node))))

(defun dir-treeview-redisplay ()
  "Redisplay current buffer, which should be a Dir Treeview buffer."
  (let ( (dir (dir-treeview-get-node-absolute-name dir-treeview-start-node))
         (buffer-read-only nil) )
    (erase-buffer)
    (goto-char (point-min))
    (treeview-make-text-overlay dir nil 'dir-treeview-start-dir-face)
    (insert ":")
    (newline)
    (treeview-display-node dir-treeview-start-node)))

(defun dir-treeview-get-buffers ()
  "Return a list of all Dir Treeview buffers."
  (seq-filter #'(lambda (buffer) (eq (buffer-local-value 'major-mode buffer) 'dir-treeview-mode)) (buffer-list)))

(defun dir-treeview-get-buffer (&optional dir)
  "Find and return the Dir Treeview buffer with DIR as root directory.
If DIR is non-nil and there exists a Dir Treeview buffer for the directory DIR,
return the first such buffer found in the list of Dir Treeview buffers.  If DIR
is nil, return the first buffer in the list of Dir Treeview buffers, or, if the
list is empty (i.e., nil), return nil."
  (let ( (dir-treeview-buffers (dir-treeview-get-buffers))
         (buffer nil) )
    (if dir
        (let ( (absolute-name (directory-file-name (expand-file-name dir))) )
          (save-excursion
            (while (and (not buffer) dir-treeview-buffers)
              (set-buffer (car dir-treeview-buffers))
              (if (equal (dir-treeview-get-node-absolute-name dir-treeview-start-node) absolute-name)
                  (setq buffer (current-buffer)))
              (setq dir-treeview-buffers (cdr dir-treeview-buffers)))))
      (if dir-treeview-buffers (setq buffer (car dir-treeview-buffers))) )
    buffer))

(defun dir-treeview-for-each-node-in-each-buffer (callback)
  "Apply CALLBACK to each node in each Dir Treeview buffer.
CALLBACK should be a function expecting a node as argument."
    (dolist (buffer (dir-treeview-get-buffers))
      (with-current-buffer buffer
        (treeview-apply-recursively dir-treeview-start-node callback))))

(defun dir-treeview-get-nodes-in-each-buffer (&optional filter)
  "Return all nodes of all Dir Treeview buffers as a list.
If the optinal argument FILTER is specified, it must be function accepting a
single node as argument.  Only nodes for which this function returns non-nil
are included in the resulting list in this case."
  (unless filter (setq filter (lambda (_node) t)))
  (let ( (nodes ()) )
    (dir-treeview-for-each-node-in-each-buffer (lambda (node) (if (funcall filter node) (push node nodes))))
    nodes))

(defvar dir-treeview-file-watch-enabled nil
  "Whether file watch is enabled or not.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information about file watching.")

(defun dir-treeview-file-notify-callback (event)
  "Handle file event EVENT.
This function is used as callback for the file watching service.  The latter
notifies Emacs of file changes outside Emacs.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information."
  (when dir-treeview-log-file-notify-events (dir-treeview-log-file-notify-event event))
  (let ( (action (nth 1 event))
         (filename (nth 2 event))
         (filename1 (nth 3 event)) )
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'dir-treeview-mode)
          (cond ((eq action 'created)
                 ;; Provided filename is accepted by the tree, add a new node:
                 (when (dir-treeview-accept-filename filename)
                   (dir-treeview-add-node-for-absolute-name filename)))
                ((eq action 'deleted)
                 (dir-treeview-remove-node-with-absolute-name filename))
                ((eq action 'renamed)
                 (if (dir-treeview-accept-filename filename1)
                     ;; If target filename is accepted by the tree, move nodes, otherwise,
                     ;; simply remove node for source filename:
                     (dir-treeview-move-node-by-absolute-name filename filename1)
                   (dir-treeview-remove-node-with-absolute-name filename)))
                ((eq action 'attribute-changed)
                 (dir-treeview-redisplay-node-with-absolute-name filename)) ))))))

(defun dir-treeview-log-file-notify-event (event)
  "Create a log message for the file event EVENT.
The message is written to the buffer \"*dir-treeview-file-notify-event-log*\".
It has the following format:

TIMESTAMP -- ACTION FILENAME FILENAME1

TIMESTAMP is the time in human readable form (ISO-like).
For ACTION, FILENAME, and FILENAME1 see the documentation of file events in the
GNU Emacs Lisp Reference Manual, chapter \"File Notifications\"."
  (let ( (action (nth 1 event))
         (filename (nth 2 event))
         (filename1 (nth 3 event)) )
    (with-current-buffer (get-buffer-create "*dir-treeview-file-notify-event-log*")
      (goto-char (point-max))
      (unless (eolp) (newline))
      (insert (format-time-string "%Y-%m-%d %H:%M:%S %3N") (format " -- %s %s %s"  action filename filename1))
      (newline))))

(defvar dir-treeview-file-watch-alist ()
  "Association list of watched directories and file watch descriptors.
The keys are the absolute paths of the directories, the values the corresponding
file watch descriptors as returned by `file-notify-add-watch'.")

(defun dir-treeview-remove-unused-file-watches ()
  "Remove file watches not referring to any node in any Dir Treeview buffer.
Compares the directories in `dir-treeview-file-watch-alist' with the directories
occurring as nodes in any Dir Treeview buffer.  For each node for which no node
exists, the corresponding file watch descriptor is removed by means of
`file-notify-rm-watch', and the corresponding entry in the alist
`dir-treeview-file-watch-alist' is removed."
  (let ( dirnames new-watch-alist item )
    ;; Get paths of all directory nodes and store them in 'dirnames':
    (dolist (node (dir-treeview-get-nodes-in-each-buffer 'dir-treeview-directory-p))
      (let ( (dirname (dir-treeview-get-node-absolute-name node)) )
        (unless (member dirname dirnames) (push dirname dirnames))))
    ;; Iterate through watch alist, remove all watches of directories not in 'directories':
    (while (setq item (pop dir-treeview-file-watch-alist))
      (if (member (car item) dirnames)
          (push item new-watch-alist)
        (file-notify-rm-watch (cdr item))))
    ;; Set new watch alist:
    (setq dir-treeview-file-watch-alist new-watch-alist)))
        
(defun dir-treeview-add-to-file-watch-if-applicable (node)
  "Add the file represented by NODE to the file watch service if appropriate.
Appropriate means the file is a directory and the state of the node is not
folded-unread.  File watching only makes sence for such nodes.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information about file watching."
  (when (and (dir-treeview-directory-p node) (not (eq (treeview-get-node-state node) 'folded-unread)))
    (let ( (dirname (dir-treeview-get-node-absolute-name node)) )
      (unless (assoc dirname dir-treeview-file-watch-alist)
        (push (cons dirname (file-notify-add-watch dirname '(change) 'dir-treeview-file-notify-callback))
              dir-treeview-file-watch-alist)))))

(defun dir-treeview-switch-off-file-watch ()
  "Turn off the file watch service.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information about file watching."
  (interactive)
  (while dir-treeview-file-watch-alist
    (file-notify-rm-watch (cdr (pop dir-treeview-file-watch-alist))))
  (setq dir-treeview-file-watch-enabled nil))

(defun dir-treeview-switch-on-file-watch ()
  "Turn on the file watch service.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information about file watching."
  (interactive)
  (require 'filenotify)
  (setq dir-treeview-file-watch-enabled t)
  (dir-treeview-for-each-node-in-each-buffer
   (lambda (node) (dir-treeview-add-to-file-watch-if-applicable node))))

(defun dir-treeview-shutdown-file-watch-if-last-buffer ()
  "Turn off file watch if the current buffer is the only Dir Treeview buffer.

This function is called by `kill-buffer-hook' to shut down the file watch
service if the last Dir Treeview buffer is about to be closed.

See chapter \"File Notifications\" of the GNU Emacs Lisp Reference Manual for
more information about file watching."
  (if (and dir-treeview-file-watch-enabled
           (eq major-mode 'dir-treeview-mode)
           (equal (length (dir-treeview-get-buffers)) 1))
      (dir-treeview-switch-off-file-watch)))

(defun dir-treeview-after-node-expanded (node)
  "Do things that must be done after NODE has been expanded.
This function is the implementation of `treeview-after-node-expanded-function'
in Dir Treeview.  Therefore, it is called each time a node is expanded.
Currently,  the function runs only one action, i.e., it calles
`dir-treeview-add-to-file-watch-if-applicable' provided
`dir-treeview-file-watch-enabled' is non-nil."
  (if dir-treeview-file-watch-enabled (dir-treeview-add-to-file-watch-if-applicable node)))

(defun dir-treeview-create-mode-line-option-entry (option-symbol label help-text)
  "Create a mode line entry for a yes/no option.
This is an auxiliary function.  It creates a mode line entry for the booelan
variable whose symbol is OPTION-SYMBOL.  LABEL and HELP-TEXT should be strings.
The content of the entry is \"[LABEL:y]\" if the variable is non-nil, and
\"[LABEL:n]\" if the variable is non-nil, where LABEL is substituted by the
value of LABEL.  The Tooltip text of the entry is HELP-TEXT."
  (list ':propertize
        (list ':eval (list 'if option-symbol (concat "[" label ":y]") (concat "[" label ":n]")))
        'help-echo (concat help-text " (y : yes, n: no)")))

(defun dir-treeview-create-mode-line-format ()
  "Create the mode line format for Dir Treeview buffers."
  (list " "
        '(:propertize "%20b" face mode-line-buffer-id)
        " "
        '(:propertize (-3 "%p") help-echo "Percentage of the buffer above the top of the window, or 'Top' or 'Bottom' or 'All'")
        " "
        (dir-treeview-create-mode-line-option-entry 'dir-treeview-show-hidden-files
                                               "H"
                                               "Whether hidden files are shown")
        " "
        (dir-treeview-create-mode-line-option-entry 'dir-treeview-show-backup-files
                                               "B"
                                               "Whether backup files are shown") ))

(defun dir-treeview-mode ()
  "Major mode for Dir Treeview buffers.

Dir-Treeview provides a tree navigation for the file system, similar to
common file managers.  You can browse the directory structure, open files in
Emacs, open files in external programs, copy, rename, or delete files, create
directories, and open terminals in directories."
  (setq mode-name "Dir-Treeview"
        major-mode 'dir-treeview-mode
        truncate-lines t
        mode-line-format (dir-treeview-create-mode-line-format))
  (use-local-map (dir-treeview-create-local-keymap))
  (add-hook 'post-command-hook 'treeview-unselect-all-nodes-after-keyboard-quit 0 t)
  (add-hook 'post-command-hook 'treeview-unhighlight-node 0 t)
  (run-mode-hooks 'dir-treeview-mode-hook))
(put 'dir-treeview-mode 'mode-class 'special)

(defun dir-treeview-buffer-name (dir)
  "Return the name of a Dir Treeview buffer with DIR as root directory.
Returns \"*dir-treeview DIR *\" (where DIR is substituted by the value of DIR)."
  (concat "*dir-treeview " dir "*"))

(defun dir-treeview-create-buffer (dir)
  "Create a Dir Treeview buffer with DIR as root directory."
  (setq dir (expand-file-name dir))
  (let ( (buffer (generate-new-buffer (dir-treeview-buffer-name dir))) )
    (set-buffer buffer)
    (setq treeview-update-node-children-function 'dir-treeview-update-node-children
          treeview-node-leaf-p-function 'dir-treeview-node-leaf-p
          treeview-get-indent-function 'dir-treeview-get-indent
          treeview-get-indent-face-function (lambda (_node) 'dir-treeview-indent-face)
          treeview-get-icon-function (if (display-graphic-p) 'dir-treeview-get-icon 'treeview-return-nil)
          treeview-get-icon-face-function 'dir-treeview-get-icon-face
          treeview-get-icon-margin-left-function (lambda (_node) dir-treeview-icon-margin-left)
          treeview-get-icon-margin-right-function (lambda (_node) dir-treeview-icon-margin-right)
          treeview-get-control-function 'dir-treeview-get-control
          treeview-get-control-margin-left-function (lambda (_node) dir-treeview-control-margin-left)
          treeview-get-control-margin-right-function (lambda (_node) dir-treeview-control-margin-right)
          treeview-get-control-keymap-function 'dir-treeview-get-control-keymap
          treeview-get-control-face-function (lambda (_node) 'dir-treeview-control-face)
          treeview-get-control-mouse-face-function (lambda (_node) 'dir-treeview-control-mouse-face)
          treeview-get-label-function 'dir-treeview-get-label
          treeview-get-label-margin-left-function 'dir-treeview-get-label-margin-left
          treeview-get-label-keymap-function 'dir-treeview-get-label-keymap
          treeview-get-label-face-function 'dir-treeview-get-label-face
          treeview-get-label-mouse-face-function (lambda (_node) 'dir-treeview-label-mouse-face)
          treeview-get-selected-node-face-function (lambda (_node) 'dir-treeview-select-face)
          treeview-get-highlighted-node-face-function (lambda (_node) 'dir-treeview-highlight-face)
          treeview-after-node-expanded-function 'dir-treeview-after-node-expanded
          dir-treeview-start-node (dir-treeview-new-node dir nil)
          treeview-get-root-node-function (lambda () dir-treeview-start-node))
    (treeview-expand-node dir-treeview-start-node)
    (unless dir-treeview-saved-theme-loaded (dir-treeview-load-saved-theme))
    (dir-treeview-redisplay)
    (goto-char (point-min))
    (dir-treeview-mode)
    (when dir-treeview-use-file-watch (unless dir-treeview-file-watch-enabled (dir-treeview-switch-on-file-watch)))
    (setq buffer-read-only t)
    buffer))

;;;###autoload
(defun dir-treeview-open (&optional dir)
  "Display the directory tree for DIR.
If omitted or nil, read DIR in the minibuffer, with `dir-treeview-default-root'
as preset value.

If there exists a Dir Treeview buffer for the directory DIR, switch to the first
such buffer found in the list of dir Dir Treeview buffers.

If there exists no such buffer, create one and switch to it."
  (interactive)
  (unless dir (setq dir (dir-treeview-read-directory-name "Directory: " dir-treeview-default-root dir-treeview-default-root t)))
  (let ( (buffer (or (dir-treeview-get-buffer dir) (dir-treeview-create-buffer dir))) )
    (if dir-treeview-show-in-side-window
        (display-buffer-in-side-window buffer
                                       '((side . left)
                                         (no-other-window . t)
                                         (no-delete-other-windows . t)
                                         (preserve-size . (nil . nil))))
      (switch-to-buffer buffer)) ))

;;;###autoload
(defun dir-treeview ()
  "Display the directory tree for `dir-treeview-default-root'.

If there exists a Dir Treeview buffer for that directory, switch to the first
such buffer found in the list of dir treeview buffers.

If there exists no such buffer, create one and switch to it."
  (interactive)
  (dir-treeview-open dir-treeview-default-root))

(defun dir-treeview-call-for-file-at-point (action-function)
  "Apply ACTION-FUNCTION to the filename of the node at point.
ACTION-FUNCTION must be the symbol of a function.  The function is called with
one argument, the absolute filename of the node at point.  If there is no node
at point, does nothing."
  (let* ( (node (treeview-get-node-at-pos (point)))
          (filename (when node (dir-treeview-get-node-absolute-name node))) )
    (when filename
      (funcall action-function filename))))

(defun dir-treeview-highlight-file-at-point-and-call (action-function)
  "Highlight node at point and call ACTION-FUNCTION for the corresponding file.

ACTION-FUNCTION must be the symbol of a function.  The function is called
with one argument, the absolute filename of the node at point.  Before
ACTION-FUNCTION is invoked, the node is highlighted be means of
`treeview-highlight-node'.  After ACTION-FUNCTION returns, the node is
un-highlighted be means of `treeview-unhighlight-node'.

If there is no node at point, does nothing.

Except highlighting, this is the same as `dir-treeview-call-for-file-at-point'."
  (let* ( (node (treeview-get-node-at-pos (point))) )
    (when node
      (treeview-highlight-node node)
      (unwind-protect
          (funcall action-function (dir-treeview-get-node-absolute-name node))
        (treeview-unhighlight-node) ))))

(defun dir-treeview-open-file (node)
  "Open the file corresponding to NODE."
  (find-file (dir-treeview-get-node-absolute-name node)))

(defun dir-treeview-open-file-at-event (event)
  "Open the file corresponding to the node at the position where EVENT occurred.
See also `dir-treeview-open-file' and `dir-treeview-open-file-at-point'."
  (interactive "@e")
  (dir-treeview-open-file (treeview-get-node-at-event event)))

(defun dir-treeview-open-file-at-point ()
  "Open the file corresponding to the node at point.
See also `dir-treeview-open-file' and `dir-treeview-open-file-at-event'."
  (interactive)
  (treeview-call-for-node-at-point 'dir-treeview-open-file))
        
(defun dir-treeview-open-with (command params filename)
  "Open FILENAME with an external program.
COMMAND is the filename of the program.  It is called with PARAMS and FILENAME
as arguments (in that order).  PARAMS should be a (possibly empty) list of
strings.."
  (setq params (append params (list filename)))
  (apply #'call-process command nil 0 nil params))

(defun dir-treeview-open-with-other (filename)
  "Open FILENAME with an external program read in the minibuffer.
Reads the name or path of an external program in the minibuffer, and calls it
with FILENAME as argument."
  (let ( (command (read-shell-command "Command: ")) )
    (dir-treeview-open-with command nil filename)))

(defun dir-treeview-get-directory (location)
  "Return the directory corresponding to LOCATION.
If LOCATION is a directory, returns the absolute and canonicalized version
of LOCATION, otherwise the absolute and canonicalized version of its parent.
The conversion to the absolute and canonicalized version is done by
`expand-file-name'."
  (setq location (expand-file-name location))
  (if (file-directory-p location) location (dir-treeview-parent-filename location)))

(defun dir-treeview-open-terminal (location)
  "Open a terminal at LOCATION.
LOCATION must be the name of a file or directory.  In case of a directory, the
terminal is opened in that directory.  In case of a non-directory file, the
terminal is opened in the directory containing the file.  The terminal program
is specified by the customizable variable `dir-treeview-terminal-program'.
If the variable is not set, the user is asked to set it now."
  (setq location (dir-treeview-get-directory location))
  (if (or (not dir-treeview-terminal-program) (string-equal dir-treeview-terminal-program ""))
      (if (dir-treeview-user-confirm-y-or-n
           (concat "No terminal program specified. "
                   "You must set the customizable variable 'dir-treeview-terminal-program' first. "
                   "Set ist now? "))
          (customize-variable 'dir-treeview-terminal-program))
    (let ( (default-directory location) )
      (call-process dir-treeview-terminal-program nil 0 nil))))

(defun dir-treeview-open-terminal-at-point ()
  "Open a terminal in the location corresponding to the node at point.
Calls `dir-treeview-open-terminal' with the absolute filename of the node at
point.  If there is no node at point, does nothing."
  (interactive)
  (dir-treeview-call-for-file-at-point 'dir-treeview-open-terminal))

(defun dir-treeview-open-new-file (location)
  "Open a buffer with a new file at LOCATION.
Asks the user to input the name of the file (which must not exist yet), creates
a buffer for that file, and switches to that buffer.  LOCATION must be the name
of a file or directory.  In case of a directory, the new file is created in
LOCATION.  In case of a non-directory, the new file is created in the directory
containing LOCATION.."
  (find-file (dir-treeview-read-new-file-name "New file: " (dir-treeview-get-directory location))))

(defun dir-treeview-open-new-file-at-point ()
  "Open a buffer with a new file in the directory at point.
Calls `dir-treeview-open-new-file' with the absolute filename of the node at
point."
  (interactive)
  (dir-treeview-call-for-file-at-point 'dir-treeview-open-new-file))

(defun dir-treeview-copy-file (node)
  "Copy the file corresponding to NODE.
Reads the target in the minibuffer.  If the target is a directory, the file
is copied to a like-named file in that directory.  If the destination file
exists already, the function asks for confirmation in the minibuffer before
overwriting it.
This function is not suitable for directories.  Use `dir-treeview-copy-dir'
to copy directories."
  (let* ( (filename (dir-treeview-get-node-absolute-name node))
          (prompt (concat "Copy " (dir-treeview-local-filename filename) " to: "))
          (new-filename (expand-file-name (dir-treeview-read-file-name prompt filename))) )
    (if (file-directory-p new-filename)
        (setq new-filename (concat (file-name-as-directory new-filename) (dir-treeview-local-filename filename))))
    (if (or (not (file-exists-p new-filename))
            (dir-treeview-user-confirm-y-or-n (concat new-filename " exists. Overwrite? ")))
        (let ( (parent-of-new (dir-treeview-find-node-with-absolute-name (dir-treeview-parent-filename new-filename))) )
          (copy-file filename new-filename t)
          (if parent-of-new
              ;; If file watch is enabled, we let its callback function do the refreshing
              (unless dir-treeview-file-watch-enabled (treeview-refresh-node parent-of-new))) )) ))

(defun dir-treeview-copy-dir (node)
  "Copy the directory corresponding to NODE.
Asks for the target directory in the minibuffer.  If the target exists
already, asks for confirmation in the minibuffer before overwriting it."
  (let* ( (dir (dir-treeview-get-node-absolute-name node))
          (prompt (concat "Copy " (dir-treeview-local-filename dir) " to: "))
          (new-dir (expand-file-name (dir-treeview-read-file-name prompt dir))) )
    (if (file-directory-p new-dir)
        (setq new-dir (concat (file-name-as-directory new-dir) (dir-treeview-local-filename dir))))
    (if (or (not (file-exists-p new-dir)) (dir-treeview-user-confirm-y-or-n (concat new-dir " exists. Overwrite? ")))
        (let ( (parent-of-new (dir-treeview-find-node-with-absolute-name (dir-treeview-parent-filename new-dir))) )
          (copy-directory dir new-dir nil t)
          (if parent-of-new
              ;; If file watch is enabled, we let its callback function do the refreshing
              (unless dir-treeview-file-watch-enabled (treeview-refresh-node parent-of-new))) ))))

(defun dir-treeview-copy-file-or-dir (node)
  "Copy the file or directory corresponding to NODE.
Calls `dir-treeview-copy-dir' if the node corresponds to a directory, and
`dir-treeview-copy-file' if the node corresponds to a non-directory."
  (if (dir-treeview-directory-p node) (dir-treeview-copy-dir node) (dir-treeview-copy-file node)))

(defun dir-treeview-copy-file-or-dir-at-point ()
  "Copy the file or directory corresponding to the node at point.
If there is a node at point, calls `dir-treeview-copy-file-or-dir'.
If there is no node at point, does nothing."
  (interactive)
  (treeview-call-for-node-at-point 'dir-treeview-copy-file-or-dir t))

(defun dir-treeview-kill-buffer (buffer)
  "Kill buffer BUFFER.
Same as (`kill-buffer' BUFFER), but marks BUFFER as unmodified before.
This suppresses the build-in confirmation question which would otherwise occur
if the buffer was modified.  Can be used if the user already confirmed to kill
the buffer by means of one of fir-treeview's own confirmation dialogs (e.g.,
`dir-treeview-user-confirm-y-or-n'."
  (with-current-buffer buffer (set-buffer-modified-p nil))
  (kill-buffer buffer))

(defun dir-treeview-get-buffers-for-prefix-path (prefix)
  "Return a list of all buffers whose filename starts with the path PREFIX.
A buffer is included in the list if, and oly if, one of the following conditions
hold true:
  (1) its filename equals PREFIX,
  (2) its filename equals PREFIX + DIR_SEP,
  (3) its filename starts with PREFIX + DIR_SEP,
where DIR_SEP is the platform-specific directory separator (e.g., \"/\" on Linux
or Unix).
This is an auxiliary function for updating buffer filenames after a file has
been renamed, or for cleaning up orphand buffers after a file or directory has
been  deleted."
  (let ( (buffers ()) (prefix-as-dir (directory-file-name prefix)) )
    (dolist (buffer (buffer-list))
      (let ( (filename (buffer-file-name buffer)) )
        (when (and filename (or (string-prefix-p prefix-as-dir filename) (string-equal prefix filename)))
          (setq buffers (cons buffer buffers)))))
    buffers))

(defun dir-treeview-update-buffer-file-name (old-filename new-filename)
  "Update the filename of all buffers affected by renaming a file or directory.
OLD-FILENAME and NEW-FILENAME are the old and new name of the file,
respectively.  They must be absolute paths.  If the renamed file is a normal
file, not a directory, the only buffer affected by the renaming is the buffer
visiting that file.  But if the file is directory, all buffers visiting files
in descendant directories are also affected.  For all affected buffers, the
buffer file name is updated (OLD-FILENAME replaced by NEW-FILENAME)."
  (let ( (offset (length old-filename)) )
  (dolist (buffer (dir-treeview-get-buffers-for-prefix-path old-filename))
    (with-current-buffer buffer
      (set-visited-file-name (concat new-filename (substring (buffer-file-name) offset)) nil t)) )) )

(defun dir-treeview-kill-orphand-buffers (filename)
  "Kill all buffers that became orphand due to the deletion of file FILENAME.
An orphand buffer is a buffer that visits a deleted file.  If FILENAME is a
normal file, not a directory, the only buffer affected by the deletion of
FILENAME is the buffer visiting FILENAME.  But if FILENAME is a directory, all
buffers visiting files in descendant directories are also affected.
The user is asked for confirmation for each file, but can choose to confirm or
deny for all remaining files in each question (see
`dir-treeview-user-confirm-y-n-Y-N')."
  (let ( (buffers (dir-treeview-get-buffers-for-prefix-path filename)) )
    ;; If there is more than one buffer, iterate through all of them. In the confirmation dialog,
    ;; offer the user "yes for all" and "no to all" (in addition to standard "yes" and "no).
    (if (> (length buffers) 1)
        (let ( (choice nil) )
          (dolist (buffer buffers)
            (unless (member choice '(?Y ?N))
              (setq choice (dir-treeview-user-confirm-y-n-Y-N (format "Kill buffer visiting %s? " (buffer-file-name buffer)))))
            (when (member choice '(?y ?Y))
              (dir-treeview-kill-buffer buffer)) ))
      ;; If there is only one buffer, show the standard confirmation dialog (only "yes and "no")
      (when (= (length buffers) 1)
        (let ( (buffer (car buffers)) )
          (when (dir-treeview-user-confirm-y-or-n (format "Kill buffer visiting %s? " (buffer-file-name buffer)))
            (dir-treeview-kill-buffer buffer)) )) )) )

(defun dir-treeview-rename-file (node)
  "Rename the file or directory corresponding to NODE.
Reads the target in the minibuffer.  If the target is a directory, the file
is renamed to a like-named file in that directory.  If the destination file
exists already, the function asks for confirmation in the minibuffer before
overwriting it."
  (let* ( (filename (dir-treeview-get-node-absolute-name node))
          (prompt (concat "Rename " (dir-treeview-local-filename filename) " to: "))
          (new-filename (expand-file-name (dir-treeview-read-file-name prompt filename))) )
    (if (file-directory-p new-filename)
        (setq new-filename (concat (file-name-as-directory new-filename) (dir-treeview-local-filename filename))))
    (if (or (not (file-exists-p new-filename))
            (dir-treeview-user-confirm-y-or-n (concat new-filename " exists. Overwrite? ")))
        (let ( (parent (treeview-get-node-parent node))
               (parent-of-new (dir-treeview-find-node-with-absolute-name (dir-treeview-parent-filename new-filename))) )
          (rename-file filename new-filename t)
          ;; If file watch is enabled, we let its callback function do the refreshing
          (unless dir-treeview-file-watch-enabled
            (treeview-refresh-node parent)
            (if (and parent-of-new (not (eq parent parent-of-new)))
                (treeview-refresh-node parent-of-new)))
          (dir-treeview-update-buffer-file-name filename new-filename) ))))

(defun dir-treeview-rename-file-at-point ()
  "Rename the file or directory corresponding to the node at point.
Calls `dir-treeview-rename-file' with the node at point.  If there is no node at
point, does nothing."
  (interactive)
  (treeview-call-for-node-at-point 'dir-treeview-rename-file t))
 
(defun dir-treeview-delete-file (node)
  "Delete the file corresponding to NODE.
Asks for confirmation in the minibuffer.
This function is not suitable for directories.  Use `dir-treeview-delete-dir'
to delete directories."
  (let ( (filename (dir-treeview-get-node-absolute-name node)) )
    (when (dir-treeview-user-confirm-y-or-n (concat "Delete " (dir-treeview-local-filename filename) "? "))
      (delete-file filename)
      ;; If file watch is enabled, we let its callback function do the refreshing
      (unless dir-treeview-file-watch-enabled (treeview-refresh-node (treeview-get-node-parent node)))
      (dir-treeview-kill-orphand-buffers filename) )))

(defun dir-treeview-delete-dir (node)
  "Recursively delete the directory corresponding to NODE.
Asks for confirmation in the minibuffer."
  (let ( (dir (dir-treeview-get-node-absolute-name node)) )
    (when (dir-treeview-user-confirm-y-or-n (concat "Recursively delete " (dir-treeview-local-filename dir) " and all its contents? "))
      (delete-directory dir t)
      ;; If file watch is enabled, we let its callback function do the refreshing
      (unless dir-treeview-file-watch-enabled (treeview-refresh-node (treeview-get-node-parent node)))
      (dir-treeview-kill-orphand-buffers dir) )))

(defun dir-treeview-delete-file-or-dir (node)
  "Delete the file or directory corresponding to the NODE.
Calls `dir-treeview-delete-dir' if NODE corresponds to a directory, and
`dir-treeview-delete-file' if NODE corresponds to a non-directory."
  (if (dir-treeview-directory-p node) (dir-treeview-delete-dir node) (dir-treeview-delete-file node)))

(defun dir-treeview-delete-file-or-dir-at-point ()
  "Delete the file or directory corresponding to the node at point.
If there is a node at point, calls `dir-treeview-delete-file-or-dir' for that
node.  If there is no node at point, does nothing."
  (interactive)
  (treeview-call-for-node-at-point 'dir-treeview-delete-file-or-dir t))

(defun dir-treeview-delete-at-point ()
  "Delete the file at point, or all selected files if node at point is selected.
If the node at point is selected, calls `dir-treeview-delete-selected-files'.
Otherwise, calls `dir-treeview-delete-file-or-dir'
If there is no node at point, does nothing."
  (interactive)
  (let ( (node (treeview-get-node-at-pos (point))) )
    (when node
      (if (treeview-node-selected-p node) (dir-treeview-delete-selected-files)
        (treeview-call-for-node node 'dir-treeview-delete-file-or-dir t) ))))

(defun dir-treeview-user-confirm-overwrite (filename)
  "Ask the user for confirmation to overwrite one or all files in question.
This function is used when a list of files is copied or moved.  It is called for
each file for which the target exists.  FILENAME is the filename of the target.
A prompt is displayed saying that the target exists and asking if
it should be overwritten.  The user can answer with \"y\", \"n\", \"a\" or \"o\".
The meaning is the following:
  y - (yes) overwrite target
  n - (no) do not overwrite target
  a - (all) overwrite this target and do so for the rest of the files
  o - (none) do not overwrite this file and do so for the rest of the files"
  (interactive)
  (let* ( (prompt (format "%s exists - overwrite? " filename))
          (input (read-event (concat prompt "(y, n, a for all, o for none) "))) )
    (while (not (member input '(?y ?n ?a ?o)))
      (setq input (read-event (concat "Please answer y, n, a, or o - " prompt))))
    input))

(defun dir-treeview-user-inform (message)
  "Display MESSAGE in the echo area and wait until the user presses RET."
  (while (not (eq (read-event (concat message " (press RET key to continue)")) 'return))))

(defun dir-treeview-copy-or-move-files-to-dir (source-files target-dir copy-or-move-function)
  "Copy or move SOURCE-FILES to TARGET-DIR by means of COPY-OR-MOVE-FUNCTION.
This is an auxliary function to implement the copying or moving of several files
to the same target directory.  SOURCE-FILES should be a list of filenames.
TARGET-DIR should be a directory name.  COPY-OR-MOVE-FUNCTION should be a
function expecting two filenames as arguments.  It is called for each filename
in SOURCE-FILES, with that filename as first and TARGET-DIR as second argument.
COPY-OR-MOVE-FUNCTION should copy or move the respective source file to
TARGET-DIR."
  (let ( (overwrite 'ask) )
    (unless (file-directory-p target-dir) (error "Not a directory: %s" target-dir))
    (dolist (source-file source-files)
      (let* ( (filename (file-name-nondirectory source-file))
              (target-file (concat (file-name-as-directory target-dir) filename)) )
        (when (or (not (file-exists-p target-file))
                  (if (not (file-regular-p target-file))
                      (progn (dir-treeview-user-inform (format "%s exists as a non-regular file - skipping" filename)) nil)
                    (eq overwrite 'all)
                    (and (eq overwrite 'ask)
                         (let ( (user-answer (dir-treeview-user-confirm-overwrite filename)) )
                           (cond ((equal user-answer ?a) (setq overwrite 'all))
                                 ((equal user-answer ?o) (setq overwrite 'none)))
                           (member user-answer '(?y ?a))))))
          (funcall copy-or-move-function source-file target-file) )))
    ;; If file watch is enabled, we let its callback function do the refreshing
    (unless dir-treeview-file-watch-enabled (treeview-refresh-tree)) ))

(defun dir-treeview-delete-files (files)
  "Delete FILES.
FILES should be a list of filenames."
  (let ( (nondirs ()) (dirs ()) )
    (dolist (file files) (push file (if (file-directory-p file) dirs nondirs)))
    (let ( (prompt nil) (nondirs-count (length nondirs)) (dirs-count (length dirs)) )
      (when (> nondirs-count 0)
        (setq prompt (format "Delete %s file(s)" nondirs-count)))
      (when (> dirs-count 0)
        (setq prompt (if prompt (concat prompt (format " and recursively delete %s directorie(s)" dirs-count))
                       (format "Recursively delete %s directorie(s)" dirs-count))))
      (setq prompt (concat prompt "? "))
      (when (dir-treeview-user-confirm-y-or-n prompt)
        (dolist (file nondirs) (delete-file file))
        (dolist (dir dirs) (delete-directory dir t))
        ;; If file watch is enabled, we let its callback function do the refreshing
        (unless dir-treeview-file-watch-enabled (treeview-refresh-tree)) ))) )

(defun dir-treeview-get-selected-files ()
  "Return all selected files (abolute names)."
  (seq-uniq (seq-map 'dir-treeview-get-node-absolute-name (treeview-get-all-selected-nodes))))

(defun dir-treeview-copy-selected-files-to (target-dir)
  "Copy all selected files to TARGET-DIR."
  (dir-treeview-copy-or-move-files-to-dir
   (dir-treeview-get-selected-files)
   target-dir
   ;; Copy function:
   (lambda (source-abs-name target-abs-name)
     (if (file-directory-p source-abs-name)
         (progn
           ;; copy-directory has no flag to overwrite an existing target, so we delete the target if existing:
           (when (file-exists-p target-abs-name) (delete-file target-abs-name))
           (copy-directory source-abs-name target-abs-name))
       (copy-file source-abs-name target-abs-name t))))
  (treeview-unselect-all-nodes))

(defun dir-treeview-move-selected-files-to (target-dir)
  "Move all selected files to TARGET-DIR."
  (dir-treeview-copy-or-move-files-to-dir
   (dir-treeview-get-selected-files)
   target-dir
   ;; Move function:
   (lambda (source-abs-name target-abs-name)
     (rename-file source-abs-name target-abs-name t)
     (dir-treeview-update-buffer-file-name source-abs-name target-abs-name)))
  (treeview-unselect-all-nodes))

(defun dir-treeview-delete-selected-files ()
  "Delete all selected files."
  (dir-treeview-delete-files (dir-treeview-get-selected-files))
  (treeview-unselect-all-nodes))

(defun dir-treeview-copy-selected-files-to-dir-at-point ()
  "Copy all selected files to the directory at point.
If there is no node at point, does nothing.  If there a are no selected files,
does nothing, but shows a message in the echo area.  If the node at point
doesn't represent a directory, an error is signaled."
  (interactive)
  (let ( (node (treeview-get-node-at-pos (point))) )
    (when node
      (if (treeview-selected-nodes-exist)
          (let ( (absolute-name (dir-treeview-get-node-absolute-name node)) )
            (unless (file-directory-p absolute-name) (error "\"%s\" is  not a directory" absolute-name))
            (when (dir-treeview-user-confirm-y-or-n "Copy selected files here? ")
              (dir-treeview-copy-selected-files-to absolute-name) ))
        (message "No files selected")) )))

(defun dir-treeview-open-selected-files ()
  "Open all selected files in Emacs."
  (let ( (buffer nil) )
    (dolist (filename (dir-treeview-get-selected-files)) (setq buffer (find-file-noselect filename)))
    (treeview-unselect-all-nodes)
    (when buffer (switch-to-buffer buffer)) ))

(defun dir-treeview-open-selected-files-with ()
  "Open all selected files with an external program read in the minibuffer."
  (apply #'call-process (read-shell-command "Open selected files with command: ") nil 0 nil (dir-treeview-get-selected-files))
  (treeview-unselect-all-nodes))

(defun dir-treeview-create-subdir (node)
  "Create a subdiectory of the directory corresponding to NODE."
  (make-directory (dir-treeview-read-directory-name
                   "New subdirectory: "
                   (file-name-as-directory (dir-treeview-get-node-absolute-name node))))
  ;; If file watch is enabled, we let its callback function do the refreshing
  (unless dir-treeview-file-watch-enabled (treeview-refresh-node node)))

(defun dir-treeview-create-subdir-at-point ()
  "Create a subdiectory of the directory corresponding to the node at point.
If there is a node at point, calls `dir-treeview-create-subdir' if the node
corresponds to a directory, and signals an error if the node corresponds to
a non-directory.  If there is no node at point, does nothing."
  (interactive)
  (let ( (node (treeview-get-node-at-pos (point))) )
    (when node
      (unless (dir-treeview-directory-p node) (user-error "Node at point is not a directory"))
      (dir-treeview-create-subdir node))))

(defun dir-treeview-format-file-size (byte-count)
  "Return a human-readable from of BYTE-COUNT.
The latter should by an integer representing a number of bytes.  Returns a
string describing this value in a suitable unit; one of G (gigabytes),
M (megabytes), K (kilobytes) or bytes.  Examples: 5.23G, 12.05K, 756.
If unit is bytes, no unit symbol is added."
  (if (< byte-count 1024) (format "%d" byte-count)
    (let ( (unit "") (size (float byte-count)) )
      (setq unit "K" size (/ size 1024))
      (when (>= size 1024) (setq unit "M" size (/ size 1024)))
      (when (>= size 1024) (setq unit "G" size (/ size 1024)))
      (format "%.2f%s" size unit))))

(defun dir-treeview-join-strings (&rest items)
  "Create a string from parts, optionally apply faces to parts.
Auxiliary function to create strings with highlighted parts.  ITEMS must be a
list.  Eeach list item must be either a string or a cons cell (FACE . STRING)
where FACE is a face and STRING is a string.  Joins all strings, where the
strings occurring in cons cells are given the face specified in the cons cell."
  (let ( (result "") (pos 0) )
    (dolist (item items)
      (if (consp item)
          (let ( (face (car item)) (part (cdr item)) (start pos) )
            (setq result (concat result part) pos (+ pos (length part)))
            (put-text-property start pos 'face face result))
        (setq result (concat result item) pos (+ pos (length item))) ))
    result))

(defun dir-treeview-create-file-info-string (filename)
  "Create a string describing the file attributes of FILENAME.
The string is similar to the output of the Linux command ls -l.  It contains the
file modes (for examlple, -rw-r--r-), owner, group, size, and last modification
time."
  (let ( (attribs (file-attributes filename 'string)) )
    (when attribs
      (dir-treeview-join-strings
       (if (stringp (nth 0 attribs)) (cons 'dir-treeview-link-target-face (concat " -> " (nth 0 attribs))) "")
       " " (cons 'dir-treeview-file-mode-face (nth 8 attribs))
       " " (cons 'dir-treeview-file-owner-face (nth 2 attribs))
       " " (cons 'dir-treeview-file-group-face (nth 3 attribs))
       " " (cons 'dir-treeview-file-size-face (dir-treeview-format-file-size (nth 7 attribs)))
       " " (cons 'dir-treeview-file-time-face (format-time-string dir-treeview-time-format (nth 5 attribs)))
       " ") )) )

(defun dir-treeview-show-file-info (filename)
  "Show the file attributes of FILENAME in the echo area.
Calls `dir-treeview-create-file-info-string' with FILENAME as argument and
displayes the result in the echo area."
  (let ( (file-info (dir-treeview-create-file-info-string filename)) )
    (when file-info (message "%s" file-info)) ))

(defun dir-treeview-show-info-for-node-at-point ()
  "Show the file attributes of the the node at point in the echo area.
Calls `dir-treeview-show-file-info' for the file corresponding to the node at
point.  If there is no node at point, or the file doesn't exist, does nothing."
  (interactive)
  (dir-treeview-call-for-file-at-point 'dir-treeview-show-file-info))

(defun dir-treeview-exec (cmd dir &rest args)
  "Execute command CMD in directory DIR with arguments ARGS.

CMD is the command to run (a string).  DIR specifies the directory of the
process.  It must be a string or nil.  If nil, DIR is set to the value of
`default-directory'.  ARGS are the arguments passed to the command.  All of
them must be strings.  The command is run synchroniously, thus, Emacs waits
until the proecess is finished.  If the exit code of the process is 0, the
function returns t, otherwise, it signals an error."
  (unless dir (setq dir default-directory))
  (with-temp-buffer
    (setq default-directory dir)
    (if (equal (apply 'call-process cmd nil t nil args) 0) t (error "Failed to execute %s: %s" cmd (buffer-string)))))

(defun dir-treeview-sudo-tramp (cmd dir &rest args)
  "Run an external command as root using Tramp's sudo method.

CMD, DIR, and ARGS have the same meaning as in `dir-treeview-exec'.  The command
is run synchroniously, thus, Emacs waits until the proecess is finished.

Tramp is actually a package to access remote files by different methods, but it
also provides the \"sudo\" method which allows us to work as root on the local
machine.  See

  URL `https://www.gnu.org/software/emacs/manual/html_node/tramp/index.html'

for more information about Tramp.  This function uses Tramp with the \"sudo\"
method to execute the above command as root.  If necessary, the user will be
asked for a password."
  (unless dir (setq dir default-directory))
  (with-temp-buffer
    (let ( (auth-source-save-behavior nil) )
      (cd (concat "/sudo:root@localhost:" dir))
      (shell-command (concat cmd " " (mapconcat 'shell-quote-argument args " "))) )))

(defun dir-treeview-sudo-nopasswd (cmd dir &rest args)
  "Run an external command with `sudo´, not expecting a password prompt.

CMD, DIR, and ARGS have the same meaning as in `dir-treeview-exec'.

Calls `sudo´ to run CMD as root in directory DIR with arguments ARGS.  Only
works properly if `sudo´ doesn't ask for a password.  This is the case if
the NOPASSWD flag is set for the current user in the `sudoers´ configuration
\(see `man sudoers´ for more information).

CMD is executed synchroniously.  If the exit code of the process is 0, returns
t, otherwise, signals an error."
  (apply 'dir-treeview-exec "sudo" dir cmd args))

(defun dir-treeview-pkexec (cmd dir &rest args)
  "Run an external command as root using `pkexec´.

CMD, DIR, and ARGS have the same meaning as in `dir-treeview-exec'.  The command
is run synchroniously, thus, Emacs waits until the proecess is finished.

`pkexec´ is a program to run a command as another user.  It is similar to `sudo´,
but unlike `sudo´, it uses a graphical dialog to enter the password by default
\(see `man pkexec´ for more information).  Note that the way `pkexec´
authenticates the user is configurable, the graphical dialog is only the
default.  If the default is changed, this function might not work properly."
  (apply 'dir-treeview-exec "pkexec" dir "--keep-cwd" cmd args))
(defun dir-treeview-sudo (cmd dir &rest args)
  "Run command CMD as root in directory DIR with arguments ARGS.

Same as `dir-treeview-exec', buts the command is executed as root.

Calls `dir-treeview-sudo-function' if in graphics mode, thus, if
\(`display-graphic-p') returns non-nil.  Otherwise, calls
`dir-treeview-sudo-function-textmode'."
  (apply
   (if (display-graphic-p) dir-treeview-sudo-function dir-treeview-sudo-function-textmode)
   cmd dir args))

(defun dir-treeview-getent (database sep &rest cols)
  "Get system information by running 'getent DATABASE' and process the output.

Recall that getent is a program to get system information like users, user
groups, etc. from sources called \"databases\" (see `man getent´ for details).
The information is written to stdout in tabulated form.  The cells are separated
by, e.g., whitespaces or colons, depending on the database.

DATABASE is the database to query.  It must be a string.

SEP must be a regular expression describing the cell separatur used in DATABASE.
Each match of SEP is treated as a separator.

If COLS is not specified or nil, the entire table is returned.  Otherwise, only
the columns specified by COLS are returned.  The values of COLS are interpreted
as column numbers in this case. Column numbers start at 0.

The return value is a list of lists of strings, where each of the latter lists
correponds to one row of output, and each element to one cell."
  (let ( (result ()) )
    (with-temp-buffer
      (when (equal (call-process "getent" nil '(t nil) nil database) 0)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ( (line (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
                  (record (split-string line sep)) )
            (when cols (setq record (seq-map #'(lambda (col) (nth col record)) cols)))
            (push record result))
          (forward-line 1))))
    result))

(defun dir-treeview-filter-by-prefix (list-of-strings prefix)
  "Filter a list of strings by prefix.
This is an auxiliary function.  LIST-OF-STRINGS must be a list of strings,
PREFIX a string.  Returns the elements of LIST-OF-STRINGS wich start with
PREFIX, as a list."
  (seq-filter #'(lambda (elem) (string-prefix-p prefix elem)) list-of-strings))

(defun dir-treeview-get-chown-arg-completions (value)
  "Return all possible completions of VALUE as the first argument of `chown´.
Recall that the first argument of chown (not counting options) specifies the new
user and, optionally, the new group in the form NEW_USER[:NEW_GROUP]."
  (let* ( (tokens (split-string value ":")) (count (length tokens)) )
    (cond ((= count 1) (dir-treeview-filter-by-prefix
                        ;; User names and ids (columns 0 and 2 in getent output):
                        (apply #'nconc (dir-treeview-getent "passwd" ":" 0 2)) value))
          ((= count 2) (seq-map #'(lambda (elem) (concat (nth 0 tokens) ":" elem))
                                (dir-treeview-filter-by-prefix
                                 ;; Group names and ids (columns 0 and 2 in getent output):
                                 (apply #'nconc (dir-treeview-getent "group" ":" 0 2)) (nth 1 tokens))))
          (t nil)) ))


(defun dir-treeview-chown (owner recursive &rest abs-filenames)
  "Apply the command `chown´ with argument OWNER to the files ABS-FILENAMES.
If RECURSIVE is non-nil, the flag \"-R\" is set in addition.  `chown´ is run as
root.  The user may be prompted for his/her (not root's) password.

This is equivalent to

  sudo chown [-R] OWNER ABS-FILENAMES

on the command line.  [-R] means -R is set if, and only if, RECURSIVE is
non-nil.

Returns t on success, and signals an error otherwise."
  (let ( (args abs-filenames) )
    (when recursive (push "-R" args))
    (push owner args)
    (apply 'dir-treeview-sudo "chown" nil args)))

(defun dir-treeview-change-owner (node)
  "Change the user, and optionally group, of the file corresponding to NODE.

This is carried out as root.  The user might be asked for his/her (not root's)
password.

The user is asked for the new owner in the minibuffer.  The owner must be
specified in a form understood by `chown´.  Thus, it must be of the form USER or
USER:GROUP, where USER and GROUP may be names or numerical ids.  If the file is
a directory, the user is asked if the owner should be changed recursively."
  (let* ( (abs-filename (treeview-get-node-prop node 'absolute-name))
          (filename (dir-treeview-local-filename abs-filename))
          (new-owner (completing-read (format "Change owner of %s (chown syntax): " filename)
                                      (completion-table-dynamic #'dir-treeview-get-chown-arg-completions)))
          (recursive (when (file-directory-p abs-filename)
                       (dir-treeview-user-confirm-y-or-n "Change owner recursively? "))) )
    (dir-treeview-chown new-owner recursive abs-filename)
    (treeview-redisplay-node node) ))

(defun dir-treeview-change-owner-at-point ()
  "Change the user, and optionally group, of the file at point.
Calls `dir-treeview-change-owner' for the node at point.  If there is no node at
point, does nothing."
  (interactive)
  (treeview-call-for-node-at-point 'dir-treeview-change-owner t t))

(defun dir-treeview-change-owner-of-selected-files ()
  "Change the user, and optionally group, of the selected files.

The user is asked for the new owner in the minibuffer.  The owner must be
specified in a form understood by `chown´.  Thus, it must be of the form USER or
USER:GROUP, where USER and GROUP may be names or numerical ids.  If there are
directories among the selected files, the user is asked if the owner of the
directories should be changed recursively."
  (interactive)
  (let ( (selected-nodes (treeview-get-all-selected-nodes)) (saved-point (point)) )
    (if selected-nodes
        (unwind-protect
            (let ( (new-owner (completing-read "Change owner of selected files (chown syntax): "
                                               (completion-table-dynamic  #'dir-treeview-get-chown-arg-completions)))
                   (recursive (when (seq-some 'dir-treeview-directory-p selected-nodes)
                                (dir-treeview-user-confirm-y-or-n "Change owner of directories recursively? "))) )
              (apply 'dir-treeview-chown new-owner recursive (seq-map 'dir-treeview-get-node-absolute-name selected-nodes))
              (dolist (node selected-nodes) (treeview-redisplay-node node)) )
          (treeview-unselect-all-nodes)
          (goto-char saved-point) ))))

(defun dir-treeview-chmod (mode recursive &rest abs-filenames)
  "Apply the command `chmod´ with argument MODE to the files ABS-FILENAMES.
If RECURSIVE is non-nil, the flag \"-R\" is set in addition.

This is equivalent to

  chmod [-R] MODE ABS-FILENAMES

on the command line.  [-R] means -R is set if, and only if, RECURSIVE is
non-nil.

Returns t on success, and signals an error otherwise."
  (let ( (args abs-filenames) )
    (when recursive (push "-R" args))
    (push mode args)
    (apply 'dir-treeview-exec "chmod" nil args)))

(defun dir-treeview-change-mode (node)
  "Change the file mode of the file corresponding to NODE.

In particular, can be used to change file permissions and file executablility.
Calls the Unix/Linux command `chmod´.

The user is asked for the new mode in the minibuffer.  The mode must be
specified in a form understood by `chmod´.  If the file is a directory,
the user is asked if the mode should be changed recursively."
  (let* ( (abs-filename (treeview-get-node-prop node 'absolute-name))
          (filename (dir-treeview-local-filename abs-filename))
          (new-mode (read-from-minibuffer (format "Change mode of %s (chmod syntax): " filename)))
          (recursive (when (file-directory-p abs-filename)
                       (dir-treeview-user-confirm-y-or-n "Change mode recursively? "))) )
    (dir-treeview-chmod new-mode recursive abs-filename)
    (treeview-redisplay-node node) ))

(defun dir-treeview-change-mode-at-point ()
  "Change the file mode of the file corresponding to the node at point.

In particular, can be used to change file permissions and file executablility.
Calls the Unix/Linux command `chmod´.

Calls `dir-treeview-change-mode' for the node at point.  If there is no node at
point, does nothing."
  (interactive)
  (treeview-call-for-node-at-point 'dir-treeview-change-mode t t))

(defun dir-treeview-change-mode-of-selected-files ()
  "Change the mode (of the selected files.

In particular, can be used to change file permissions and file executablility.
Calls the Unix/Linux command `chmod´.

The user is asked for the new mode in the minibuffer.  The mode must be
specified in a form understood by `chmod´.  If there are directories among the
selected files, the user is asked if the mode of the directories should be
changed recursively."
  (interactive)
  (let ( (selected-nodes (treeview-get-all-selected-nodes)) (saved-point (point)) )
    (if selected-nodes
        (unwind-protect
            (let ( (new-mode (read-from-minibuffer "Change mode of selected files (chmod syntax): "))
                   (recursive (when (seq-some 'dir-treeview-directory-p selected-nodes)
                                (dir-treeview-user-confirm-y-or-n "Change mode of directories recursively? "))) )
              (apply 'dir-treeview-chmod new-mode recursive (seq-map 'dir-treeview-get-node-absolute-name selected-nodes))
              (dolist (node selected-nodes) (treeview-redisplay-node node)) )
          (treeview-unselect-all-nodes)
          (goto-char saved-point) ))))

(defun dir-treeview-change-mode-at-point ()
  "Change the file mode of the file at point or all selected files.

In particular, can be used to change file permissions and file executablility.
Calls the Unix/Linux command `chmod´.

If the node at point is selected, calls
`dir-treeview-change-mode-of-selected-files'.
Otherwise, calls `dir-treeview-change-mode' for the node at point.
If there is no node at point, does nothing."
  (interactive)
  (let ( (node (treeview-get-node-at-pos (point))) )
    (when node
      (if (treeview-node-selected-p node) (dir-treeview-change-mode-of-selected-files)
        (treeview-call-for-node node 'dir-treeview-change-mode t t) ))))

(defun dir-treeview-get-open-with-menu (node)
  "Create and return the 'Open with ...' submenu of the popup menu of NODE.
The submenu offers external programs or Lisp functions to open the file
represented by NODE.  The submenu is controlled by the customizable variable
`dir-treeview-open-commands'.

For the parent menu (the popup menu of NODE), see `dir-treeview-get-node-menu'."
  (let ( (filename (dir-treeview-get-node-absolute-name node))
         (command-table dir-treeview-open-commands)
         (menu (list "Open with . . ."))
         (menu-names ()) )
    (while command-table
      (let ( (key (nth 0 (car command-table))) )
        (if (if (functionp key) (funcall key filename) (string-match key filename))
            (let* ( (name (nth 1 (car command-table)))
                    (command (nth 0 (nth 2 (car command-table))))
                    (params (nth 1 (nth 2 (car command-table))))
                    (callback (if (functionp command)
                                  (list 'apply (list 'quote command) filename (list 'quote params))
                                (if (executable-find command)
                                    (list 'dir-treeview-open-with command (list 'quote params) filename)))) )
              (when (and callback (not (member name menu-names)))
                (setq menu (append menu (list (vector name callback)))
                      menu-names (cons name menu-names))) ))
        (setq command-table (cdr command-table))))
    (setq menu (append menu (list (vector "Other . . ." (list 'dir-treeview-open-with-other filename)))))
    menu))

(defun dir-treeview-get-default-node-menu (node)
  "Create and return the default popup menu for NODE.
This is the default implementation of the customizable variable
`dir-treeview-get-node-menu-function'."
  (if (treeview-node-selected-p node)
      ;; Node is selected
      (list "Selection"
            (vector "Delete selected files" (list 'dir-treeview-delete-selected-files))
            (vector "Open selected files" (list 'dir-treeview-open-selected-files))
            (vector "Open selected files with . . ." (list 'dir-treeview-open-selected-files-with))
            (vector "Change mode of selected files" (list 'dir-treeview-change-mode-of-selected-files))
            (vector "Change owner of selected files" (list 'dir-treeview-change-owner-of-selected-files)))
    ;; Node is not selected
    (let* ( (absolute-name (dir-treeview-get-node-absolute-name node))
            (local-name (dir-treeview-local-filename absolute-name))
            (selected-nodes-exist (treeview-selected-nodes-exist)) )
      (if (file-directory-p absolute-name)
          ;; Node is a directory
          (list local-name
                (vector "Open" (list 'find-file absolute-name))
                (vector "Open in Other Window" (list 'find-file-other-window absolute-name))
                (vector "Open in New Frame"  (list 'find-file-other-frame absolute-name))
                "--"
                (vector "Copy selected files here" (list 'dir-treeview-copy-selected-files-to absolute-name) :enable selected-nodes-exist)
                (vector "Move selected files here" (list 'dir-treeview-move-selected-files-to absolute-name) :enable selected-nodes-exist)
                "--"
                (dir-treeview-get-open-with-menu node)
                "--"
                (vector "Open Terminal" (list 'dir-treeview-open-terminal absolute-name))
                (vector "Re-read" (list 'treeview-refresh-node (list 'quote node)))
                (vector "Create File" (list 'dir-treeview-open-new-file absolute-name))
                (vector "Create Subdir" (list 'dir-treeview-create-subdir (list 'quote node)))
                "--"
                (vector "Copy" (list 'dir-treeview-copy-dir (list 'quote node)))
                (vector "Rename" (list 'dir-treeview-rename-file (list 'quote node)))
                (vector "Delete" (list 'dir-treeview-delete-dir (list 'quote node)))
                "--"
                (vector "Change mode" (list 'dir-treeview-change-mode (list 'quote node)))
                (vector "Change owner" (list 'dir-treeview-change-owner (list 'quote node))))
        ;; Node is not a directory
        (if (funcall dir-treeview-is-openable-in-editor-p-function absolute-name)
            ;; Node is openable in editor
            (list local-name
                  (vector "Open" (list 'find-file absolute-name))
                  (vector "Open in Other Window" (list 'find-file-other-window absolute-name))
                  (vector "Open in New Frame"  (list 'find-file-other-frame absolute-name))
                  "--"
                  (dir-treeview-get-open-with-menu node)
                  "--"
                  (vector "View" (list 'view-file absolute-name))
                  (vector "View in Other Window" (list 'view-file-other-window absolute-name))
                  (vector "View in New Frame" (list 'view-file-other-frame absolute-name))
                  "--"
                  (vector "Open Terminal" (list 'dir-treeview-open-terminal absolute-name))
                  "--"
                  (vector "Copy" (list 'dir-treeview-copy-file (list 'quote node)))
                  (vector "Rename" (list 'dir-treeview-rename-file (list 'quote node)))
                  (vector "Delete" (list 'dir-treeview-delete-file (list 'quote node)))
                  "--"
                  (vector "Change mode" (list 'dir-treeview-change-mode (list 'quote node)))
                  (vector "Change owner" (list 'dir-treeview-change-owner (list 'quote node))))
          ;; Node is not openable in editor
          (list local-name
                (dir-treeview-get-open-with-menu node)
                "--"
                (vector "Open Terminal" (list 'dir-treeview-open-terminal absolute-name))
                "--"
                (vector "Copy" (list 'dir-treeview-copy-file absolute-name))
                (vector "Rename" (list 'dir-treeview-rename-file absolute-name))
                (vector "Delete" (list 'dir-treeview-delete-file absolute-name))
                "--"
                (vector "Change mode" (list 'dir-treeview-change-mode (list 'quote node)))
                (vector "Change owner" (list 'dir-treeview-change-owner (list 'quote node)))) )))) )

(defun dir-treeview-get-node-menu (node)
  "Create and return the popup menu for NODE.
The popup menu is the menu which (by default) is bound to right mouse button.
This function calls the function stored in the customizable variable
`dir-treeview-get-node-menu-function'.  Its default implementation is
`dir-treeview-get-default-node-menu'."
  (funcall dir-treeview-get-node-menu-function node))

(defun dir-treeview-popup-node-menu-at-mouse (event)
  "Show the popup menu of the node where EVENT occurred.
EVENT must be a mouse event.  If there is no such node, does nothing."
  (interactive "@e")
  (let ( (node (treeview-get-node-at-event event)) )
    (if node (popup-menu (dir-treeview-get-node-menu node)))))

(defun dir-treeview-popup-node-menu-at-point ()
  "Show the popup menu of the node at point.
If there is no node at point, does nothing."
  (interactive)
  (let ( (node (treeview-get-node-at-pos (point))) )
    (if node (popup-menu (dir-treeview-get-node-menu node) (posn-at-point)))))

(defun dir-treeview-save-menu-options ()
  "Save the Dir Treeview options that can be set in the menu."
  (interactive)
  (let ( (variables '(dir-treeview-show-hidden-files
                      dir-treeview-show-backup-files)) )
    (while variables
      (let ( (variable (car variables)) )
        (customize-save-variable variable (symbol-value variable))
        (setq variables (cdr variables))))
    (dir-treeview-save-theme)
    (message "Options saved")))

(defun dir-treeview-toggle-option (variable)
  "Toggle the option represented by VARIABLE."
  (set variable (not (symbol-value variable)))
  (treeview-refresh-tree)
  (force-mode-line-update))

(defun dir-treeview-toggle-show-hidden-files ()
  "Toggle whether hidden files are shown or not."
  (interactive)
  (dir-treeview-toggle-option 'dir-treeview-show-hidden-files))

(defun dir-treeview-toggle-show-backup-files ()
  "Toggle whether backup files are shown or not."
  (interactive)
  (dir-treeview-toggle-option 'dir-treeview-show-backup-files))

(defun dir-treeview-customize ()
  "Open the customization buffer of the group \"dir-treeview\"."
  (interactive)
  (customize-group 'dir-treeview))

(defvar dir-treeview-theme-alist nil
  "Alist with all Dir Treeview themes and their display names.
The keys are theme symbols, the values the coorresponding display names.")

(defun dir-treeview-theme-p (theme)
  "Return non-nil if THEME is a Dir Treeview theme, otherwise nil.
Dir Treeview theme are defined by the keys of `dir-treeview-theme-alist'.
Thus, THEME is considered a Dir Treeview theme if, an only if, it is contained
in the keys of `dir-treeview-theme-alist'."
  (and (symbolp theme) (assq theme dir-treeview-theme-alist)))

(defun dir-treeview-register-theme (theme display-name)
  "Add THEME with corresponding DISPLAY-NAME to `dir-treeview-theme-alist'.
If `dir-treeview-theme-alist' already contains an entry for THEME, thew display
name is updated."
  (let ( (entry (assq theme dir-treeview-theme-alist)) )
    (if entry
        (setcdr entry display-name)
      (setq dir-treeview-theme-alist (cons (cons theme display-name) dir-treeview-theme-alist)) )))

(defun dir-treeview-get-themes ()
  "Return all Dir Treeview themes, as a list of theme symbols.
These are all keys of `dir-treeview-theme-alist'."
  (mapcar 'car dir-treeview-theme-alist))

(defun dir-treeview-get-enabled-themes ()
  "Return a list of all currently enabled Dir Treeview themes.
Theses are all currently enabled themes for which `dir-treeview-theme-p' returns
non-nil.  Usually, there should be at most one enabled Dir Treeview theme at a
time."
  (let (themes)
    (dolist (theme custom-enabled-themes)
      (when (dir-treeview-theme-p theme) (setq themes (cons theme themes))))
    themes))

(defun dir-treeview-disable-themes ()
  "Disable all currently enabled Dir Treeview themes.
That are all currently enabled themes whose name starts with \"dir-treeview-\".
Usually, there should be at most one enabled Dir Treeview theme at a time."
  (interactive)
  (dolist (theme (dir-treeview-get-enabled-themes)) (disable-theme theme)))

(defun dir-treeview-get-theme-display-name (theme)
  "Return the display name of THEME.
THEME must be a Dir Treeview theme, as a theme symbol.
See `dir-treeview-theme-p' for the criterion when a theme is considered a
Dir Treeview theme."
    (let ( (entry (assq theme dir-treeview-theme-alist)) )
      (if entry (cdr entry)
        (error "Not a dir-treeview theme: %s" theme) )) )

(defun dir-treeview-get-themes-display-names ()
  "Return the display names of all Dir Treeview themes, as a list.
These are all values of `dir-treeview-theme-alist'."
  (mapcar 'cdr dir-treeview-theme-alist))

(defun dir-treeview-get-enabled-themes-display-names ()
  "Return the display names of all enabled Dir Treeview themes, as a list.
Usually, there should be at most one enabled Dir Treeview theme at a time.
See `dir-treeview-theme-p' for a definition what a Dir Treeview theme is."
  (mapcar 'dir-treeview-get-theme-display-name (dir-treeview-get-enabled-themes)))

(defun dir-treeview-get-theme-for-display-name (display-name)
  "Return the Dir Treeview theme for DISPLAY-NAME, as a theme symbol.
This is the key mapped to DISPLAY-NAME in `dir-treeview-theme-alist'.
If no such key exists, an error is thrown.
See also `dir-treeview-theme-p' for the term \"Dir Treeview theme\"."
  (let ( (entry (rassoc display-name dir-treeview-theme-alist)) )
    (unless entry (error "Not a dir-treeview theme: %s" display-name))
    (car entry)))

(defun dir-treeview-load-theme (name &optional no-confirm)
  "Load and apply the Dir Treeview theme with the display name NAME.

NO-CONFIRM has the same meaning as in `load-theme'.

Before loading, any currently enabled Dir Treeview theme is disabled.
After loading, the theme is enabled and applied to all Dir Treeview buffers.

It is also allowd that NAME is \"None\", in which case no theme is used, and Dir
Treeview is displayed in the default, themeless way.

See `dir-treeview-theme-p' for a definition what a Dir Treeview themes is."
  (interactive (list (completing-read "Load Dir Treeview theme: " (dir-treeview-get-themes-display-names)) nil))
  (dir-treeview-disable-themes)
  (unless (string-equal name "None")
    (load-theme (dir-treeview-get-theme-for-display-name name) no-confirm))
  (dolist (buffer (dir-treeview-get-buffers))
    (with-current-buffer buffer (treeview-refresh-tree))))

(defun dir-treeview-add-theme-dir-by-load-file-name ()
  "Add the directory of `load-file-name' to `custom-theme-load-path'.
If `load-file-name' is set and `custom-theme-load-path' is defined, the
directory part of `load-file-name' is added to `custom-theme-load-path'
provided it is not already contained.

Note that `load-file-name', if set, contains the path of the currently loaded
Lisp file.  If the latter is part of a package, and the package provides a
theme, this function can be called to add the package directory to
`custom-theme-load-path' so that the theme can be found."
  (when (and (boundp 'custom-theme-load-path) load-file-name)
    (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))))

(defun dir-treeview-create-theme-menu (&optional _unused)
  "Create the menu for selecting the theme."
  (let ( (menu (make-sparse-keymap "Theme")) (names-of-enabled (dir-treeview-get-enabled-themes-display-names)) )
    (define-key menu
      [none]
      (list 'menu-item (if names-of-enabled "None" "None  ⏺") 'dir-treeview-disable-themes))
    (define-key menu [sep-1] '(menu-item "--" nil))
    (dolist (name (sort (dir-treeview-get-themes-display-names) 'string-greaterp))
      (define-key menu
        (vector (intern (downcase name)))
        (list 'menu-item
              (if (member name names-of-enabled) (concat name "  ⏺") name)
              `(lambda () (interactive) (dir-treeview-load-theme ,name)))) )
    menu))

(defvar dir-treeview-theme-file
  (concat (file-name-as-directory user-emacs-directory) "dir-treeview-theme.txt")
  "Text file in which the theme name is stored.")

(defun dir-treeview-read-theme ()
  "Read the display name of the Dir Treeview theme from `dir-treeview-theme-file'."
  (let ( (theme nil) )
    (if (file-exists-p dir-treeview-theme-file)
        (with-temp-buffer
          (insert-file-contents dir-treeview-theme-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ( (line (buffer-substring (line-beginning-position) (line-end-position))) )
              (unless (string-match-p "^\\s-*#\\|^\\s-*$" line)
                (if theme
                    (error "Multiple Dir Treeview themes stored in %s" dir-treeview-theme-file)
                  (setq theme (string-trim line))) ))
            (forward-line 1) )))
    theme))

(defun dir-treeview-save-theme ()
  "Write the display name of the Dir Treeview theme to `dir-treeview-theme-file'."
  (interactive)
  (let ( (themes (dir-treeview-get-enabled-themes-display-names)) )
    (if themes
      (if (> (length themes) 1)
          (error "Multiple Dir Treeview themes enabled - cannot store theme")
        (with-temp-buffer
          (insert "# Conatins the name of the dir-treeview theme.") (newline)
          (insert "# This file is automatically written.")  (newline 2)
          (insert (nth 0 themes)) (newline)
          (write-region (point-min) (point-max) dir-treeview-theme-file))
        (message "Theme saved"))
      (message "No theme to save")) ))

(defvar dir-treeview-saved-theme-loaded nil
  "Whether the theme saved in `dir-treeview-theme-file' was already loaded.
This variable is initially nil and will be set to a non-nil value by
`dir-treeview-load-saved-theme'.")

(defun dir-treeview-load-saved-theme ()
  "Laod the theme saved in `dir-treeview-theme-file'.
Sets `dir-treeview-saved-theme-loaded' to t.
When `dir-treeview-theme-file' does not exist, doen't load a theme, but sets
`dir-treeview-saved-theme-loaded' to t anyway."
  (let ( (name (dir-treeview-read-theme)) )
    (when name (dir-treeview-load-theme name)))
  (setq dir-treeview-saved-theme-loaded t))

(defun dir-treeview-create-local-keymap ()
  "Create and return the local keymap for Dir Treeview buffers."
  (let ( (map (make-keymap)) )
    (define-key map "h" 'dir-treeview-toggle-show-hidden-files)
    (define-key map "b" 'dir-treeview-toggle-show-backup-files)
    (define-key map (kbd "<down>") 'treeview-next-line)
    (define-key map (kbd "<up>") 'treeview-previous-line)
    (define-key map (kbd "C-<up>") 'treeview-goto-first-sibling)
    (define-key map (kbd "C-<down>") 'treeview-goto-last-sibling)
    (define-key map (kbd ".") 'treeview-refresh-subtree-at-point)
    (define-key map (kbd "=") 'treeview-refresh-tree)
    (define-key map (kbd "e") 'dir-treeview-popup-node-menu-at-point)
    (define-key map (kbd "d") 'dir-treeview-delete-at-point)
    (define-key map (kbd "<delete>") 'dir-treeview-delete-at-point)
    (define-key map (kbd "c") 'dir-treeview-copy-file-or-dir-at-point)
    (define-key map (kbd "C") 'dir-treeview-copy-selected-files-to-dir-at-point)
    (define-key map (kbd "r") 'dir-treeview-rename-file-at-point)
    (define-key map (kbd "t") 'dir-treeview-open-terminal-at-point)
    (define-key map (kbd "f") 'dir-treeview-open-new-file-at-point)
    (define-key map (kbd "s") 'dir-treeview-create-subdir-at-point)
    (define-key map (kbd "a") 'treeview-toggle-select-node-at-point)
    (define-key map (kbd "A") 'treeview-select-gap-above-node-at-point)
    (define-key map (kbd "i") 'dir-treeview-show-info-for-node-at-point)
    (define-key map (kbd "M-m") 'dir-treeview-change-mode-at-point)
    (define-key map (kbd "M-o") 'dir-treeview-change-owner-at-point)
    (define-key map [menu-bar treeview]
      (cons "Dir-Treeview" (make-sparse-keymap "Dir-Treeview")))
    (define-key map [menu-bar treeview customize]
      '(menu-item
        "Customize"
        dir-treeview-customize
        :help "Customize Dir Treeview"))
    (define-key map [menu-bar treeview save-options]
      '(menu-item
        "Save Options"
        dir-treeview-save-menu-options
        :help "Save the changes made in this nenu"))
    (define-key map [menu-bar treeview sep]
      '(menu-item "--"))
    (define-key map [menu-bar treeview theme]
      '(menu-item "Theme" nil :filter dir-treeview-create-theme-menu))
    (define-key map [menu-bar treeview show-backup-files]
      '(menu-item
        "Show Backup Files"
        dir-treeview-toggle-show-backup-files
        :button (:toggle . (symbol-value 'dir-treeview-show-backup-files))
        :help "Toggle whether backup files are shown or not"))
    (define-key map [menu-bar treeview show-hidden-files]
      '(menu-item
        "Show Hidden Files"
        dir-treeview-toggle-show-hidden-files
        :button (:toggle . (symbol-value 'dir-treeview-show-hidden-files))
        :help "Toggle whether hidden files are shown or not"))
    map))

(add-hook 'kill-buffer-hook #'dir-treeview-shutdown-file-watch-if-last-buffer)

(dir-treeview-add-theme-dir-by-load-file-name)
(dir-treeview-register-theme 'dir-treeview-pleasant "Pleasant")

(provide 'dir-treeview)

;; Load additional themes if installed:
(require 'dir-treeview-themes nil t)

;;; dir-treeview.el ends here
