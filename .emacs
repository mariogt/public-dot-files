;;; package -- Sumary

;; Mario Gajardo Tassara
;; MarioGT Software
;; https://www.mariogt.com
;; mario@mariogt.com

;; inspired by many tips from the internet, including some portions taken from
;; Casey Muratori and Xah (http://ergoemacs.org/) config files.

;;; Commentary:

;; if you want to point to different location for your Emacs config files, put
;; this code into your dummy .emacs file, mods the paths to fit windows or
;; macos/linux conventions.

;;(setq user-init-file "/some/dir/.emacs")
;;(setq user-emacs-directory "/some/dir/.emacs.d/")
;;(setq default-directory (concat (getenv "HOME") "/some/dir/"))
;;(load user-init-file)

;; -- Windows --
;; place this file in C:\Users\Username\AppData\Roaming
;; -- macOS & linux --
;; place this file on your home root

;;; Code:
;; -----------------------------------------------------------------------------
;; EMACS CORE SETUPS
;; -----------------------------------------------------------------------------

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("elpa"  . "http://elpa.gnu.org/packages/")))

;; put here your emacs custom themes path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; customs functions
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "gtHtmlFunctions")
(load "gtUtilFunctions")
(load "xah-html-mode")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;; all themes are safe!
(setq custom-safe-themes t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(beacon-color "#F8BBD0")
 '(column-number-mode t)
 '(company-clang-executable
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++")
 '(custom-enabled-themes '(naysayer))
 '(diff-hl-show-hunk-posframe-internal-border-color "#ffffffffffff")
 '(display-line-numbers nil)
 '(display-time-mode t)
 '(exwm-floating-border-color "#504945")
 '(fci-rule-color "#504945")
 '(fringe-mode 10 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315"))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors '(("#F8BBD0" . 0) ("#FAFAFA" . 100)))
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#a89984"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#79740e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#928374"))
 '(linum-format " %7i ")
 '(objed-cursor-color "#9d0006")
 '(package-selected-packages
   '(zone-nyan php-mode c-eldoc css-eldoc irony-eldoc eldoc lsp-ui dap-mode htmlize lsp-mode rustic flycheck-rust company-lua lua-mode blox gruvbox-theme company-c-headers slime-company slime common-lisp-snippets exec-path-from-shell magit flycheck-clang-analyzer flycheck-irony company-irony-c-headers company-irony irony company-web company-fuzzy company company-ctags rainbow-blocks rainbow-identifiers rainbow-delimiters doom-themes apropospriate-theme flycheck-cask flycheck-indicator flycheck-inline flycheck-objc-clang flycheck multiple-cursors sr-speedbar whole-line-or-region yasnippet))
 '(pdf-view-midnight-colors (cons "#282828" "#fbf1c7"))
 '(pos-tip-background-color "#ffffffffffff")
 '(pos-tip-foreground-color "#78909C")
 '(powerline-color1 "#6D3300")
 '(powerline-color2 "#411E00")
 '(rustic-ansi-faces
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
 '(safe-local-variable-values '((Syntax . ANSI-Common-Lisp)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tabbar-background-color "#ffffffffffff")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#fbf1c7")
 '(vc-annotate-color-map
   (list
    (cons 20 "#79740e")
    (cons 40 "#8d7410")
    (cons 60 "#a17512")
    (cons 80 "#b57614")
    (cons 100 "#b3620e")
    (cons 120 "#b14e08")
    (cons 140 "#af3a03")
    (cons 160 "#af472e")
    (cons 180 "#b0545a")
    (cons 200 "#b16286")
    (cons 220 "#aa415b")
    (cons 240 "#a32030")
    (cons 260 "#9d0006")
    (cons 280 "#9a2021")
    (cons 300 "#97413c")
    (cons 320 "#946258")
    (cons 340 "#504945")
    (cons 360 "#504945")))
 '(vc-annotate-very-old-color nil))
;;'(menu-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; -----------------------------------------------------------------------------
;; GENERAL SETUPS
;; -----------------------------------------------------------------------------

;; WINDOW SETUP
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(split-window-horizontally)
(setq default-frame-alist '((left . 300) (width . 150) (fullscreen . fullheight)))

;; show full file path on window tittle
(setq frame-title-format
      '("" invocation-name ": "
        (:eval
         (if buffer-file-name
             (abbreviate-file-name buffer-file-name)
           "%b"))))

;; my info
(setq-default user-full-name "Mario Gajardo Tassara"
              user-mail-address "mario@mariogt.com")

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; font setup
(set-frame-font "Hack 13" nil t)

;; clock
(display-time)

;; nyan
;;(nyan-mode 1)

;; large text files
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; 80 col visual guide
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'conf-unix-mode-hook #'display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)

;; set fill paragraph column size
(setq-default fill-column 80)

;; word wrap
(global-visual-line-mode 1)

;; no bell
(setq-default visible-bell t)

;; kill scratch message crap
(setq initial-scratch-message nil)

;; Double-spaces after periods is morally wrong.
(setq sentence-end-double-space nil)

;; Never ding at me, ever.
(setq ring-bell-function 'ignore)

;; Prompts should go in the minibuffer, not in a GUI.
(setq use-dialog-box nil)

;; never ever split my window !!
(setq split-window-preferred-function 'gt-never-split-a-window)

;; stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; reload files when needed
(global-auto-revert-mode 1)

;; load yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; load whole line
(require 'whole-line-or-region)
(whole-line-or-region-global-mode 1)

;; no backup/autosave
(setq-default make-backup-files nil)
(setq-default imenu-auto-rescan t)
(setq-default imenu-auto-rescan-maxout 500000)
(setq-default kept-new-versions 5)
(setq-default kept-old-versions 5)
(setq-default make-backup-file-name-function (quote ignore))
(setq-default auto-save-default nil)
(setq-default auto-save-interval 0)
(setq-default auto-save-list-file-prefix nil)
(setq-default auto-save-timeout 0)
(setq-default delete-auto-save-files nil)
(setq-default delete-old-versions (quote other))

;; org mode dir
(setq-default org-directory "~/Documents/org/")

;; make return key also do indent, globally
(electric-indent-mode 1)

;; open new files on the same frame
(setq-default ns-pop-up-frames nil)
;; truncate stuff
(setq-default next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-window nil)

;; shifted arrow
(windmove-default-keybindings)

;; restore split pane config, winner-undo, winner-redo
(winner-mode 1)

;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)
;; exclude elpa from recentf
(add-to-list 'recentf-exclude "\\elpa")

;; ibuffer hide TAGS buffers
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\TAGS")

;; ;; rainbow curlys
;; (require 'rainbow-delimiters)
;; (setq-default rainbow-delimiters-mode t)
;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; (setq-default rainbow-identifiers-mode t)
;; (setq-default rainbow-blocks-mode t)

;; speedbar global setup
;; delete/skip old windows
(setq-default speedbar-delete-windows t)
(setq-default sr-speedbar-skip-other-window-p t)
;; no icons
(setq-default speedbar-use-images nil)
;; on the left
(setq-default sr-speedbar-right-side nil)
;; show all files
(setq-default speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
(setq-default speedbar-show-unknown-files t)
;; use text for buttons
(setq-default speedbar-use-images nil)
;; no auto refresh
;;(setq sr-speedbar-auto-refresh nil)

;; emoji crap
(if ( version< "27.0" emacs-version ) ; )
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (warn "This Emacs version is too old to properly support emoji."))

;; (require 'column-enforce-mode)
;; (modify-face 'column-enforce-face "salmon3" nil nil t nil t nil nil)
;; (add-hook 'prog-mode-hook #'column-enforce-mode)
;; (add-hook 'text-mode-hook #'column-enforce-mode)


;; -----------------------------------------------------------------------------
;; KEYBINDINGS
;; -----------------------------------------------------------------------------

;; use all the special keys on the mac keyboard
(when (eq system-type 'darwin)
  (setq-default mac-option-modifier nil
                ns-function-modifier 'super
                mac-right-command-modifier 'hyper
                mac-right-option-modifier 'alt
                mac-command-modifier 'meta))

;; bind for enlarge o shrink windows splits
(global-set-key (kbd "A-<down>") 'enlarge-window)
(global-set-key (kbd "A-<up>") 'shrink-window)
(global-set-key (kbd "A-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "A-<left>") 'enlarge-window-horizontally)

;; toggle show/hidde invisibles
(global-set-key (kbd "C-M-#") 'whitespace-mode)

;; abort with esc instead of C-g
(global-set-key (kbd "<escape>") (kbd "C-g"))

;; toggle speedbar
(global-set-key (kbd "C-M-%") 'sr-speedbar-toggle)

;; multiple cursors
(global-set-key (kbd "C-M-^") 'mc/edit-lines)

;; quickly open emacs config
(global-set-key
 (kbd "<f8> <f8>")
 (lambda ()
   (interactive)
   (find-file "~/.emacs")))

;; quickly open todo file
(global-set-key
 (kbd "<f9> <f9>")
 (lambda ()
   (interactive)
   (find-file "~/Documents/remember.txt")))

;; add functions menu bar
(global-set-key (kbd "C-&") 'imenu-add-menubar-index)
(global-set-key (kbd "C-M-&") 'lsp-ui-imenu)

;; flycheck list errors
(global-set-key (kbd "C-M-*") 'flycheck-list-errors)

;; find file
(global-set-key (kbd "C-,") 'find-file)

;; save buffer
(global-set-key (kbd "C-.") 'gt-save-buffer)

;; kill buffer
(global-set-key (kbd "M-C-=") 'kill-buffer)

;; buffer nav
(global-set-key (kbd "C--") 'gt-prev-user-buffer)
(global-set-key (kbd "C-=") 'gt-next-user-buffer)
(global-set-key (kbd "C-–") 'gt-prev-emacs-buffer)
(global-set-key (kbd "C-≠") 'gt-next-emacs-buffer)
(global-set-key (kbd "C-|") 'ibuffer)

;; Move thru camelCase
(global-subword-mode 0) ; 1 for on, 0 for off

;; Indent
(define-key global-map [C-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-according-to-mode)

;; select all
(global-set-key (kbd "C-M-/") 'mark-whole-buffer)

;; select html current element block
(global-set-key (kbd "C-M-,") 'xah-html-select-current-element)

;; vi-like line insertion
(global-set-key (kbd "C-o") (lambda () (interactive)(beginning-of-line)(open-line 1)))
(global-set-key (kbd "M-o") (lambda () (interactive)(end-of-line)(newline)))

;; copy line
(global-set-key (kbd "C-'") 'gt-copy-line)
;; copy word
(global-set-key (kbd "M-'") 'gt-copy-word)

;; move line up
(global-set-key (kbd "C-M-<up>") 'move-line-up)
;; move line down
(global-set-key (kbd "C-M-<down>") 'move-line-down)

;; kill region
(define-key global-map [M-backspace] 'whole-line-or-region-kill-region)

;; kill word at point
(define-key global-map [C-M-backspace] 'gt-kill-word-at-point)

;; comment/uncomment
(global-set-key (kbd "C-!") 'comment-or-uncomment-region)

;; find word under cursor
(global-set-key (kbd "M-+") 'isearch-forward-symbol-at-point)

;; search replace
(define-key global-map (kbd "M-C-+") 'gt-replace-string)

;; eval buffer
(define-key global-map (kbd "C-M-!") 'eval-buffer)
;; eval region
(define-key global-map (kbd "C-M-?") 'eval-region)

;; slime repl
;; clear buffer
(define-key global-map (kbd "M-ç") 'slime-repl-clear-buffer)
;; find definition
(define-key global-map (kbd "M-≥") 'slime-edit-definition)


;; -----------------------------------------------------------------------------
;;  CODE SETUPS
;; -----------------------------------------------------------------------------

;; rust
;; rustfmt on save
(setq-default rustic-format-on-save t)

;; lsp
(setq-default lsp-rust-analyzer-cargo-watch-command "clippy")
(setq-default lsp-rust-analyzer-server-display-inlay-hints t)
(setq-default lsp-eldoc-render-all t)
(setq-default lsp-idle-delay 0.6)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq-default lsp-eldoc-hook nil)
;;(setq-default lsp-ui-peek-always-show t)
;;(setq-default lsp-ui-sideline-show-hover t)
;;(setq-default lsp-ui-doc-enable nil)

;; dap
(dap-ui-mode 1)
(dap-ui-controls-mode 1)
(require 'dap-lldb)
(require 'dap-gdb-lldb)
;; installs .extension/vscode
(dap-gdb-lldb-setup)
(dap-register-debug-template
 "Rust::LLDB Run Configuration"
 (list :type "lldb"
       :request "launch"
       :name "LLDB::Run"
       :gdbpath "rust-lldb"
       :target nil
       :cwd nil))

;; lisp
(setq-default inferior-lisp-program "sbcl")

;; shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; build make setup
(setq-default gt-makescript "make test")
(setq-default make-directory "~/code/sourceCode/")

;; tabs setup
(setq-default tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default lua-basic-offset 4)
(setq-default objc-basic-offset 4)
(setq-default c-basic-offset 4)
(setq-default c++-basic-offset 4)

;; applescript mode
(autoload 'applescript-mode "~/.emacs.d/modes/applescript-mode"
  "Major mode for editing AppleScript source." t)
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

;; tags setup
;; Don't ask before rereading the TAGS files if they have changed
(setq-default tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq-default large-file-warning-threshold nil)

;; c - c++ setups
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; irony setups
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook #'irony-eldoc)

;; flycheck setup
(require 'flycheck)
;;(global-flycheck-mode)
(setq flycheck-checker-error-threshold 2000)
(flycheck-clang-analyzer-setup)
;;(add-hook 'flycheck-mode-hook #'flycheck-objc-clang-setup)
(add-hook 'flycheck-mode-hook #'flycheck-indicator-mode)
(add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
(add-hook 'flycheck-mode-hook #'flycheck-cask-setup)
(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
;;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; company setups
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default company-idle-delay 0.4)
(setq-default company-minimum-prefix-lenght 1)
(setq-default company-selection-wrap-around t)
(setq-default company-ctags-fuzzy-match-p t)
(company-ctags-auto-setup)
(setq-default company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/")
(setq-default company-c-headers-path-user "/opt/homebrew/include")

;;(require 'clang-format)
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;;clang-format -style=google -dump-config > .clang-format
;;(global-set-key (kbd "C-c C-f") 'clang-format-region)

;; BRIGHT-COLOR TODOs
(setq fixme-modes '(text-mode indented-text-mode c++-mode c-mode objc-mode emacs-lisp-mode shell-script-mode applescript-mode))

(make-face 'font-lock-check-face)
(make-face 'font-lock-bug-face)
(make-face 'font-lock-fixed-face)
(make-face 'font-lock-note-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(check\\)" 1 'font-lock-check-face t)
           ("\\<\\(bug\\)" 1 'font-lock-bug-face t)
           ("\\<\\(fixed\\)" 1 'font-lock-fixed-face t)
           ("\\<\\(note\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(modify-face 'font-lock-check-face "VioletRed1" nil nil t nil t nil nil)
(modify-face 'font-lock-bug-face "red1" nil nil t nil t nil nil)
(modify-face 'font-lock-fixed-face "DarkGoldenrod2" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "magenta1" nil nil t nil t nil nil)

;; Accepted file extensions and their appropriate modes
(setq-default auto-mode-alist
              (append
               '(("\\.cpp$"         . c++-mode)
                 ("\\.hin$"         . c++-mode)
                 ("\\.cin$"         . c++-mode)
                 ("\\.inl$"         . c++-mode)
                 ("\\.rdc$"         . c++-mode)
                 ("\\.h$"           . c++-mode)
                 ("\\.c$"           . c++-mode)
                 ("\\.cc$"          . c++-mode)
                 ("\\.metal$"       . c++-mode)
                 ("\\.c8$"          . c++-mode)
                 ("\\.txt$"         . indented-text-mode)
                 ("\\.md$"          . indented-text-mode)
                 ("\\.emacs$"       . emacs-lisp-mode)
                 ("\\.gen$"         . gen-mode)
                 ("\\.ms$"          . fundamental-mode)
                 ("\\.m$"           . objc-mode)
                 ("\\.mm$"          . objc-mode)
                 ("\\.applescript$" . applescript-mode)
                 ("\\MakeFile$"     . makefile-mode)
                 ("\\MAKE$"         . makefile-mode)
                 ("\\.rs$"          . rustic-mode)
                 ("\\.css$"         . css-mode)
                 ) auto-mode-alist))


;; -----------------------------------------------------------------------------
;; DEFUNS
;; -----------------------------------------------------------------------------
;;  EMACS DEFUNS mov/save/nav/copy/search
;; -----------------------------------------------------------------------------

;; save current buffer, remove trailing whitespace, and detabify only if the
;; buffer isn't a Makefile, because tabs are fundamental on these.

(defun gt-save-buffer ()
  "Save the buffer after delete trailing ws and untabify if its not a Make file."
  (interactive)
  (save-excursion
    (widen)
    (if (string-equal "Make" (substring (buffer-name)))
        (untabify (point-min) (point-max))
      )
    (delete-trailing-whitespace)
    (save-buffer)
    (message "buffer ws trailed, untabify (if is not a Make file) and saved")))

;; never split the fucking window
(defun gt-never-split-a-window ()
  "Never, ever split a window.  Why would anyone EVER want you to do that??"
  nil)

;; function for load all recent files
(defun gt-reload-all-recent-files ()
  "Reload recent files."
  (interactive)
  (dolist (file  recentf-list) (find-file file)))

;; kill word at point
(defun gt-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun gt-kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (gt-kill-thing-at-point 'word))

;; function for easy copy of lines
(defun gt-copy-line ()
  "Copy line."
  (interactive)
  (kill-ring-save (point-at-bol) (point-at-eol))
  (message "line copied"))

;; copy word
(defun get-point (symbol &optional arg)
  "SYMBOL ARG Get the point."
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "BEGIN-OF-THING END-OF-THING ARG Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun gt-copy-word (&optional arg)
  "ARG Copy words at point."
  (interactive "P")
  (copy-thing 'forward-word 'backward-word arg)
  (message "word copied"))

;; search replace string
(defun gt-replace-string (FromString ToString)
  "FROMSTRING TOSTRING Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))

;; move lines
(defun move-line-up ()
  "Move lines up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move lines down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun gt-copy-rectangle-to-kill-ring (@begin @end)
  "Copy region as column (rectangle region) to `kill-ring'
See also: `kill-rectangle', `copy-to-register'.
URL `http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat 'identity (extract-rectangle @begin @end) "\n")))


;; -----------------------------------------------------------------------------
;; CODE DEFUNS
;; -----------------------------------------------------------------------------

;; open corresponding h or c, cpp files
(defun gt-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.m" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".c"))
        (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(defun gt-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (gt-find-corresponding-file)
  (other-window -1))

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p gt-makescript) t
    (cd "../")
    (find-project-directory-recursive)))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory make-directory)
  (switch-to-buffer-other-window "*compilation*")
  (setq last-compilation-directory default-directory))

;; make build
(defun build-code ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile gt-makescript))
  (other-window 1))


;; -----------------------------------------------------------------------------
;; BUFFERS NAVIGATION DEFUNS
;; -----------------------------------------------------------------------------

(defun gt-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `gt-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (gt-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun gt-prev-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (gt-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;; listado de buffers a ignorar
(defun gt-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command,
 so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal major-mode "tags-table-mode")
      nil
    (if (string-equal "*" (substring (buffer-name) 0 1))
        nil
      (if (string-equal major-mode "dired-mode")
          nil
        t
        ))))

;; listado de buffers internos de Emacs
(defun gt-next-emacs-buffer ()
  "Switch to the next Emacs buffer.
“Emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun gt-prev-emacs-buffer ()
  "Switch to the previous Emacs buffer.
“Emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

;;; .emacs ends here
