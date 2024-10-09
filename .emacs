;;; package -- Sumary
;; 03-03-2022 09:30:26
;; Mario Gajardo Tassara custom Emacs config for windows/macOS
;; inspired by many tips from the internet
;; and some portions taken from Casey Muratori .emacs file

;;; Commentary:
;; if you want to point to diferent Emacs config files put this dummy Emacs lines on .emacs
;; Windows
;; place this file in C:\Users\Username\AppData\Roaming and point to the appropriate files

;;(load user-init-file)
;; updated on:
;; 09-10-2024 17:15:31


;;; Code:
;; -----------------------------------------------------------------------------
;; EMACS CORE SETUPS
;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; put here your emacs custom themes path
(when (eq system-type 'windows-nt)
  (add-to-list 'custom-theme-load-path "c:\\Users\\mario\\AppData\\Roaming\\.emacs.d\\themes\\")
  (setq default-directory "D:/dev/github/"))
;; customs functions
(add-to-list 'load-path "C:\\Users\\mario\\AppData\\Roaming\\.emacs.d\\lisp\\")

(when (eq system-type 'darwin)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (setq default-directory "~/github/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "gtHtmlFunctions")
(load "gtUtilFunctions")
(load "xah-html-mode")
(load "jai-mode")

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
 '(ansi-color-names-vector
   ["#002732" "#ec423a" "#93a61a" "#c49619" "#3c98e0" "#e2468f" "#3cafa5" "#8d9fa1"])
 '(beacon-color "#ed0547ad8099")
 '(column-number-mode t)
 (when (eq system-type 'windows-nt)
   '(company-clang-executable "c:/Program Files/LLVM/bin/clang.exe"))
 (when (eq system-type 'darwin)
   '(company-clang-executable "/opt/homebrew/opt/llvm/bin/clang"))
 '(custom-enabled-themes '(suscolors))
 '(display-line-numbers nil)
 '(display-time-mode nil)
 '(fringe-mode 50 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors
   ((("#0e332f" "#0e332f" "green")
     . 0)
    (("#06343d" "#06343d" "brightcyan")
     . 20)))
 '(inhibit-startup-screen t)
 '(linum-format " %7i ")
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(go-mode fzf column-enforce-mode powershell json-mode zig-mode company-emoji eldoc lsp-ui htmlize lsp-mode company-lua lua-mode company-c-headers common-lisp-snippets exec-path-from-shell flycheck-clang-analyzer flycheck-irony company-irony-c-headers company-irony irony company-web company-fuzzy company rainbow-blocks rainbow-identifiers rainbow-delimiters apropospriate-theme flycheck-indicator flycheck-inline flycheck multiple-cursors whole-line-or-region yasnippet))
 '(safe-local-variable-values '((Syntax . ANSI-Common-Lisp)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; start emacs server only if isn't started yet
(if (server-mode nil)
    (server-start))

;; -----------------------------------------------------------------------------
;; GENERAL SETUPS
;; -----------------------------------------------------------------------------

;; WINDOW SETUP
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(split-window-horizontally)
;;(setq default-frame-alist '((left . 3) (width . 110) (fullscreen . fullheight)))

;; show full file path on window tittle
(setq frame-title-format
      '("" invocation-name ": "
        (:eval
         (if buffer-file-name
             (abbreviate-file-name buffer-file-name)
           "%b"))))

;; my info
(setq-default user-full-name "your name"
              user-mail-address "youremail@server.com")

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; font setup
(when (eq system-type 'windows-nt)
  (set-frame-font "Hack Nerd Font 11" nil t))
(when (eq system-type 'darwin)
  (set-frame-font "Hack Nerd Font 14" nil t))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; clock
(display-time)

;; no abbrevs
(setq-default save-abbrevs nil)

;; large text files
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; 80 col visual guide
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
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

;; exclude files and folders from recentf
(add-to-list 'recentf-exclude "\\elpa")

;; ibuffer hide TAGS buffers
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\TAGS")

;; rainbow curlys
(require 'rainbow-delimiters)
(setq-default rainbow-delimiters-mode t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(setq-default rainbow-identifiers-mode t)
(setq-default rainbow-blocks-mode t)

;; emoji crap
(if ( version< "27.0" emacs-version )
    (when (eq system-type 'windows-nt)
      (set-fontset-font "fontset-default" 'unicode "Segoe UI Emoji" nil 'prepend))
  (when (eq system-type 'darwin)
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))
  (warn "This Emacs version is too old to properly support emoji."))

;; (require 'column-enforce-mode)
;; (modify-face 'column-enforce-face "hot pink" nil nil t nil t nil nil)
;; (add-hook 'prog-mode-hook #'column-enforce-mode)
;; (add-hook 'text-mode-hook #'column-enforce-mode)


;; -----------------------------------------------------------------------------
;; KEYBINDINGS
;; -----------------------------------------------------------------------------

;; system-types
;; macOS = darwin
;; windows = windows-nt
;; linux = gnu/linux
;; free BSD = gnu/kfreebsd

;; use all the special keys on the mac keyboard
(when (eq system-type 'darwin)
  (setq-default mac-option-modifier nil
                ns-function-modifier 'super
                mac-right-command-modifier 'hyper
                mac-right-option-modifier 'alt
                mac-command-modifier 'meta))

;; super key to left windows key on Windows NT systems
(when (eq system-type 'windows-nt)
  (setq-default w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super))

;; bind for enlarge o shrink windows splits
(global-set-key (kbd "M-s-k") 'enlarge-window)
(global-set-key (kbd "M-s-i") 'shrink-window)
(global-set-key (kbd "M-s-l") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-j") 'enlarge-window-horizontally)

;; toggle show/hidde invisibles
(global-set-key (kbd "C-M-#") 'whitespace-mode)

;; abort with esc instead of C-g
(global-set-key (kbd "<escape>") (kbd "C-g"))

;; multiple cursors
(global-set-key (kbd "C-M-&") 'mc/edit-lines)

;; lock file
(global-set-key (kbd "<f7>") 'read-only-mode)

;; describe function
(global-set-key (kbd "M-_") 'describe-function)

;; quickly open emacs config
(global-set-key
 (kbd "<f8> <f8>")
 (lambda ()
   (interactive)
   (when (eq system-type 'windows-nt)
     (find-file "C:\\Users\\mario\\AppData\\Roaming\\.emacs"))
   (when (eq system-type 'darwin)
     (find-file "~/.emacs"))))

;; quickly open todo file
(global-set-key
 (kbd "<f9> <f9>")
 (lambda ()
   (interactive)
   (when (eq system-type 'windows-nt)
     (find-file "c:\\Users\\mario\\text.txt"))
   (when (eq system-type 'darwin)
     (find-file "~/text.txt")))
 )

;; add functions menu bar
;;(global-set-key (kbd "C-!") 'imenu-add-menubar-index)
;;(global-set-key (kbd "C-M-&") 'lsp-ui-imenu)

;; flycheck list errors
(global-set-key (kbd "<f5>") 'flycheck-list-errors)

;; find file
(global-set-key (kbd "C-,") 'find-file)

;; save buffer
(global-set-key (kbd "C-.") 'gt-save-buffer)

;; kill buffer
(define-key key-translation-map (kbd "M-C-=") (kbd "M-C-¿"))
(global-set-key (kbd "M-C-¿") 'kill-buffer)

;; buffer nav
(define-key key-translation-map (kbd "C--") (kbd "C-'"))
(global-set-key (kbd "C-'") 'gt-prev-user-buffer)
(define-key key-translation-map (kbd "C-=") (kbd "C-¿"))
(global-set-key (kbd "C-¿") 'gt-next-user-buffer)
(define-key key-translation-map (kbd "C-_") (kbd "C-?"))
(global-set-key (kbd "C-?") 'gt-prev-emacs-buffer)
(define-key key-translation-map (kbd "C-+") (kbd "C-¡"))
(global-set-key (kbd "C-¡") 'gt-next-emacs-buffer)
(define-key key-translation-map (kbd "M-C-=") (kbd "M-C-¿"))
(global-set-key (kbd "C-|") 'ibuffer)

;; Move thru camelCase
(global-subword-mode 1) ; 1 for on, 0 for off

;; Indent
(define-key global-map [C-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-according-to-mode)

;; select all
(define-key key-translation-map (kbd "C-M-/") (kbd "C-M--"))
(global-set-key (kbd "C-M--") 'mark-whole-buffer)

;; select html current element block
(global-set-key (kbd "C-M-,") 'xah-html-select-element)

;; vi-like line insertion
(global-set-key (kbd "C-o") (lambda () (interactive)(beginning-of-line)(open-line 1)))
(global-set-key (kbd "M-o") (lambda () (interactive)(end-of-line)(newline)))

;; copy line
(define-key key-translation-map (kbd "C-;") (kbd "C-{"))
(global-set-key (kbd "C-{") 'gt-copy-line)
;; copy word
(define-key key-translation-map (kbd "M-;") (kbd "M-{"))
(global-set-key (kbd "M-{") 'gt-copy-word)

;; move line up
(global-set-key (kbd "C-M-o") 'move-line-up)
;; move line down
(global-set-key (kbd "C-M-p") 'move-line-down)

;; kill region
(define-key global-map [M-backspace] 'whole-line-or-region-kill-region)

;; kill word at point
(define-key global-map [C-M-backspace] 'gt-kill-word-at-point)

;; comment/uncomment
(global-set-key (kbd "C-!") 'comment-or-uncomment-region)

;; find word under cursor
(define-key key-translation-map (kbd "C-M-_") (kbd "M-¡"))
(global-set-key (kbd "M-¡") 'isearch-forward-symbol-at-point)

;; search replace
(define-key key-translation-map (kbd "C-M-+") (kbd "C-M-¡"))
(define-key global-map (kbd "C-M-¡") 'gt-replace-string)

;; eval buffer
(define-key global-map (kbd "C-M-!") 'eval-buffer)
;; eval region
(define-key global-map (kbd "C-M-\"") 'eval-region)

(when (eq system-type 'darwin)
  ;; FZF keybindings
  (global-set-key (kbd "C-M-s-f") 'fzf)
  (global-set-key (kbd "C-M-s-s") 'fzf-find-in-buffer)
  (global-set-key (kbd "C-M-s-r") 'fzf-recentf)
  (global-set-key (kbd "C-M-s-b") 'fzf-switch-buffer))


;; -----------------------------------------------------------------------------
;;  CODE SETUPS
;; -----------------------------------------------------------------------------

;; (use-package lsp-mode
;;   :ensure t
;;   :bind-keymap
;;   ("C-c l" . lsp-command-map)
;;   :custom
;;   (lsp-keymap-prefix "C-c l"))

;; lsp
(setq-default lsp-auto-guess-root nil)
(setq-default lsp-eldoc-render-all t)
(setq-default lsp-eldoc-hook nil)
(setq-default lsp-idle-delay 0.5)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq-default lsp-ui-peek-always-show t)
(setq-default lsp-ui-sideline-show-hover nil)
(setq-default lsp-ui-doc-enable t)

;; rust
(setq-default rustic-format-on-save t)
(setq-default lsp-rust-analyzer-cargo-watch-command "clippy")
(setq-default lsp-rust-analyzer-server-display-inlay-hints t)

(when (eq system-type 'darwin)
  ;; applescript mode
  (autoload 'applescript-mode "~/.emacs.d/modes/applescript-mode"
    "Major mode for editing AppleScript source." t)
  (add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode)))

;; LSP connections
(with-eval-after-load 'lsp-mode
  (define-key key-translation-map (kbd "<f6>") (kbd "C-M-;"))
  (define-key global-map (kbd "C-M-;") 'lsp-find-references)

  ;; zig
  (add-hook 'zig-mode-hook #'lsp)
  (when (eq system-type 'windows-nt)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '"c:/Archivos de programa/zls/zls.exe")
                      :major-modes '(zig-mode)
                      :priority -1
                      :server-id 'zls)))
  (when (eq system-type 'darwin)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '"~/zls/zig-out/bin/zls")
                      :major-modes '(zig-mode)
                      :priority -1
                      :server-id 'zls)))

  ;; c++
  (add-hook 'c++-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"clangd")
                    :major-modes '(c++-mode)
                    :priority -1
                    :server-id 'clangd))

  ;; html
  (add-hook 'html-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"html-ls")
                    :major-modes '(html-mode)
                    :priority -1
                    :server-id 'html-ls))

  ;; css
  (add-hook 'css-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"css-ls")
                    :major-modes '(css-mode)
                    :priority -1
                    :server-id 'css-ls))

  ;; json
  (add-hook 'json-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"json-ls")
                    :major-modes '(json-mode)
                    :priority -1
                    :server-id 'json-ls))

  ;; bash
  (add-hook 'sh-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"bash-language-server start")
                    :major-modes '(sh-mode)
                    :priority -1
                    :server-id 'bash-language-server))

  ;; xml
  (add-hook 'xml-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"xmlls")
                    :major-modes '(xml-mode)
                    :priority -1
                    :server-id 'xmlls))

  ;; go
  (add-hook 'go-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '"gopls")
                    :major-modes '(go-mode)
                    :priority -1
                    :server-id 'gopls))
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  ;; ;; csharp
  ;; (add-hook 'csharp-mode-hook #'omnisharp-start-omnisharp-server)
  ;; (add-hook 'csharp-mode-hook #'lsp)
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '"omnisharp")
  ;;                   :major-modes '(omnisharp-mode)
  ;;                   :priority -1
  ;;                   :server-id 'omnisharp))
  )

;; build make setup
(when (eq system-type 'windows-nt)
  (setq-default make-directory "c:/Users/mario/test/")
  (setq-default gt-makescript "build.bat"))
(when (eq system-type 'darwin)
  (setq-default make-directory "~/test")
  (setq-default gt-makescript "Make"))

;; tabs setup
(setq-default tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default lua-basic-offset 4)
(setq-default objc-basic-offset 4)
(setq-default c-basic-offset 4)
(setq-default c++-basic-offset 4)

;; tags setup
;; Don't ask before rereading the TAGS files if they have changed
(setq-default tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq-default large-file-warning-threshold nil)

;; flycheck setup
(require 'flycheck)
(global-flycheck-mode)
(setq flycheck-checker-error-threshold 2000)
(flycheck-clang-analyzer-setup)
(add-hook 'flycheck-mode-hook #'flycheck-indicator-mode)
(add-hook 'flycheck-mode-hook #'flycheck-inline-mode)

(when (eq system-type 'windows-nt)
  (setq flycheck-clang-include-path (list
                                     "c:\\Users\\mario\\OneDrive\\Documentos\\code\\sourceCode\\sdl\\windows\\sdl_includes"
                                     "c:\\Program Files (x86)\\Windows Kits\\10\\Include" )))
(when (eq system-type 'darwin)
  (setq flycheck-clang-include-path (list
                                     "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/"
                                     "/opt/homebrew/include" )))

;; Define the modes/packages you need
(require 'company)
(require 'irony)
(require 'company-c-headers)

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)
(setq-default company-idle-delay 0.4)
(setq-default company-minimum-prefix-lenght 1)
(setq-default company-selection-wrap-around t)
(setq-default company-ctags-fuzzy-match-p t)

;; Here I define a function so it can be called anytime I want to load it
(defun irony-comp-setup-basic()
  ;; function to add path to company-c-headers
  (defun company-c-headers-includes ()
    ;; You just need to modified the path inside the quote to your header files path
    (when (eq system-type 'windows-nt)
      (add-to-list 'company-c-headers-path-system
                   "c:\\Users\\mario\\OneDrive\\Documentos\\code\\sourceCode\\sdl\\windows\\sdl_includes"
                   "c:\\Program Files (x86)\\Windows Kits\\10\\Include"))
    (when (eq system-type 'darwin)
      (add-to-list 'company-c-headers-path-system
                   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/"
                   "/opt/homebrew/include"))
    )
  ;; Now call this function so it add your path to company-c-header-path-system
  (company-c-headers-includes)

  ;; Irony-mode configuration
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)

  ;; This add your company-c-headers to company-backends
  (add-to-list 'company-backends 'company-c-headers)

  ;; Default config for company-irony mode
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-irony))

  ;; For irony mode I think
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  )

;; Now call this function to active it
(irony-comp-setup-basic)

;;(require 'clang-format)
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;;clang-format -style=google -dump-config > .clang-format
;;(global-set-key (kbd "C-c C-f") 'clang-format-region)

;; BRIGHT-COLOR TODOs
(setq fixme-modes '(zig-mode rust-mode text-mode indented-text-mode c++-mode c-mode objc-mode emacs-lisp-mode shell-script-mode))

(make-face 'font-lock-check-face)
(make-face 'font-lock-bug-face)
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(check\\)" 1 'font-lock-check-face t)
           ("\\<\\(bug\\)" 1 'font-lock-bug-face t)
           ("\\<\\(fixme\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(note\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(modify-face 'font-lock-check-face "VioletRed1" nil nil t nil t nil nil)
(modify-face 'font-lock-bug-face "red1" nil nil t nil t nil nil)
(modify-face 'font-lock-fixme-face "DarkGoldenrod2" nil nil t nil t nil nil)
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
                 ("\\.jai$"         . jai-mode)
                 ("\\.json$"        . json-mode)
                 ("\\.py$"          . python-mode)
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
