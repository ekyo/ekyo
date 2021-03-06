;;; dot-emacs.el --- Ekyo's configuration;;;
;;
;; Copyright (c) 2012, 2013 Simon Kérouack <ekyo777@gmail.com>
;;
;; Author: Simon Kérouack <ekyo777@gmail.com>
;; URL: https://github.com/ekyo/ekyo
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Configuration I use on a daily basis.  Use whatever you like.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defvar *emacs-load-start* (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure required packages are installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defvar ekyo-packages
  '(ack-and-a-half
    auto-complete
    buffer-move
    clojure-mode
    coffee-mode
    color-theme-monokai
    color-theme-sanityinc-tomorrow
    csharp-mode
    deft
    diminish
    d-mode
    expand-region
    flymake-coffee
    flymake-d
    flymake-haskell-multi
    flymake-json
    flymake-yaml
    ghc
    ghci-completion
    gist
    ;git-gutter+
    god-mode
    groovy-mode
    haml-mode
    haskell-mode
    helm
    helm-projectile
    ido-ubiquitous
    inf-ruby
    linum-relative
    magit
    ;magithub
    markdown-mode
    multi-term
    omnisharp
    paredit
    projectile
    python
    rainbow-mode
    sass-mode
    scion
    scss-mode
    smart-tabs-mode
    smex
    thesaurus
    volatile-highlights
    yaml-mode
    yari
    yasnippet
    zeal-at-point
    )
  "A list of packages to ensure are installed at launch.")

(message "%s" "Emacs Prelude is now refreshing its package database...")
(package-refresh-contents)
(message "%s" " done.")


(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; install the missing packages
(dolist (p ekyo-packages)
  (require-package p))

(provide 'ekyo-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Place Backup Files in Specific Directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push "/usr/bin" exec-path)
;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(setq cursor-type 'bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load auto-complete-mode on startup
; autocomplete - Emacs auto-complete-mode at startup - Stack Overflow
; http://stackoverflow.com/questions/8095715/emacs-auto-complete-mode-at-startup
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-ignore-case t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)
; Dirty hack to enable auto-complete-mode in all modes except the
; minibuffer mode.
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)
(setq ac-use-menu-map t)
; Cycle candidates with C-n and C-p
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

; Isearch auto-complete results with control-space.
; This is the most natural mapping for me.
(define-key ac-menu-map (kbd "C-SPC") 'ac-isearch)

; Search in all open buffers with C-M-SPC
;;(define-key evil-insert-state-map (kbd "C-M-SPC") 'ac-complete-words-in-all-buffer)

; A more ergonomic mapping to start auto-completing. Though this
; will mess up my writing sometimes. TODO
;; https://github.com/auto-complete/auto-complete/issues/188
;;(define-key evil-insert-state-map (kbd "C-SPC")
;;  (lambda ()
;;    (interactive)
;;    (auto-complete)
;;    (ac-complete-with-helm)))


; Delay before automatically showing the completion popup menu.
; In seconds.
(setq ac-delay 0.1)

; Auto-complete, by default, will search for matching words in
; buffers in the same mode, and snippets
(setq-default ac-sources
              '(ac-source-words-in-same-mode-buffers
                ac-source-abbrev
                ac-source-yasnippet))

;; Start auto-complete automatically when 3 chars have been
;; entered
(setq ac-auto-start 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename current buffer file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation Hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yank indent
;; Automatically indent yanked text if in a programming mode.
(defvar yank-indent-modes
  '(emacs-lisp-mode c-mode c++-mode js2-mode emacs-lisp-mode
                    LaTeX-mode prolog-mode python-mode scheme-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text
(with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text
(with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(setq whitespace-line-column 120) ;; limit line length

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Re-indents the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "<f10>")
                'iwb)

;; Using smart-tabs
(setq standard-indent 2)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq c-basic-offset 2)
(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

(global-set-key (kbd "<f12>")
                'indent-buffer)

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-c q")
                'kill-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thesaurus configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Thesaurus Key
(eval-after-load "thesaurus"
  '(progn (setq thesaurus-bhl-api-key "699761ef74acd451675d335fa614f48e")))

(global-set-key (kbd "C-x t")
                'thesaurus-choose-synonym-and-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell-script hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run a shell command in a shell buffer.
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(eval-after-load "sh-script"
  '(progn
     (define-key sh-mode-map (kbd "C-j") 'sh-send-line-or-region-and-step)
     (define-key sh-mode-map (kbd "C-c C-z") 'sh-switch-to-process-buffer)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tmux control from emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tmux-session-name 0)
(setq tmux-window-name 1)
(setq tmux-pane-number 1)

(defun tmux-setup (x y z)
  "Setup global variables for tmux session, window, and pane"
  (interactive "sEnter tmux session name: \nsEnter tmux window name: \nsEnter tmux pane number: ")
  (setq tmux-session-name x)
  (setq tmux-window-name y)
  (setq tmux-pane-number z)
  (message "Tmux Setup, session name: %s, window name: %s, pane number: %s"
           tmux-session-name tmux-window-name tmux-pane-number))

(defun tmux-exec (command)
  "Execute command in tmux pane"
  (interactive)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s '%s' Enter" tmux-session-name tmux-window-name tmux-pane-number command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Howdoi inside emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun howdoi ()
  (interactive
   (let ((args (read-string "howdoi: " nil 'my-history)))
     (async-shell-command (concat "howdoi " args))
     )))

(global-set-key (kbd "C-c C-h")
                'howdoi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use mouse in terminal. eh... it can be of use sometimes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(xterm-mouse-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only enable linum-mode during goto-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "C-g")
                'goto-line-with-feedback)
                                        ;(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<f5>")
                'linum-mode)

(global-set-key (kbd "<f6>")
                'linum-relative-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)
(global-set-key (kbd "C-z")
                'undo-tree-visualize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(add-to-list 'helm-boring-file-regexp-list ".local/share/Trash")
(global-set-key (kbd "C-c C-f")
                'helm-for-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(haskell-mode-hook (lambda ()
                       (flymake-haskell-multi-load)
                       (ghci-completion-mode)
                       (turn-on-haskell-indentation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "RET")
                'newline-and-indent)

(global-set-key (kbd "C-/")
                'comment-or-uncomment-region)

(global-set-key (kbd "C-S-p")
                'smex)

(global-set-key (kbd "C-c C-s")
                'shell)

(global-set-key (kbd "C-c C-d")
                'zeal-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme Adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(color-theme-monokai)
;; Remove Scroll Bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;
(set-default 'truncate-lines nil)
(setq truncate-partial-width-windows nil)

(setq echo-keystrokes 0.01)
(setq frame-title-format '("%f - " user-real-login-name "@" system-name))
(setq inhibit-startup-screen t)

(blink-cursor-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(tool-bar-mode -1)
(which-function-mode t)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-ignore-extensions t)
(setq ido-use-filename-at-point 'guess)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-enable-prefix nil)
(setq ido-max-prospects 8)
(setq ido-use-filename-at-point 'guess)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-ubiquitous t)
(smex-initialize)

(flyspell-mode 0)
;(flyspell-prog-mode)

;; show paren mode
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match-face nil :underline t)

;(set-default-font
; "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;(set-default-font
; "-unknown-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Remove comment from scratch
(setq initial-scratch-message nil)

;; Set font size
(set-face-attribute 'default nil :height 110)

;; Set cursor color
(defun refresh-cursor-color ()
  (interactive ())
  (set-cursor-color "#AEFEBD"))

(add-hook 'create-frame-hook 'refresh-cursor-color)
(define-key global-map (kbd "C-c C-c") 'refresh-cursor-color)
(refresh-cursor-color)

;; C-n inserts newline if point at end of buffer
(setq next-line-add-newlines t)

;; Show diff with last commit while editing a file, shows oneOf "+=- " in margin
;(global-git-gutter+-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mode-line-cleaner-alist
  '((auto-complete-mode . " ☯")
    (paredit-mode . " ☂")
    (projectile-mode . " ⚑")
    (undo-tree-mode . "")
    (yas-minor-mode . " υ")
    (emacs-lisp-mode . " EL")
    (eldoc-mode . "")
    (elisp-slime-nav-mode . "")
    (prelude-mode . "")
    (rainbow-mode . "")
    (volatile-highlights-mode . "")
    (whitespace-mode . " ☠")
    (coffee-mode . "☕")))

;; Nice characters to use: (C-x 8 C-h to see all characters)
;; ❄☃Φ
;; Some japanese: のぬるぬるスクロール＆ミニマッ

(defun clean-mode-line ()
  "Replace frequent modes with single characters on the mode line."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(set-face-attribute 'mode-line nil :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :inverse-video nil
                    :box nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-coffee)
(setq flymake-log-level 3)
(setq flymake-coffee-coffeelint-configuration-file "/usr/lib/node_modules/coffeelint/coffeelint.json")
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(require 'flymake-json)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)

(require 'd-mode)
(add-hook 'd-mode-hook 'flymake-d-load)

(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  "."
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-mail-address          "ekyo777@gmail.com")
(setq user-full-name             "Simon Kérouack")

(provide 'dot-emacs)
;;; dot-emacs.el ends here
