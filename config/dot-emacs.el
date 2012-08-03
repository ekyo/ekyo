(setq standard-indent 2)

(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; Set Thesaurus Key
(eval-after-load "thesaurus"
  '(progn (setq thesaurus-bhl-api-key "699761ef74acd451675d335fa614f48e")))

(global-set-key (kbd "C-z")
                'undo)

(global-set-key (kbd "C-x t")
                'thesaurus-choose-synonym-and-replace)

(global-set-key (kbd "C-g")
                'goto-line)

(global-set-key (kbd "<f11>")
                'djcb-full-screen-toggle)

(global-set-key (kbd "C-<f5>")
                'linum-mode)

;; Remove Scroll Bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-mail-address          "ekyo777@gmail.com")
(setq user-full-name             "Simon Kerouack")
