;;; init.el --- A Clean and Modern Emacs Configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Core Emacs Settings & UI Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Performance ---
;; Set the garbage collection threshold to a higher value during startup.
;; It will be reset to a lower value after initialization.
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 800 1024))))

;; --- Basic UI ---
(setq-default
 inhibit-startup-message t      ; Disable the startup screen
 ring-bell-function 'ignore     ; Silence the bell
 indent-tabs-mode nil           ; Use spaces, not tabs
 )

(set-default-coding-systems 'utf-8)
(global-hl-line-mode 1)         ; Highlight the current line
(blink-cursor-mode -1)          ; No blinking cursor
(tool-bar-mode -1)              ; Disable the tool bar
(menu-bar-mode -1)              ; Disable the menu bar
;;(scroll-bar-mode -1)            ; Disable the scroll bar
(electric-pair-mode 1)
(pixel-scroll-precision-mode 1) ; Enable smooth scrolling

;; --- Font Setup (from Nano Emacs) ---
(set-face-attribute 'default nil :height 120 :family "Roboto Mono")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)

;; --- Frame Setup (from Nano Emacs) ---
(setq default-frame-alist
      '((height . 40) (width . 120) (left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)))

;; --- File Backups & Auto-Saves ---
;; This is your excellent backup configuration, just moved into one place.
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t))
      auto-save-list-file-prefix (concat user-emacs-directory "auto-saves/sessions_")
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil) ; Hide .# lockfiles

;; Create the directories if they don't exist
(make-directory (file-name-as-directory (expand-file-name "backups/" user-emacs-directory)) t)
(make-directory (file-name-as-directory (expand-file-name "auto-saves/" user-emacs-directory)) t)

;; --- Clipboard Integration ---
;; Your clipboard code is good. This ensures it works in both GUI and terminal.
(setq select-enable-clipboard t)
(unless (display-graphic-p)
  (when (and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy"))
    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste))
  (when (and (getenv "DISPLAY") (executable-find "xclip"))
    (setq interprogram-cut-function
          (lambda (text &optional push)
            (with-temp-buffer
              (insert text)
              (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard"))))
    (setq interprogram-paste-function (lambda () (shell-command-to-string "xclip -o -selection clipboard")))))

;; --- History and Recent Files ---
;; Essential for remembering your work between sessions.
(use-package savehist
  :ensure nil ; Built-in
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil ; Built-in
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. The Nano Theme
;; This is the theme logic from your nano-emacs.el.
;; It's better to keep it together as its own "package".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface nano-default '((t)) "") (defface nano-default-i '((t)) "")
(defface nano-highlight '((t)) "") (defface nano-highlight-i '((t)) "")
(defface nano-subtle '((t)) "") (defface nano-subtle-i '((t)) "")
(defface nano-faded '((t)) "") (defface nano-faded-i '((t)) "")
(defface nano-salient '((t)) "") (defface nano-salient-i '((t)) "")
(defface nano-popout '((t)) "") (defface nano-popout-i '((t)) "")
(defface nano-strong '((t)) "") (defface nano-strong-i '((t)) "")
(defface nano-critical '((t)) "") (defface nano-critical-i '((t)) "")

(defun nano-set-face (name &optional foreground background weight)
  (apply #'set-face-attribute `(,name nil ,@(when foreground `(:foreground ,foreground)) ,@(when background `(:background ,background)) ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute `(,(intern (concat (symbol-name name) "-i")) nil :foreground ,(face-background 'nano-default) ,@(when foreground `(:background ,foreground)) :weight regular)))

(defun nano-link-face (sources faces &optional attributes)
  (let ((attributes (or attributes '(:foreground :background :family :weight :height :slant :overline :underline :box))))
    (dolist (face (seq-filter #'facep faces))
      (dolist (attribute attributes) (set-face-attribute face nil attribute 'unspecified))
      (set-face-attribute face nil :inherit sources))))

(defun nano-install-theme ()
  (set-face-attribute 'default nil :foreground (face-foreground 'nano-default) :background (face-background 'nano-default))
  (dolist (item '((nano-default . (variable-pitch variable-pitch-text fixed-pitch fixed-pitch-serif))
                  (nano-highlight . (hl-line highlight))
                  (nano-subtle . (match region lazy-highlight widget-field))
                  (nano-faded . (shadow font-lock-comment-face font-lock-doc-face))
                  (nano-popout . (warning font-lock-string-face))
                  (nano-salient . (success link help-argument-name custom-visibility font-lock-type-face font-lock-keyword-face font-lock-builtin-face))
                  (nano-strong . (font-lock-function-name-face font-lock-variable-name-face minibuffer-prompt))
                  (nano-critical . (error))
                  (nano-faded-i . (help-key-binding))
                  (nano-default-i . (custom-button-mouse isearch))
                  (nano-critical-i . (isearch-fail))
                  ((nano-subtle nano-strong) . (custom-button))
                  ((nano-faded-i nano-strong) . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))
  (set-face-attribute 'header-line nil :background 'unspecified :underline nil :box `( :line-width 1 :color ,(face-background 'nano-default)) :inherit 'nano-subtle)
  (set-face-attribute 'mode-line nil :background (face-background 'default) :underline (face-foreground 'nano-faded) :height 40 :overline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :background (face-background 'default) :underline (face-foreground 'nano-faded) :height 40 :overline nil :box nil))

(defun nano-dark (&rest args)
  (interactive)
  (nano-set-face 'nano-default "#ECEFF4" "#2E3440")
  (nano-set-face 'nano-strong "#ECEFF4" nil 'regular)
  (nano-set-face 'nano-highlight nil "#3B4252")
  (nano-set-face 'nano-subtle nil "#434C5E")
  (nano-set-face 'nano-faded "#677691")
  (nano-set-face 'nano-salient "#81A1C1")
  (nano-set-face 'nano-popout "#D08770")
  (nano-set-face 'nano-critical "#EBCB8B")
  (nano-install-theme))

;; Load the dark theme by default
;; (nano-dark)

(use-package sublime-themes
  :init
  (load-theme 'spolsky t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Minibuffer Completion: The Vertico Stack ✨
;; This replaces the icomplete setup from nano-emacs with a modern, powerful
;; and extensible completion system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :init
  (vertico-mode)
  ;; Grow the minibuffer automatically
  (setq resize-mini-windows 'grow-only))

(use-package marginalia
  :after vertico
  :init
  ;; Must be enabled before vertico
  (marginalia-mode))

(use-package consult
  :bind (;; Search and navigation
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ;; Find file in project or home directory
         ;;("C-x C-f" . consult-find)
         ))

;; Use a more flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Essential Packages & Quality of Life 🧑‍💻
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package exec-path-from-shell
  :if (memq system-type '(darwin gnu/linux))
  :init (exec-path-from-shell-initialize))

(use-package yasnippet
  :init
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Programming - Rust Setup 🦀
;; This is your existing Rust setup, which is already very well structured.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Improve LSP & GC performance
(setq read-process-output-max (* 3 1024 1024))

;; --- Autocompletion with Company ---
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("TAB"   . company-complete-selection)))

;; --- LSP (Language Server Protocol) ---
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((rustic-mode . lsp-deferred)
         ;; ✅ ADD THIS HOOK to handle formatting on save reliably
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
         ("C-." . lsp-execute-code-action))
  :config
  ;; ✅ ADD THIS to automatically format on save using LSP
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  :custom
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.6)
  (lsp-eldoc-render-all t)
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-run-build-scripts t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-code-actions t))

;; --- Rustic Mode for Rust ---
(use-package rustic
  :after lsp-mode
  :custom
  ;; We let lsp-mode handle formatting, so this can be turned off.
  (rustic-format-on-save nil) ; ⬅️ CHANGED THIS from t to nil
  :config
  ;; Ensure LSP completions go through company
  (setq-default company-backends '(company-capf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. OS-Specific Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        ns-function-modifier 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Finalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "✅ Emacs configuration loaded successfully!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11"
     "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Dashboard Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List of Zen Buddhist quotes for dashboard footer
(defvar my/zen-quotes
  '(
    "Sitting quietly, doing nothing, Spring comes, and the grass grows by itself."
    "Before I had studied Zen for thirty years, I saw mountains as mountains, and waters as waters."
    "All sentient beings are essentially Buddhas."
    "When you realize nothing is lacking, the whole world belongs to you. - Lao Tzu"
    "The feeling that any task is a nuisance will soon disappear if it is done in mindfulness. - Thich Nhat Hanh"
    "Each morning, we are born again. What we do today is what matters most. - Buddha"
    "Let go, or be dragged. - Zen Proverb"
    "To be beautiful means to be yourself. You don't need to be accepted by others. You need to accept yourself. - Thich Nhat Hanh"
    "If you are depressed, you are living in the past. If you are anxious, you are living in the future. If you are at peace, you are living in the present. - Lao Tzu"
    "Do not dwell in the past, do not dream of the future, concentrate the mind on the present moment. - Buddha"
    "The present moment is filled with joy and happiness. If you are attentive, you will see it. - Thich Nhat Hanh"
    "What you are looking for is already in you. You already are everything you are seeking. - Thich Nhat Hanh"
    "Throughout this life, you can never be certain of living long enough to take another breath. - Huang Po"
    "When thoughts arise, then do all things arise. When thoughts vanish, then do all things vanish. - Huang Po"
    "The resistance to the unpleasant situation is the root of suffering. - Ram Dass"
    "Self-realization is effortless. What you are trying to find is what you already are. - Ramesh Balsekar"
    "Things are as they are. Looking out into the universe at night, we make no comparisons between right and wrong stars. - Alan Watts"
    "Wisdom says we are nothing. Love says we are everything. Between these two our life flows. - Jack Kornfield"
    "If you miss the present moment, you miss your appointment with life. That is very serious! - Thich Nhat Hanh"
    "The real meditation is how you live your life. - Jon Kabat-Zinn"
    )
  "Collection of Zen Buddhist quotes for the dashboard footer.")

(use-package nerd-icons
  :init)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  
  ;; Customize dashboard appearance
  (setq dashboard-banner-logo-title "Welcome to Zen Emacs")
  (setq dashboard-startup-banner 'official)  ; Disable banner since no image support
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  
  ;; Additional dashboard customization
  (setq dashboard-page-separator "\n\n")
  (setq dashboard-set-heading-icons nil)  ; Disable icons
  (setq dashboard-set-file-icons nil)     ; Disable icons
  (setq dashboard-set-navigator nil)      ; Disable navigator icons
  
  ;; Set Zen quotes as footer (will show random quote)
  (setq dashboard-footer-messages my/zen-quotes)
  
  ;; Set dashboard as initial buffer with refresh
  (setq initial-buffer-choice (lambda () 
                                (dashboard-refresh-buffer)
                                (get-buffer "*dashboard*"))))
