;;; package --- Summary
;;; Commentary:
;; Emacs configuration

;;; Code:
;; http://cask.github.io/
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Keeps ~Cask~ file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(require 'pallet)

;; Show line number
(global-linum-mode t)

(setq case-replace nil)
(setq case-fold-search nil)

;; Evil mode
(require 'evil)
(evil-mode 1)
;(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'inferior-python-mode 'emacs)

;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; ipdb set/remove
(defun set-ipdb ()
  "Add debug code and move line down."
  (interactive)
  (back-to-indentation)
  (let ((x (current-column)))
    (insert "import ipdb; ipdb.set_trace()##########\n")
    (insert (make-string x ?\s))
    (save-buffer)))

(defun remove-ipdb ()
  "Remove py debug code, if found."
  (interactive)
  (let ((cur (point)))
    (goto-char (point-min))
    (delete-matching-lines "^[ ]*import ipdb; ipdb.set_trace()##########")
    (save-buffer)
    (goto-char cur)))

(defmacro bind (&rest COMMANDS)
  "Convience macro which create a lambda interactive COMMANDS."
  `(lambda ()
     (interactive)
     ,@COMMANDS))

(evil-leader/set-key
  "c" (bind
        (evil-window-split)
        (windmove-down)
        (eshell))
  "e" 'switch-to-buffer
  "f" 'find-file
  "k" 'kill-buffer
  "l" 'flycheck-list-errors
  "o" 'delete-other-windows
  "q" 'delete-window
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "w" 'delete-trailing-whitespace
  "x" (bind (execute-extended-command nil)))
(evil-leader/set-key-for-mode 'python-mode "b" 'set-ipdb)
(evil-leader/set-key-for-mode 'python-mode "d" 'remove-ipdb)
(evil-leader/set-key-for-mode 'python-mode "r" 'run-python)


;; Swap ";" and ":" in evil mode
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;; Themes
(load-theme 'monokai t)
(setq evil-default-cursor (quote (t "Firebrick")))

;; Don't show startup screen
(setq inhibit-startup-screen t)


;; Show keystrokes
(setq echo-keystrokes 0.02)

;; Enable Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'powerline)
(powerline-center-evil-theme)

;; Git
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Surround
(require 'surround)
(global-surround-mode 1)

;; Complete brackets, quotes, etc
(electric-pair-mode 1)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; No unnecessary whitespaces
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; Indicates the 79th column for python
(require 'fill-column-indicator)
(setq-default fci-rule-column 79)
(add-hook 'python-mode-hook 'fci-mode)

;; jedi completion
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(require 'virtualenvwrapper)
(venv-initialize-eshell)

;; Ipython integration with fgallina/python.el
(require 'python)
(defun setup-ipython ()
  "Setup ipython integration with `python-mode'."
  (interactive)

  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n"))

(evilnc-default-hotkeys)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'user)
;;; user.el ends here
