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

;; Disable graphical dialog as they do not work
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent 'yes-or-no-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent 'y-or-n-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))

;; Show line number
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d")

(setq case-replace nil)
(setq case-fold-search nil)

;; Hightlight line
(global-hl-line-mode 1)

;; Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Evil mode
(require 'evil)
(evil-mode 1)

(mapc (lambda (element)
        (let ((mode (car element))
              (state (cdr element)))
          (evil-set-initial-state mode state)))
      '((inferior-emacs-lisp-mode . emacs)
        (inferior-python-mode . emacs)
        (nrepl-mode . insert)
        (pylookup-mode . emacs)
        (comint-mode . normal)
        (shell-mode . insert)
        (git-commit-mode . insert)
        (git-rebase-mode . emacs)
        (term-mode . emacs)
        (help-mode . emacs)
        (helm-grep-mode . emacs)
        (grep-mode . emacs)
        (bc-menu-mode . emacs)
        (magit-branch-manager-mode . emacs)
        (rdictcc-buffer-mode . emacs)
        (dired-mode . emacs)
        (wdired-mode . normal)))


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

(defun current-callable ()
  "Find the current function or class name."
  (save-excursion
    (re-search-backward
     "^ \\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun pytest-current-module ()
  "Run pytest on current file."
  (interactive)
  (shell-command (concat "py.test " (buffer-file-name)))
)

(defun pytest-current-func ()
  "Run pytest on current function or class."
  (interactive)
  (shell-command (concat "py.test " (buffer-file-name) " -k " (current-callable)))
)

(defun eshell-here (command)
  "Opens up a new shell in the directory associated with the current buffer's file and run the COMMAND."
  (interactive)
  (let* ((parent (file-name-directory (buffer-file-name)))
         (name   (car
                  (last
                   (split-string parent "/" t)))))
    (split-window-vertically)
    (other-window 1)
    (eshell "new")

    (insert (concat command))
    (eshell-send-input)))

(defun pytest-current-func-eshell ()
  "Run pytest on current function or class in eshell."
  (interactive)
  (eshell-here (concat "py.test " (buffer-file-name) " -s -k " (current-callable)))
)

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is Emacs LISP, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffix-map
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            )
          )
         (f-name (buffer-file-name))
         (f-suffix (file-name-extension f-name))
         (prog-name (cdr (assoc f-suffix suffix-map)))
         (cmd-str (concat prog-name " \""   f-name "\""))
         )

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified.  Do you want to save first? ")
          (save-buffer) ) )

    (if (string-equal f-suffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension f-name))
      (if prog-name
          (progn
            (message "Running…")
            (shell-command cmd-str "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ) ) ))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(evil-leader/set-key
  "e" 'helm-mini
  "f" 'helm-find-files
  "g" 'helm-do-grep
  "h" 'delete-window  ;; hide window
  "k" 'kill-buffer
  "l" 'flycheck-list-errors
  "m" 'magit-status
  "o" 'delete-other-windows
  "p" 'helm-projectile
  "r" 'run-current-file
  "s" 'evil-window-split
  "t" 'visit-term-buffer
  "v" 'evil-window-vsplit
  "w" 'delete-trailing-whitespace
  "x" (bind (execute-extended-command nil)))
;; (set-key-for-mode 'python-mode (kbd "<SPC> b") 'set-ipdb)
(evil-define-key 'normal python-mode-map (kbd "<SPC> a") 'venv-workon)
(evil-define-key 'normal python-mode-map (kbd "<SPC> b") 'set-ipdb)
(evil-define-key 'normal python-mode-map (kbd "<SPC> d") 'remove-ipdb)
(evil-define-key 'normal python-mode-map (kbd "<SPC> m") 'pytest-current-module)
(evil-define-key 'normal python-mode-map (kbd "<SPC> f") 'pytest-current-func)
(evil-define-key 'normal python-mode-map (kbd "<SPC> if") 'pytest-current-func-eshell)


;; Swap ";" and ":" in evil mode
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;; Themes
(load-theme 'monokai t)

;; Cursor colour
(set-face-background 'cursor "Firebrick")
(setq evil-default-cursor t)


;; Don't show startup screen
(setq inhibit-startup-screen t)


;; give the underscore the word syntax-class in all C-like buffers.
(modify-syntax-entry ?_ "w")
;; Show keystrokes
;;(setq echo-keystrokes 0.02)

;; Enable Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'powerline)
(powerline-center-evil-theme)

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
;; make it happy
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

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

;; Put autosave files (ie #foo#) and backup files (ie foo~) in
;; ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; disable automatic line break in html mode
;; (add-hook 'html-mode-hook 'turn-off-auto-fill)
;; (add-hook 'html-model-hook
;;           (function (lambda ()
;;                       (setq evil-shift-width 2))))

(eval-after-load 'html-mode
  '(progn
     (turn-off-auto-fill)
     (setq evil-shift-width 2)
     (setq tab-width 2)))

(custom-set-variables
 '(ls-lisp-verbosity nil))

(add-hook 'term-exec-hook
  (function
   (lambda ()
     (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))

;; zsh is problematic for managing path
(setq shell-file-name "/bin/bash")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; do not prettify lambdas
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)
;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; gitgutter+
(global-git-gutter+-mode t)

(defun eshell/x ()
  "Delete window."
  (delete-window))

(provide 'user)
;;; user.el ends here
