
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *emacs-load-start* (current-time))

;hi eldoc fill

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
    ;; protected form
    (progn
      (if (stringp feature)
        (load-library feature)
        (require feature))
      t)
    ;; error handler
    (file-error nil)))

(defmacro try-run (feature &rest code)
  `(when (try-require ,feature) (progn ,@code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro setq-all (&rest vars)
  (if (null vars) '(progn)
    `(progn
       (setq         ,(car vars) ,(cadr vars))
       (setq-default ,(car vars) ,(cadr vars))
       (setq-all     ,@(cddr vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cloj-on () (setq-all inferior-lisp-program "clojure"))
(defun scm-on  () (setq-all inferior-lisp-program "gsi"))
(defun lisp-on () (setq-all inferior-lisp-program "/usr/bin/sbcl"))

(lisp-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-all
  indent-tabs-mode         nil
  lisp-indent-offset       2
  tab-width                4
  c-basic-offset           4
  c-default-style          '((java-mode . "java") (other . "bsd"))
  show-trailing-whitespace nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode       1)
(setq-all
  show-paren-delay     0
  blink-matching-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-all
  scroll-margin 5
;;  scroll-conservatively 100000
;;  scroll-preserve-screen-position 0
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-all
  backup-inhibited  t
  auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro when-disable (p) `(if (fboundp ',p) (,p -1)))

(when-disable scroll-bar-mode)
(when-disable tool-bar-mode)
(when-disable menu-bar-mode)

(setq-all inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 90)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(try-run 'highlight-80+ (global-highlight-80+-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(try-run 'linum (global-linum-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/slime")
(try-run 'slime (slime-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(try-run 'clojure-auto
  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/doc/git-core/contrib/emacs")
(try-run 'vc-git

  (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

  (try-run 'git

    (autoload 'git-blame-mode "git-blame"
      "Minor mode for incremental blame for Git." t)))

(setq-all
  vc-follow-sym       t
  vc-suppress-confirm t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(try-require 'php-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(try-run 'espresso
;  (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;  (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(try-run 'ido
  (ido-mode t)
  (setq ido-enable-flex-matching t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Maximise - doesn't currently use up all of its space when maximised

;(interactive)
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Attempts to get emacs to automatically open in server

;(server-start)

;(add-hook 'after-init-hook 'server-start)
;(add-hook 'server-done-hook
;  (lambda ()
;    (shell-command
;      "screen -r -X select `cat ~/tmp/emacsclient-caller`")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ispell)
(setq ispell-program-name "aspell")
;(setq ispell-dictionary   "british")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-p4open ()
  (interactive "")
  (let ((o (shell-command-to-string (concat "cd \"`dirname '" buffer-file-name "'`\" && p4 edit \"`basename '" buffer-file-name "'`\""))))
    (revert-buffer t t)
    (message o)))

(global-set-key (kbd "C-c C-x C-f") 'my-p4open)

(defun my-p4revert ()
  (interactive "")
  (let ((o (shell-command-to-string (concat "cd \"`dirname '" buffer-file-name "'`\" && p4 revert \"`basename '" buffer-file-name "'`\""))))
    (revert-buffer t t)
    (message o)))

(global-set-key (kbd "C-c C-x C-k") 'my-p4revert)

(defun my-diff-contents-against (f)
  (let ((bs (buffer-string)) (tmp (make-temp-file "diff")))
    (find-file tmp)
    (insert bs)
    (save-buffer)
    (kill-buffer (current-buffer))
    (let ((o (shell-command-to-string (concat "diff " f " " tmp))))
      (shell-command-to-string (concat "rm -f " tmp))
      (display-message-or-buffer o))))

(defun my-p4diff ()
  (interactive "")
  (let* ((tmp1 (make-temp-file "p4diff"))
         (o1 (shell-command-to-string (concat "cd \"`dirname '" buffer-file-name "'`\" && p4 print -o " tmp1 " -q \"`basename '" buffer-file-name "'`\""))))
    (message o1)
    (my-diff-contents-against tmp1)
    (shell-command-to-string (concat "rm -f " tmp1))))

(global-set-key (kbd "C-c C-d") 'my-p4diff)

(defun my-p4change ()
  (interactive "")
  (let* ((tmp1 (make-temp-file "p4change"))
          (name buffer-file-name)
          (o1 (shell-command-to-string (concat "cd \"`dirname '" buffer-file-name "'`\" && p4 change -o"))))
    (find-file tmp1)
    (insert (concat name "\n"))
    (insert o1)))

(global-set-key (kbd "C-c C-x o") 'my-p4change)

;; Quite unsafe ATM...
(defun my-p4submit ()
  (interactive "")

  ;; we need to get hold of the real dirname..... buffer-file-name is a tempfile

  (let* ((f buffer-filename)
         (o1 (shell-command-to-string (concat "cd \"`dirname '" buffer-file-name "'`\" && p4 change -i < " f))))
    ;; o1="Change 39006 created with 1 open file(s) fixing 1 job(s)."
    (shell-command-to-string (concat "rm -f " f))
    (message (shell-command-to-string (concat "cd \"`dirname '" buffer-file-name "'`\" && p4 submit -c ")))))

(global-set-key (kbd "C-c C-x o") 'my-p4change)

(defun my-diff ()
  (interactive "")
  (my-diff-contents-against buffer-file-name))

(global-set-key (kbd "C-c d") 'my-diff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

(setq w3m-use-cookies t)

(setq w3m-quickstart nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message ".emacs loaded in %ds"
  (destructuring-bind (hi lo ms) (current-time)
    (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-allow-keywords-as-property-names nil)
 '(js2-auto-insert-catch-block nil)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-include-gears-externs nil)
 '(js2-include-rhino-externs nil)
 '(js2-mirror-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'camelCase)
(add-hook 'find-file-hook 'camelCase-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell-do (&rest x)
  (shell-command-to-string (mapconcat 'identity x "")))

(defun shell-msg (&rest x)
  (display-message-or-buffer (apply 'shell-do x)))

(defun string-search-and-replace (search replace string)
  "Replace all instances of SEARCH with REPLACE in STRING."
  (replace-regexp-in-string (regexp-quote search) replace string t t))

(defun mypwd ()
  (let ((p (shell-do "pwd")))
    (substring p 0 (- (length p) 1))))

(defmacro with-cd (x &rest code)
  (let ((pd (mypwd)))
    `(progn
       (cd ,x)
       ,@code
       (cd ,pd))))

(defun get-apps-path ()
  (let* ((o1 (shell-do "dirname '" buffer-file-name "'"))
         (o2 (split-string o1 "/preset/")))
    (if (> (length o2) 1)
      (concat (car o2) "/preset")
      nil)))

(defun get-apps-path-part ()
  (let* ((o1 buffer-file-name)
         (o2 (split-string o1 "/preset/")))
    (if (> (length o2) 1)
      (cadr o2)
      nil)))

(defun my-apps-tags ()
  (interactive "")

  (let ((o3 (get-apps-path)))
    (if o3
      (with-cd o3
        (shell-msg "find apps/ library/js -name '*.js' | xargs etags")
        (if (not (member o3 tags-table-list))
          (setq tags-table-list (cons o3 tags-table-list))))
      (message "not in taggable dir"))))

(global-set-key (kbd "C-c e") 'my-apps-tags)

(defun my-debg-print ()
  (interactive "")
  (insert (concat "window.debug.log(\"" (read-from-minibuffer "Debug: ") "\");\n")))

(global-set-key (kbd "C-c w") 'my-debg-print)

(setq my-random-base (random 1000))
(setq my-random-id 0)

(defun my-debg-rand ()
  (interactive "")
  (insert (concat "window.debug.log(\"POINT " (number-to-string my-random-base) " " (number-to-string my-random-id) "\");\n"))
  (setq my-random-id (+ my-random-id 1)))

(global-set-key (kbd "C-c W") 'my-debg-rand)


(defun my-reload-emacs ()
  (interactive "")
  (load-file "~/.emacs"))

(global-set-key (kbd "C-c r") 'my-reload-emacs)

(defun my-jslint ()
  (interactive "")
  (let ((o1 (get-apps-path))
         (o2 (get-apps-path-part)))
    (if o1
      (let ((path (concat "build/jslint/" (string-search-and-replace ".js" ".txt" o2))))
        (with-cd (concat o1 "/../../builds/generic")
          (shell-do "rm -f " path)
          (message "Running JSLint")
          (compile (concat "PRETTY='' make " path))))
      (message "not in apps dir"))))

(global-set-key (kbd "C-c j") 'my-jslint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

(setq ac-auto-start t)
(setq ac-delay 0.1)
(setq ac-show-menu-immediately-on-auto-complete t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'p4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(my-apps-tags)


