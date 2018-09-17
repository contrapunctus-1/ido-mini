(require 'recentf)
(require 'dash)
(require 'ido)

(require 'ido-mini-custom)
(require 'ido-mini-faces)

;; add variables to
;; - toggle displaying paths with buffer names

(defun ->list (var functions)
  "Assuming FUNCTIONS is a list of functions (FN1 FN2 FN3),
return the result of (FN3 (FN2 (FN1 VAR)))"
  ;; (--reduce-from (funcall it acc) var functions)
  (dolist (fn functions)
    (setq var (funcall fn var)))
  var)

(defun ido-mini-buffer-names (&optional buffer-list)
  "Return a list of the name of all buffers returned
by (buffer-list)."
  (let ((buffer-list (if buffer-list buffer-list (buffer-list))))
    (-map 'buffer-name buffer-list)))

(defun ido-mini-buffers-clean (buffer-names)
  "Remove buffers whose names start with a space."
  (cl-flet ((leading-space-p (el)
                             (string-match-p "^ " el)))
    (-remove #'leading-space-p buffer-names)))

(defun ido-mini-buffers-bury-visible (buffer-names)
  "Put visible buffers at the end of list, in reverse order of
  appearance (e.g. (vb1 b1 vb2 b2 ...) becomes (b1 b2 ... vb2
  vb1))"
  (let* ((bufs-wins-alist (-zip buffer-names
                                (-map 'get-buffer-window-list
                                      buffer-names)))
         (bufs-with-wins  (-filter 'cdr bufs-wins-alist))
         (bufs-wo-wins    (-remove 'cdr bufs-wins-alist)))
    (append
     (-map 'car bufs-wo-wins)
     (reverse (-map 'car bufs-with-wins)))))

(defun ido-mini-buffers-color (buffer-names)
  (-map
   (lambda (buffer-name)
     (let ((buffer (get-buffer buffer-name)))
       (cond ((let ((bfn (buffer-file-name buffer)))
                (and bfn
                     (not (file-exists-p bfn))))
              (propertize buffer-name 'face 'ido-mini-buffer-file-missing))
             ;; buffers with unsaved files
             ((and (buffer-file-name buffer)
                   (buffer-modified-p buffer))
              (propertize buffer-name 'face 'ido-mini-unsaved-buffer))
             ;; buffer modified outside emacs
             ((not (verify-visited-file-modtime buffer))
              (propertize buffer-name 'face 'ido-mini-buffer-changed))
             ;; buffers with files
             ((buffer-file-name buffer)
              (propertize buffer-name 'face 'font-lock-type-face))
             ;; dired buffers
             ((with-current-buffer buffer
                (derived-mode-p 'dired-mode))
              (propertize buffer-name 'face 'dired-directory))
             ;; TODO make this light gray
             ((string-match-p "^\\*" buffer-name)
              (propertize buffer-name 'face 'italic))
             (t buffer-name))))
   buffer-names))

(defun ido-mini-recentf-bury-visited (recentf)
  "Returns the contents of `recentf-list', with files being
visited by a buffer placed at the end of the list."
  (append
   (-remove #'get-file-buffer recentf)
   (-filter #'get-file-buffer recentf)))

(defun ido-mini-recentf-color (recentf)
  "Color recentf-list."
  (-map (lambda (el) (propertize el 'face 'ido-virtual))
        recentf))

(defun ido-mini-select-text (ido-choice)
  "Mimic superficial behaviour of `ido-select-text'. If there is
an exact match for the search string, select it, else create a
new buffer using the search string as the name.")

(defun ido-mini-exit-minibuffer (ido-choice)
  "Mimic superficial behaviour of `ido-exit-minibuffer'. If there
is any match for the search string, select it, else print
[Confirm] and after another RET, create a buffer using the search
string as the name.")

(defun ido-mini-save-search-string ()
  "Save search string from ido-completing-read into history.")

;; (ido-completing-read "Buffer:"
;;                      (append (->list (buffer-list)
;;                                      ido-mini-buffer-list-functions)
;;                              (->list recentf-list
;;                                      ido-mini-recentf-list-functions)))

;;;; IDO-MINI ----

(defun ido-mini ()
  "A helm-mini replacement using Ido. Switch to a buffer or a
recentf entry with ido. If `ido-mini-use-paths' is non-nil,
search for and display the whole file path instead of just the
file name.

Using ido-vertical in conjunction may be beneficial.

It can be extended conveniently - see the variables
`ido-mini-buffer-list-functions' and `ido-mini-recentf-list-functions'.

Based off code from wilfredh -
https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d"
  (interactive)
  (let*
      ((buffers-sorted         (->list (buffer-list)
                                       ido-mini-buffer-list-functions))
       (recentf-sorted         (ido-mini-recentf-bury-visited recentf-list))
       (buffer-count           (length buffers-sorted))

       (processed-buffers      (->list (buffer-list)
                                       ido-mini-buffer-list-functions))
       (processed-recentf      (->list recentf-list
                                       ido-mini-recentf-list-functions))
       (candidates             (append processed-buffers
                                       processed-recentf))
       (ido-choice             (completing-read "Switch to buffer: "
                                                candidates
                                                nil nil nil
                                                'ido-buffer-history))
       (chosen-index           (-elem-index ido-choice candidates)))
    ;; if chosen-index is nil, we create a buffer with that name
    (if chosen-index
        ;; is the chosen candidate in the buffer-list or recentf-list?
        (if (< chosen-index buffer-count)
            (switch-to-buffer (nth chosen-index buffers-sorted))
          (find-file (nth (- chosen-index buffer-count)
                          recentf-sorted)))
      (switch-to-buffer (get-buffer-create ido-choice)))))

(provide 'ido-mini)

;; Local Variables:
;; nameless-current-name: "ido-mini"
;; End:
