(defface ido-mini-unsaved-buffer
  '((t (:foreground "orange")))
  "Face used by `ido-mini' to indicate buffers which have been edited but not saved."
  :group 'ido-mini)

(defface ido-mini-buffer-changed
  '((t (:foreground "red" :background "black")))
  "Face used by `ido-mini' to indicate buffers whose file has been edited outside Emacs."
  :group 'ido-mini)

(defface ido-mini-buffer-file-missing
  '((t (:foreground "Indianred2")))
  "Face used by `ido-mini' to indicate buffers whose file does not exist on disk."
  :group 'ido-mini)

(provide 'ido-mini-faces)

;; Local Variables:
;; nameless-current-name: "ido-mini"
;; End:
