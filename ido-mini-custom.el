(defgroup ido-mini nil
  "Variables for the `ido-mini' buffer switching command.")

(defcustom ido-mini-use-paths nil
  "If non-nil, display file paths of the associated files of buffers, where applicable (see function `ido-mini-add-paths').

Additionally, completion will search for buffer names as well as
their file paths.

Users may also find it useful to set this to nil and to enable
buffer uniquifying via `toggle-uniquify-buffer-names'."
  :group 'ido-mini)

(defcustom ido-mini-buffer-list-functions
  '(ido-mini-buffer-names
    ido-mini-buffers-clean
    ido-mini-buffers-bury-visible
    ido-mini-buffers-color)
  "List of functions run sequentially over the output of `(buffer-list)', with the result of one being the input of the next (using `->list').

Each should accept exactly one argument. The resulting list is
used by `ido-mini' for completion candidates."
  :group 'ido-mini)

(defcustom ido-mini-recentf-list-functions
  '(ido-mini-recentf-bury-visited
    ido-mini-recentf-color)
  "List of functions run sequentially over `recentf-list', with the result of one being the input of the next (using `->list').

Each should accept exactly one argument. The resulting list is
used by `ido-mini' for completion candidates."
  :group 'ido-mini)

(provide 'ido-mini-custom)

;; Local Variables:
;; nameless-current-name: "ido-mini"
;; End:
