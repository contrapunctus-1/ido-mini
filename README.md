# ido-mini

A [helm-mini](https://github.com/emacs-helm/helm) in ido

Ido's -
- smarter sorting
- snappier speed

meets helm-mini's -
- informatively-colored candidates list
- convenience of a single command to switch to buffers, open recentf
  files, and create new buffers.

Plus, some improvements of its own -
1. If a recentf entry is already visited by a buffer, put the entry at
   the end of the (recentf?) candidates list.
2. (planned) tweak `ido-mini-use-paths` to search and display not just
   buffer names but also their file paths, wherever applicable.
3. (planned) show documentation of functions from smex, akin to C-j in
   helm-M-x

## Installation
`ido-mini` requires [dash.el](https://github.com/magnars/dash.el) and `ido`.

`ido-vertical-mode` can be used to view candidates vertically.

`ido-mini` assumes you use Emacs' built-in `recentf-mode`. If not, simply add `(recentf-mode)` to your init.

## Customization
### Keys
**IMPORTANT**

In Ido, key customization is sadly NOT done the usual way. Instead,
you write a function to change keybindings (you want to use
`ido-completion-map`) and add that to `ido-setup-hook'. See section
'Customization' in (find-library "ido")

For fuzzy matching, install the flex-ido package. It's not always useful, you can define a command to toggle it -
```
(defun my/ido-toggle-flex ()
  "Toggle the value of `ido-enable-flex-matching'."
  (interactive)
  (setq flx-ido-mode (not flx-ido-mode)))
```
and bind it to a key.

### ido-mini behaviour
ido-mini tries to let the user modify core behaviour without
redefining entire functions. This is done by providing variables which
hold lists of functions (like typical Emacs hooks).

The functions are run in sequence, and each accepting the output of
the previous. Changing the functions in the list or even the order of
the functions will change behaviour.

Currently, there are two such variables -
`ido-mini-buffer-list-functions` and
`ido-mini-recentf-list-functions`. ido-mini uses the output of these
functions to derive the final list of candidates.

## Known issues
flx-ido and recent versions of ido override ido-mini's candidate coloring. As of now it's a choice between flx-ido/ido's matched string highlighting, and ido-mini's informatively colored candidates. If you'd rather have the latter, add `(setq flx-ido-use-faces nil)` and `(setq ido-use-faces nil)` to your init respectively.

## TODO
1. When called twice in succession, quit ido-mini
2. color the matched substring in the candidates?
3. store only search terms in input history, not the selected buffer
   names/file paths
4. Mimic exact C-j (`ido-select-text`) and RET behaviour
   (`ido-exit-minibuffer`)
5. What if we search for files (perhaps only in user-specified
   directories, when provided) when there are no matches in the buffer
   list as well as in recentf?
6. Add animated
7. Add indicator if flx-ido-mode is enabled
8. Add tests
9. Don't fail if recentf-list is nil
10. Create user-redefinable function to return final list of
    candidates.

## Contributions and contact
Feedback and MRs very welcome. 🙂

Contact the creator and other Emacs users in the Emacs room on the Jabber network - [xmpp:emacs@salas.suchat.org?join](xmpp:emacs@salas.suchat.org?join) ([web chat](https://inverse.chat/#converse/room?jid=emacs@salas.suchat.org))

(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))

## License
ido-mini is released under your choice of [CC0](https://creativecommons.org/publicdomain/zero/1.0/), [Unlicense](https://unlicense.org/), and [WTFPL](http://www.wtfpl.net/).

## Thanks
wilfredh for the initial code that got me started -
https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d

wasamasa, bpalmer and #emacs in general for all their help and support

fiete for testing
