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

## Key customization
**IMPORTANT**

In Ido, key customization is sadly NOT done the usual way. Instead,
you write a function to change keybindings (you want to use
`ido-completion-map`) and add that to `ido-setup-hook'. See section
'Customization' in (find-library "ido")

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

## Contact
Contact the creator and other Emacs users in the Emacs room on the Jabber network - [xmpp:emacs@salas.suchat.org?join](xmpp:emacs@salas.suchat.org?join)
(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))

## License
timeclock-ui is released under your choice of [CC0](https://creativecommons.org/publicdomain/zero/1.0/), [Unlicense](https://unlicense.org/), and [WTFPL](http://www.wtfpl.net/).

## Thanks
wasamasa, bpalmer and #emacs in general for all their help and support

wilfredh for the initial code that got me started -
https://gist.github.com/Wilfred/31e8e0b24e3820c24850920444dd941d
