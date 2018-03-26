(defun reset-erc-track-mode ()
  "Clears out annoying erc-track-mode stuff for when we don't care."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(setq erc-interpret-controls-p 'remove ;; remove highlight/bold font effects
	  erc-track-showcoun t
	  erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE") ;; don't track these changes
	  )

