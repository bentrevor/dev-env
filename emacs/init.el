
(progn
  (require 'package)
  (package-initialize)

  (load "~/.emacs.d/mine/settings.el")
  (load "~/.emacs.d/mine/keybindings.el")
  (load "~/.emacs.d/mine/org-mode-options.el")

  (magit-status)
  (delete-other-windows)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "8b313e1793da427e90c034dbe74f3ad9092ac291846c0f855908c42a6bda1ff4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "6065eaee1ef5aa6bbfc594b60fed3dba9e22cff9fb37559861babda07d50a634" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "5a21604c4b1f2df98e67cda2347b8f42dc9ce471a48164fcb8d3d52c3a0d10be" "55ed02951e7b458e4cd18837eefa1956884c9afd22bb514f697fd1d2d1abb3d3" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "d25d9b2b1e800a74fea4f6d174c4bd1b9c19a7617b22cc349245a36417c56ece" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "c6faf8734bd04d303d9a272daef7a1c37a85597e2de3c006a4319ecf579821a0" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" default)))
 '(erc-hl-nicks-mode t)
 '(erc-hl-nicks-skip-nicks (quote ("so" "So")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
