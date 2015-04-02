;;; .gnus -- Gnus configuration

;; Author: Deyaaeldeen Almahallawi

;;; Commentary:
;;
;;  This setup needs the following software to be installed in your
;;  system:
;;  o gnupg: for digital signature and encryption
;;  o hashcash: for whitelisting
;;
;;  To install, put this file in your home directory.
;;--------------------------------------------------------------------------
;;
;;  TODO:
;;
;;  Clear the mess of scoring stuff.
;;--------------------------------------------------------------------------

;;; Code:

(setq indiana-address "dalmahal@indiana.edu"
      indiana-name "Deyaaeldeen Almahallawi"
      gmail-address "diaa6510@gmail.com"
      gmail-name "Deyaa (ضياء)"
      user-mail-address indiana-address
      user-full-name indiana-name
      indiana-pk "Deyaaeldeen Almahallawi <dalmahal@indiana.edu>")

(setq gnus-select-method
      '(nnimap "gmail"
           (nnimap-address "imap.gmail.com")
	   (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")))

(setq gnus-secondary-select-methods
      '(;(nnml "iu")
        (nnimap "indiana"
                (nnimap-address "imap.exchange.iu.edu")
		(nnmail-expiry-target "nnimap+indiana:Deleted Items"))))

(setq gnus-posting-styles
      '(("^INBOX"
	 (name gmail-name)
	 (address gmail-address)
	 ("X-Message-SMTP-Method" "smtp smtp.gmail.com 465"))
	("nnimap.indiana*"
	 (address indiana-address)
	 (name indiana-name)
	 (organization "Indiana University")
	 (gcc "\"nnimap+indiana:Sent Items\"")
	 ("X-Message-SMTP-Method" "smtp mail-relay.iu.edu 465"))))

(setq message-alternative-emails
      (regexp-opt (list gmail-address indiana-address "dalmahal@umail.iu.edu")))

(setq gnus-message-archive-group
           '(("nnimap.indiana*" "nnimap+indiana:Sent Items")))

(gnus-registry-initialize)

(setq mail-user-agent 'gnus-user-agent
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t
      gnus-auto-select-first 'unseen
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      gnus-thread-ignore-subject t
      gnus-thread-hide-subtree t
      gnus-use-cache t
      gnus-check-new-newgroups nil
      gnus-save-newsrc-file nil
      gnus-use-adaptive-scoring t
      gnus-save-score t
      auth-source-save-behavior nil
      gnus-novice-user nil
      gnus-expert-user t
      message-kill-buffer-on-exit t
      gnus-large-newsgroup t
      message-forward-as-mime t
      nnmail-expiry-wait 'immediate
      mm-text-html-renderer 'w3m
      gnus-permanently-visible-groups "INBOX"
      gnus-treat-hide-boring-headers 'head
      gnus-boring-article-headers (list 'long-to)
      gnus-gcc-mark-as-read t
      gnus-break-pages nil
      message-generate-hashcash t
      gnus-asynchronous t
      gnus-visual t
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-strip-leading-blank-lines t
      gnus-treat-strip-multiple-blank-lines t
      gnus-treat-strip-cr t
      mm-discouraged-alternatives '("text/html" "text/richtext")
      message-default-mail-headers "Cc: \nBcc: \nGcc: \n")

(setq gnus-visible-headers
      (concat "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|"
	      "^Followup-To:\\|^Reply-To:\\|^Organization:\\|"
	      "^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|"
	      "^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|"
	      "^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|"
	      "^X-Sent:\\|^X-Mailer\\|^User-Agent\\|^X-Interest-Karra"))

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent ""
 gnus-sum-thread-tree-leaf-with-other "-> "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "|_ "
 gnus-sum-thread-tree-vertical "|")

(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
; @see http://stackoverflow.com/questions/945419/how-dont-use-gnus-adaptive-scoring-in-some-newsgroups
(setq gnus-parameters
      '(("nnimap.*"
         (gnus-use-scoring nil))
        ))

(defvar gnus-default-adaptive-score-alist
  '((gnus-kill-file-mark (from -10))
    (gnus-unread-mark)
    (gnus-read-mark (from 10) (subject 30))
    (gnus-catchup-mark (subject -10))
    (gnus-killed-mark (from -1) (subject -30))
    (gnus-del-mark (from -2) (subject -15))
    (gnus-ticked-mark (from 10))
    (gnus-dormant-mark (from 5))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

(add-hook 'message-mode-hook
          '(lambda ()
             (bbdb-initialize 'message)
             (bbdb-initialize 'gnus)
             (local-set-key "<TAB>" 'bbdb-complete-name)
	     (setq fill-column 72)
	     (turn-on-auto-fill)
	     (flyspell-mode)
	     (flycheck-mode)))

;;pgp

(require 'epa)
(setq mml2015-use 'epg
      mml2015-verbose t
      epg-user-id indiana-pk
      mml2015-encrypt-to-self t
      mml2015-always-trust nil
      mml2015-cache-passphrase t
      mml2015-passphrase-cache-expiry '36000
      mml2015-sign-with-sender t
      gnus-message-replyencrypt t
      gnus-message-replysign t
      gnus-message-replysignencrypted t
      gnus-treat-x-pgp-sig t
      ;;       mm-sign-option 'guided
      ;;       mm-encrypt-option 'guided
      mm-verify-option 'always
      mm-decrypt-option 'always
      epg-debug t ;;  then read the *epg-debug*" buffer
      gnus-buttonized-mime-types
      '("multipart/alternative" "multipart/encrypted" "multipart/signed"
	".*/signed"))

(bbdb-initialize)

(setq
 bbdb-use-pop-up nil
 bbdb-offer-save 1                        ;; 1 means save-without-asking
 bbdb-always-add-address t                ;; add new addresses to existing...
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
 bbdb-completion-type nil                 ;; complete on anything
 bbdb-complete-name-allow-cycling t       ;; cycle through matches
 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA
 ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

; mail search
(require 'nnir)
