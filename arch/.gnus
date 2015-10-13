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
;;  None.
;;--------------------------------------------------------------------------

;;; Code:

;; Configurations related to specific mail sources

(setq indiana-address "dalmahal@indiana.edu"
      indiana-name "Deyaaeldeen Almahallawi"
      gmail-address "diaa6510@gmail.com"
      gmail-name "Deyaa (ضياء)"
      user-mail-address indiana-address
      user-full-name indiana-name
      gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
	       (gnus-thread-sort-functions
	       	'(not gnus-thread-sort-by-number)))
      gnus-secondary-select-methods
      '((nnimap "indiana"
                (nnimap-address "imap.exchange.iu.edu")
		(nnmail-expiry-target "nnimap+indiana:Deleted Items")
		(gnus-thread-sort-functions
		'(not gnus-thread-sort-by-number))))
      
      gnus-posting-styles
      '(("^INBOX"
	 (name gmail-name)
	 (address gmail-address)
	 ("X-Message-SMTP-Method" "smtp smtp.gmail.com 465"))
	("nnimap.indiana*"
	 (address indiana-address)
	 (name indiana-name)
	 (organization "Indiana University")
	 (gcc "\"nnimap+indiana:Sent Items\"")
	 ("X-Message-SMTP-Method" "smtp mail-relay.iu.edu 465")
	 (signature-file "~/.signature2")))
      gnus-message-archive-group
      '(("nnimap.indiana*" "nnimap+indiana:Sent Items"))
      ;; generated by regexp-opt
      message-alternative-emails
      "\\(?:d\\(?:almahal@\\(?:\\(?:indiana\\|umail\\.iu\\)\\.edu\\)\\|iaa6510@gmail\\.com\\)\\)"
      gnus-group-jump-to-group-prompt "nnimap+indiana:INBOX")

;; -------------------------------------------------------------------

(gnus-registry-initialize)

(setq mail-user-agent 'gnus-user-agent
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t
      gnus-auto-select-first 'unseen
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
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
      gnus-large-newsgroup 500 ;; clean up your inboxes first :P
      message-forward-as-mime t
      nnmail-expiry-wait 'immediate
      mm-text-html-renderer 'w3m
      gnus-permanently-visible-groups "\\(?:INBOX\\|Sent\\)"
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
      message-default-mail-headers "Cc: \nBcc: \nGcc: \n"
      gnus-visible-headers
      (concat "^From:\\|^Subject:\\|^Date:\\|^Reply-To:\\|^Organization:\\|"
	      "^To:\\|^[BGF]?Cc:\\|^X-Sent:\\|^X-Mailer\\|^User-Agent")
      gnus-summary-highlight '(((or
				 (eq mark gnus-dormant-mark)
				 (eq mark gnus-ticked-mark))
				. gnus-summary-normal-ticked)
			       ((> score default-high)
				. gnus-summary-high-unread)
			       ((< score default-low)
				. gnus-summary-low-read)
			       (t . gnus-summary-normal-unread))
      gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
      gnus-summary-mode-line-format "Gnus: %g [%z] %Z" ;; group score unread
      gnus-user-date-format-alist '((t . "%Y-%d-%m %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent ""
      gnus-sum-thread-tree-leaf-with-other "-> "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "|_ "
      gnus-sum-thread-tree-vertical "|"
      gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject
      ; @see http://stackoverflow.com/questions/945419/how-dont-use-gnus-adaptive-scoring-in-some-newsgroups
      gnus-parameters
      '(("nnimap.*"
         (gnus-use-scoring nil))
        ))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;(add-hook 'mail-citation-hook 'sc-cite-original) ;;supercite
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'message-mode-hook
          '(lambda ()
             (bbdb-initialize 'gnus 'message)
             (bbdb-initialize 'gnus)
             (local-set-key "<TAB>" 'bbdb-complete-name)
	     (setq fill-column 72)
	     (turn-on-auto-fill)
	     (flyspell-mode)
	     (flycheck-mode)))

(defvar gnus-default-adaptive-score-alist
  '((gnus-kill-file-mark (from -10))
    (gnus-unread-mark)
    (gnus-read-mark (from 10) (subject 30))
    (gnus-catchup-mark (subject -10))
    (gnus-killed-mark (from -1) (subject -30))
    (gnus-del-mark (from -2) (subject -15))
    (gnus-ticked-mark (from 10))
    (gnus-dormant-mark (from 5))))

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
   bbdb-ignore-some-messages-alist
   '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
