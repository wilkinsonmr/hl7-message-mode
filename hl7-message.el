;;; hl7-message.el --- View and navigate HL7 message files

;; Copyright (C) 2022 Mark R. Wilkinson

;; Author: Mark R. Wilkinson <wilkinsonmr@gmail.com>
;; Version: 1.0
;; Package-Requires: ((cl-lib) (tree-mode) (hierarchy "0.7.0"))
;; Keywords: HL7
;; URL: https://github.com/wilkinsonmr/hl7-message-mode


;;; Commentary:

;; This package provides a major mode to view and navigate HL7 messages.

;;; Code:

(defgroup hl7-message nil
  "Major mode for HL7 messages."
  :group 'languages
  :link '(url-link :tag "Website" "https://github.com/wilkinsonmr/hl7-message-mode"))

(defface hl7-message-segment-name-face
  '((t :inherit font-lock-type-face))
  "Segment name"
  :group 'hl7-message)

(defface hl7-message-field-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Field separator"
  :group 'hl7-message)

(defface hl7-message-component-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Component separator"
  :group 'hl7-message)

(defface hl7-message-subcomponent-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Subcomponent separator"
  :group 'hl7-message)

(defface hl7-message-field-repeat-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Field repeat separator"
  :group 'hl7-message)

(defface hl7-message-escape-character-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Escape character"
  :group 'hl7-message)

(defconst hl7-message-font-lock-defaults
  ;; TODO: these characters actually come from the message itself
  (list '("^\\(...\\)|" 1 'hl7-message-segment-name-face)
        '("|" 0 'hl7-message-field-separator-face)
        '("\\^" 0 'hl7-message-component-separator-face)
        '("&" 0 'hl7-message-subcomponent-separator-face)
        '("~" 0 'hl7-message-field-repeat-separator-face)
        '("\\(\\\\.\\\\\\)" 1 'hl7-message-escape-character-face)))

;;;###autoload
(define-derived-mode hl7-message-mode fundamental-mode "HL7"
  "Major mode for viewing HL7 messages."
  (setq font-lock-defaults '((hl7-message-font-lock-defaults) nil t)))

(provide 'hl7-message)

;;; hl7-message.el ends here
