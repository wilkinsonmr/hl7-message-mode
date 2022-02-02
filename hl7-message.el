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

(require 'cl-lib)
(require 'hierarchy)
(require 'tree-mode)

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

(defcustom hl7-segment-definition-file nil
  "Path to a file containing tab-separated values for the following fields:

ELEMENT_ID DESCRIPTION LENGTH DATA_TYPE OPTIONALITY REPEATABILITY TABLE

Where

ELEMENT_ID is like ORC.7.10.1"
  :type '(file)
  :group 'hl7-message)

(defvar hl7-segment-defs nil
  "Alist of segment definitions.")

(defun hl7-load-segment-defs ()
  "Load the TSV of segment descriptions into `hl7-segment-defs'."
  (if hl7-segment-definition-file
      (setq hl7-segment-defs
            (with-temp-buffer
              (insert-file-contents hl7-segment-definition-file)
              (goto-char (point-min))
              (let (desc fields id seg-id seg-desc rtn)
                (while (not (eobp))
                  (setq desc (buffer-substring-no-properties
                              (point) (progn (forward-line 1) (point)))
                        desc (string-trim-right desc "\n")
                        fields (split-string desc "\t")
                        id (split-string (car fields) " - " nil " ")
                        seg-id (car id)
                        seg-desc (list (string-join (cdr id) " - ")))
                  (push (cons seg-id (append seg-desc (cdr fields))) rtn))
                rtn)))
    (message "No segment definition file found.")))

(defun hl7-lookup-segment-def (seg-id segment-defs)
  "Return the alist associated with SEG-ID from SEGMENT-DEFS."
  (assoc seg-id segment-defs 'string=))

(defun hl7-segment-to-nested-list (segment)
  "Convert an HL7 SEGMENT (line) to a nested list."
  (let* ((fields (split-string segment "|")) ; TODO: get this from MSH.1
         (seg-name (car fields))
         (field-n -1)
         field-id field-nest msh.2 )
    ;; Handle MSH specially
    (when (string= seg-name "MSH")
      (setq fields (cons (car fields) (cons "|" (cdr fields))))
      (setq msh.2 (nth 2 fields))
      ;; TODO: replace all of the encoding characters with unsplittable text
      (setf (nth 2 fields)
            (replace-regexp-in-string "\\^" "CARET" (nth 2 fields)))
      (setf (nth 2 fields)
            (replace-regexp-in-string "&" "AMPERSAND" (nth 2 fields))))
    (setq field-nest
          ;; TODO: factor this out into three separate functions
          (mapcar
           #'(lambda (field)
               (setq field-n (1+ field-n)
                     field-id (format "%s.%d" seg-name field-n))
               (if (string-match-p "\\^" field) ; Don't expand if not multi-component
                   (let* ((comps (split-string field "\\^"))
                          (comp-n 0)
                          (comp-id nil)
                          (comp-nest
                           (mapcar
                            #'(lambda (comp)
                                (setq comp-n (1+ comp-n)
                                      comp-id (format "%s.%d.%d"
                                                      seg-name
                                                      field-n
                                                      comp-n))
                                ;; (defun subcomponents comp comp-n comp-id sub-sep
                                ;; return this:
                                (if (string-match-p "&" comp)
                                    (let* ((subcomps (split-string comp "&"))
                                           (subcomp-n 0)
                                           (subcomp-id nil)
                                           (subcomp-nest
                                            (mapcar
                                             #'(lambda (subcomp)
                                                 (setq subcomp-n (1+ subcomp-n)
                                                       subcomp-id (format "%s.%d.%d.%d"
                                                                          seg-name
                                                                          field-n
                                                                          comp-n
                                                                          subcomp-n))
                                                 (append (list subcomp-id) (list (list subcomp))))
                                             subcomps)))
                                      (append (list comp-id) subcomp-nest))
                                  (append (list comp-id) (list (list comp)))))
                            comps)))
                     (append (list field-id) comp-nest))
                 (append (list field-id) (list (list field)))
                 ))
           fields))
    (when msh.2
      ;; Restore original msh.2 encoding characters
      (setf (cdr (nth 1 (cdr field-nest))) (list (list msh.2))))
    (append (list seg-name) (cdr field-nest))))

(defun hl7-buffer-to-nested-list (&optional buffer)
  "Convert an HL7 message in the current buffer or BUFFER to a nested list."
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'hl7-message-mode)
      (error "Not in an HL7 buffer"))
    (unless (string= "MSH" (buffer-substring-no-properties 1 4))
      (error "MSH is not the first segment"))
    (let ((fld-sep (string (or (char-after 4)
                               (error "Missing field separator"))))
          (cmp-sep (string (or (char-after 5)
                               (error "Missing component separator"))))
          (rep-sep (string (or (char-after 6)
                               (error "Missing repetition separator"))))
          (esc-chr (string (or (char-after 7)
                               (error "Missing escape character"))))
          (sub-sep (string (or (char-after 8)
                               (error "Missing subcomponent separator"))))
          (root (list "Message"))
          (inhibit-field-text-motion t)
          segment segment-nest)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq segment (buffer-substring-no-properties
                         ;; TODO: just end-of-line?
                         (point) (progn (end-of-line 1) (point)))
                segment-nest (hl7-segment-to-nested-list segment)
                root (append root (list segment-nest)))
          (forward-line 1)))
      root)))

(defun hl7-hier-item-is-leaf-p (item indent)
  "Return non-nil if ITEM is a leaf.  INDENT is ignored."
  (not (and (listp item) (= 1 (length (cdr item))))))

(defun hl7-hier-labelfn-button-label (item indent)
  "Insert the properly indented HL7 segment/field ID ITEM.  INDENT is ignored."
  (unless (bound-and-true-p hl7-segment-defs) ; TODO: get from MSH.12?
    (hl7-load-segment-defs))
  (let* ((seg-id (cadr item))
         (seg-def (hl7-lookup-segment-def seg-id hl7-segment-defs)))
    ;; FIXME: only lookup if seg id, not values
    ;; TODO: if "", display as (null)
    (if (string= seg-id "")
        (insert "(null)")
      (insert seg-id))
    (when-let ((name (nth 1 seg-def)))
      ;; Name
      (insert " - " name))
    (when-let ((opt (nth 4 seg-def)))
      ;; Optionality
      (insert (pcase opt
                ("R" " (R)")
                ("O" "")
                ("C" " (C)")
                ("B" " (B)")
                ("X" "")
                (_ (format " (%s)" opt))) ))))

;; TODO: Add hyperlink info via text properties rather parsing segment ID
;; If URL is provided by the checker, and cannot be composed
;; from other elements in the `flycheck-error' object, consider
;; passing the URL via text properties:
;;
;; ;; During the error object creation
;; (put-text-property 0 1 'explainer-url .url .check_id)
;;
;; ;; In the error-explainer FUNCTION
;; (let ((id (flycheck-error-id err)))
;;   (and id `(url . ,(get-text-property 0 'explainer-url id))))

(defun hl7-hier-labelfn-button-action (item indent)
  "Define the action for a hierarchy button (link) for ITEM.  INDENT is ignored."
  (let* ((ref (cadr item))
         (url (format
               "https://hl7-definition.caristix.com/v2/HL7v2.5.1/%s/%s"
               (pcase (cl-count ?. ref)
                 (0 "Segments")
                 (1 "Fields")
                 (2 "Fields")
                 (3 "Fields"))
               ref)))
    (browse-url url)))

(defun hl7-prune-null-from-nested-list (nest)
  "Remove nodes with null (empty string) value from a nested list NEST."
  ;; Remove null post hoc so fields are numbered correctly
  (mapcar #'(lambda (x)
              (if (listp x)
                  (remove nil
                          (if (equal (cdr x) (list (list "")))
                              nil
                            (hl7-prune-null-from-nested-list x)))
                x))
          nest))

;;;###autoload
(defun hl7-display-buffer-as-tree (&optional rm-null buffer)
  "Display an HL7 message BUFFER as an interactive tree.

Non-nil RM-NULL omits null entries."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (tree-buf (get-buffer-create (concat (buffer-name buf) "<tree>")))
         (msg-nest (hl7-buffer-to-nested-list buf))
         (msg-nest (if rm-null
                       (hl7-prune-null-from-nested-list msg-nest)
                     msg-nest))
         (msg-hier (hierarchy-from-list msg-nest 'allow-dups)))
    (switch-to-buffer
     (hierarchy-tree-display
      msg-hier
      (hierarchy-labelfn-indent
       (hierarchy-labelfn-button-if
        'hl7-hier-labelfn-button-label
        'hl7-hier-item-is-leaf-p
        'hl7-hier-labelfn-button-action)
       ;; No additional indentation
       "")
      tree-buf))
    (tree-minor-mode 1)
    (tree-mode-expand-level 1)))

;;;###autoload
(defun hl7-display-buffer-as-tree-without-nulls (&optional buffer)
  "Same as `hl7-display-buffer-as-tree' for BUFFER but omits null entries."
  (interactive)
  (hl7-display-buffer-as-tree t buffer))

;;;###autoload
(defun hl7-display-buffer-as-text (&optional rm-null buffer)
  "Display an HL7 message BUFFER as indented text.

Non-nil RM-NULL omits null entries."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (text-buf (get-buffer-create (concat (buffer-name buf) "<text>")))
         (msg-nest (hl7-buffer-to-nested-list buf))
         (msg-nest (if rm-null
                       (hl7-prune-null-from-nested-list msg-nest)
                     msg-nest))
         (msg-hier (hierarchy-from-list msg-nest 'allow-dups)))
    (switch-to-buffer
     (hierarchy-tabulated-display
      msg-hier
      (hierarchy-labelfn-indent
       (hierarchy-labelfn-button-if
        'hl7-hier-labelfn-button-label
        'hl7-hier-item-is-leaf-p
        'hl7-hier-labelfn-button-action))
      text-buf))))

;;;###autoload
(defun hl7-display-buffer-as-text-without-nulls (&optional buffer)
  "Same as `hl7-display-buffer-as-text' for BUFFER but omits null entries."
  (interactive)
  (hl7-display-buffer-as-text t buffer))

(defun hl7-message-mode-eldoc ()
  "Eldoc function for hl7 message mode."
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (pt (point))
         seg-name field-beg field-end field-nbr comp-beg comp-end comp-nbr
         n-comps subc-nbr n-subcs n-reps rep-nbr)
    (setq seg-name (buffer-substring-no-properties beg (min (+ beg 3) (point-max))))
    (when (= 3 (length seg-name))
      ; FIXME: handle MSH segments
      (setq field-beg (save-excursion (or (search-backward "|" beg t) beg))
            field-end (save-excursion (or (search-forward "|" end t) end))
            field-nbr (how-many "|" beg pt)
            rep-beg (save-excursion (or (search-backward "~" field-beg t) field-beg))
            rep-end (save-excursion (or (search-forward "~" field-end t) field-end))
            n-reps (how-many "~" field-beg field-end)
            rep-nbr (if (> n-reps 0)
                        (concat "/" (number-to-string (1+ (how-many "~" field-beg pt))))
                      "")
            comp-beg (save-excursion (or (search-backward "^" rep-beg t) rep-beg))
            comp-end (save-excursion (or (search-forward "^" rep-end t) rep-end))
            comp-nbr (1+ (how-many "\\^" rep-beg pt))
            n-comps (how-many "\\^" comp-beg comp-end)
            subc-nbr (1+ (how-many "&" comp-beg pt))
            n-subcs (how-many "&" comp-beg comp-end))
      (cond
       ((zerop field-nbr)
        seg-name)
       ((zerop n-comps)
        (format "%s.%s%s" seg-name field-nbr rep-nbr))
       ((zerop n-subcs)
        (format "%s.%s.%s%s" seg-name field-nbr comp-nbr rep-nbr))
       (t
        (format "%s.%s.%s.%s%s" seg-name field-nbr comp-nbr subc-nbr rep-nbr))))))

;;;###autoload
(define-derived-mode hl7-message-mode fundamental-mode "HL7"
  "Major mode for viewing HL7 messages."
  (setq font-lock-defaults '((hl7-message-font-lock-defaults) nil t))
  (add-function :before-until (local 'eldoc-documentation-function)
                #'hl7-message-mode-eldoc))

(provide 'hl7-message)

;;; hl7-message.el ends here
