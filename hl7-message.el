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

;;;###autoload
(defun hl7-forward-component ()
  "Move point to the next HL7 component."
  (interactive)
  (or (re-search-forward "[|^&]" (line-end-position) t)
      (forward-line 1)))

;;;###autoload
(defun hl7-backward-component ()
  "Move point to the previous HL7 component."
  (interactive)
  (cond
   ((bolp)
    (backward-char 1))
   (t
    (or (re-search-backward "[|^&]" (line-beginning-position) t)
        (beginning-of-line)))))

;;;###autoload
(defun hl7-load-segment-defs (&optional file)
  "Load the TSV of segment descriptions into `hl7-segment-defs'."
  (interactive)
  (if-let ((file (or file hl7-segment-definition-file)))
      (setq hl7-segment-defs
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (let (desc fields rtn)
                (while (not (eobp))
                  (setq desc (buffer-substring-no-properties
                              (point) (progn (forward-line 1) (point)))
                        desc (string-trim-right desc "\n")
                        fields (split-string desc "\t"))
                  (push fields rtn))
                rtn)))
    (message "No segment definition file found.")))

(defun hl7-lookup-segment-def (seg-id segment-defs)
  "Return the alist associated with SEG-ID from SEGMENT-DEFS."
  (assoc seg-id segment-defs 'string=))

(defun hl7-comp-to-subcomp-nested-list (comp comp-id)
  "Convert an HL7 rep COMP to a nested list."
  (let* ((subcomps (split-string comp "&"))
         (n (length subcomps))
         (subcomp-nest (seq-mapn
                        (lambda (cid idx subcomp)
                          (let ((subcomp-id (format "%s.%d" cid idx)))
                            (append (list subcomp-id)
                                    (list (list subcomp)))))
                        (make-list n comp-id)
                        (number-sequence 1 n)
                        subcomps)))
    (append (list comp-id) subcomp-nest)))

(defun hl7-field-to-comp-nested-list (field field-id)
  "Convert an HL7 segment FIELD to a nested list."
  (let* ((comps (split-string field "\\^"))
         (n (length comps))
         (comp-nest (seq-mapn
                     (lambda (fid idx comp)
                       (let ((comp-id (format "%s.%d" fid idx)))
                         (if (string-match-p "&" comp)
                             (hl7-comp-to-subcomp-nested-list comp comp-id)
                           (append (list comp-id) (list (list comp))))))
                     (make-list n field-id)
                     (number-sequence 1 n)
                     comps)))
    (append (list field-id) comp-nest)))

(defun hl7-rep-to-comp-nested-list (rep rep-id)
  "Convert an HL7 field REP to a nested list."
  (let* ((comps (split-string rep "\\^"))
         (n (length comps))
         (comp-nest (seq-mapn
                     (lambda (fid idx comp)
                       (let ((comp-id (format "%s.%d" fid idx)))
                         (if (string-match-p "&" comp)
                             (hl7-comp-to-subcomp-nested-list comp comp-id)
                           (append (list comp-id) (list (list comp))))))
                     (make-list n rep-id)
                     (number-sequence 1 n)
                     comps)))
    (append (list rep-id) comp-nest)))

(defun hl7-field-to-rep-nested-list (field field-id)
  "Convert an HL7 segment FIELD to a nested list."
  (let* ((reps (split-string field "~"))
         (n (length reps))
         (rep-nest (seq-mapn
                     (lambda (fid idx rep)
                       (let ((rep-id (format "%s[%d]" fid idx)))
                         (if (string-match-p "\\^" rep)
                             (hl7-rep-to-comp-nested-list rep rep-id)
                           (append (list rep-id) (list (list rep))))))
                     (make-list n field-id)
                     (number-sequence 1 n)
                     reps)))
    (append (list field-id) rep-nest)))

(defun hl7-segment-to-field-nested-list (segment)
  "Convert an HL7 SEGMENT (line) to a nested list."
  (let* ((fields (split-string segment "|")) ; TODO: get this from MSH.1
         (n (length fields))
         (seg-id (car fields))
         field-nest msh.2)
    ;; Special handling for MSH segment
    (when (string= seg-id "MSH")
      ;; Insert "|" as MSH.1
      (setq fields (nconc (list (car fields)) '("|") (cdr fields))
            n (1+ n))
      ;; Save current MSH.2 and prevent further splitting of MSH.2
      (setq msh.2 (nth 2 fields))
      (setf (nth 2 fields) ""))
    (setq field-nest
          (seq-mapn
           (lambda (sid idx field)
             (let ((field-id (format "%s.%d" sid idx)))
               (cond
                ((string-match-p "~" field)
                 (hl7-field-to-rep-nested-list field field-id))
                ((string-match-p "\\^" field)
                 (hl7-field-to-comp-nested-list field field-id))
                (t
                 (append (list field-id) (list (list field)))))))
           (make-list n seg-id)
           (number-sequence 0 n)
           fields))
    (when msh.2
      ;; Restore original MSH.2 encoding characters
      (setf (cdr (nth 1 (cdr field-nest))) (list (list msh.2))))
    (append (list seg-id) (cdr field-nest))))

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
                segment-nest (hl7-segment-to-field-nested-list segment)
                root (append root (list segment-nest)))
          (forward-line 1)))
      root)))

;;;###autoload
(defun hl7-hier-add-annotation (hierarchy)
  "Add additional annotation to HIERARCHY's nodes."
  (hierarchy-map
   (lambda (item indent)
     (when-let* ((parent (hierarchy-parent hierarchy item))
                 (seg-lbl (or (cadr parent) ""))
                 (seg-id (replace-regexp-in-string "\\[[0-9]+\\]" "" seg-lbl))
                 (seg-def (hl7-lookup-segment-def seg-id hl7-segment-defs)))
       ;; Add table reference from parent description
       (when-let ((tbl (nth 6 seg-def)))
         (when (= 2 (length item))
           ;; Set this here so we can link nulls with tables
           ;; Deep copy to allow multiple property sets
           (when (string-empty-p (nth 1 item))
             (setcdr item (list (copy-sequence "(null)") )))
           (put-text-property 0 (length (nth 1 item)) 'table tbl (nth 1 item))))))
   hierarchy 0))

(defun hl7-hier-item-is-leaf-p (item indent)
  "Return non-nil if ITEM is a leaf.  INDENT is ignored."
  (not (and (listp item) (= 1 (length (cdr item))))))

(defun hl7-hier-make-button-p (item indent)
  "Return non-nil if a button should be made for ITEM.  INDENT is ignored."
  (or (hl7-hier-item-is-leaf-p item indent)
      (get-text-property 0 'table (nth 1 item))))

(defun hl7-hier-labelfn-button-label (item indent)
  "Insert the properly indented HL7 segment/field ID ITEM.  INDENT is ignored."
  (unless (bound-and-true-p hl7-segment-defs) ; TODO: get from MSH.12?
    (hl7-load-segment-defs))
  (let* ((seg-lbl (cadr item))
         (seg-id (replace-regexp-in-string "\\[[0-9]+\\]" "" seg-lbl))
         (seg-def (hl7-lookup-segment-def seg-id hl7-segment-defs)))
    (if (string= seg-lbl "")
        (insert "(null)")
      (insert seg-lbl))
    (when-let ((name (nth 1 seg-def)))
      ;; Name label
      (insert " - " name))
    (when-let ((opt (nth 4 seg-def)))
      ;; Optionality label
      (insert (pcase opt
                ("R" "*")
                ("O" "")
                ("C" "*")
                ("B" "")
                ("X" "")
                (_ (format " (%s)" opt))) ))))

(defun hl7-hier-labelfn-button-action (item indent)
  "Define the action for a hierarchy button (link) for ITEM.  INDENT is ignored."
  (let* ((ref-id (replace-regexp-in-string "\\[[0-9]+\\]" "" (cadr item)))
         url)
    (if-let ((table (get-text-property 0 'table (nth 1 item))))
        (setq url (format
                   "https://hl7-definition.caristix.com/v2/HL7v2.5.1/%s/%s"
                   "Tables" table))
      (setq url (format
                 "https://hl7-definition.caristix.com/v2/HL7v2.5.1/%s/%s"
                 (pcase (cl-count ?. ref-id)
                   (0 "Segments")
                   (1 "Fields")
                   (2 "Fields")
                   (3 "Fields"))
                 ref-id)))
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
(defun hl7-as-tree (&optional rm-null buffer)
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
    (hl7-hier-add-annotation msg-hier)
    (switch-to-buffer
     (hierarchy-tree-display
      msg-hier
      (hierarchy-labelfn-indent
       (hierarchy-labelfn-button-if
        'hl7-hier-labelfn-button-label
        'hl7-hier-make-button-p
        'hl7-hier-labelfn-button-action)
       ;; No additional indentation
       "")
      tree-buf))
    (tree-minor-mode 1)
    (tree-mode-expand-level 1)))

;;;###autoload
(defun hl7-as-tree-no-nulls (&optional buffer)
  "Same as `hl7-as-tree' for BUFFER but omits null entries."
  (interactive)
  (hl7-as-tree t buffer))

;;;###autoload
(defun hl7-as-list (&optional rm-null buffer)
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
    (hl7-hier-add-annotation msg-hier)
    (switch-to-buffer
     (hierarchy-tabulated-display
      msg-hier
      (hierarchy-labelfn-indent
       (hierarchy-labelfn-button-if
        'hl7-hier-labelfn-button-label
        'hl7-hier-make-button-p
        'hl7-hier-labelfn-button-action))
      text-buf))))

;;;###autoload
(defun hl7-as-list-no-nulls (&optional buffer)
  "Same as `hl7-as-list' for BUFFER but omits null entries."
  (interactive)
  (hl7-as-list t buffer))

(defun hl7-message-mode-eldoc ()
  "Eldoc function for hl7 message mode."
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (pt (point))
         seg-name field-beg field-end field-nbr comp-beg comp-end comp-nbr
         n-comps subc-nbr n-subcs rep-beg rep-end n-reps rep-nbr msh-adj line-pos)
    (setq seg-name (buffer-substring-no-properties beg (min (+ beg 3) (point-max))))
    (when (= 3 (length seg-name))
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
            n-subcs (how-many "&" comp-beg comp-end)
            line-pos (+ 1 (current-column))
            msh-adj 0)
      ;; Handle MSH segments
      (when (string= seg-name "MSH")
        (cond
         ((= 4 line-pos)
          (setq n-comps 0
                rep-nbr ""
                msh-adj 0
                field-nbr 1))
         ((<= 5 line-pos 9)
          (setq n-comps 0
                rep-nbr ""
                msh-adj 0
                field-nbr 2))
         (t
          (setq msh-adj 1))))
      (cond
       ((zerop field-nbr)
        seg-name)
       ((zerop n-comps)
        (format "%s.%s%s" seg-name (+ field-nbr msh-adj) rep-nbr))
       ((zerop n-subcs)
        (format "%s.%s.%s%s" seg-name (+ field-nbr msh-adj) comp-nbr rep-nbr))
       (t
        (format "%s.%s.%s.%s%s" seg-name (+ field-nbr msh-adj) comp-nbr subc-nbr rep-nbr))))))

(eldoc-add-command 'hl7-forward-component 'hl7-backward-component)

(defvar hl7-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'hl7-forward-component)
    (define-key map [backtab] 'hl7-backward-component)
    map)
  "Keymap of `hl7-message-mode'.")

;;;###autoload
(define-derived-mode hl7-message-mode fundamental-mode "HL7"
  "Major mode for viewing HL7 messages."
  (setq font-lock-defaults '((hl7-message-font-lock-defaults) nil t))
  (add-function :before-until (local 'eldoc-documentation-function)
                #'hl7-message-mode-eldoc))

(provide 'hl7-message)

;;; hl7-message.el ends here
