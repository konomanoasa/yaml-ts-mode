;;; yaml-ts-mode.el --- Tree-sitter mode for YAML  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/yaml-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for YAML 1.2.2.

;;; Code:

(require 'elec-pair)
(require 'newcomment)
(require 'treesit)

(defgroup yaml-ts nil
  "Tree-sitter mode for YAML 1.2.2."
  :group 'languages)

;;;; Grammar

(defconst yaml-ts-mode--grammar-sources
  '((yaml "https://github.com/konomanoasa/tree-sitter-yaml"
          :revision "v0.2.0"))
  "Tree-sitter grammar sources for YAML 1.2.2.")

(defun yaml-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language yaml-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

;;;; Context

(defconst yaml-ts-mode--scalar-regexp
  (rx string-start
      (or "plain_scalar" "single_quoted_scalar" "double_quoted_scalar"
          "literal_scalar" "folded_scalar")
      string-end)
  "Regexp matching scalar node types.")

(defconst yaml-ts-mode--flow-regexp
  (rx string-start (or "flow_sequence" "flow_mapping") string-end)
  "Regexp matching flow collection node types.")

(defconst yaml-ts-mode--collection-regexp
  (rx string-start
      (or "block_mapping" "flow_mapping" "block_sequence" "flow_sequence")
      string-end)
  "Regexp matching collection node types.")

(defconst yaml-ts-mode--owner-regexp
  (rx string-start
      (or "block_mapping_pair" "block_sequence_entry" "document")
      string-end)
  "Regexp matching node types that own block content.")

;;;; Syntax

(defvar yaml-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\' ?` ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar yaml-ts-mode-syntax-table
  (let ((table (copy-syntax-table yaml-ts-mode-syntax--text-table)))
    (dolist (entry '((?\[ . "(]") (?\] . ")[") (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `yaml-ts-mode'.")

;;;;; Syntax Queries

(defconst yaml-ts-mode-syntax--query
  (treesit-query-compile
   'yaml
   '((comment_marker) @comment
     [(single_quoted_scalar) (double_quoted_scalar)] @string
     [(flow_sequence_open) (flow_sequence_close)
      (flow_mapping_open) (flow_mapping_close)]
     @delimiter))
  "Compiled syntax query for YAML 1.2.2.")

;;;;; Propertization

(defun yaml-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table yaml-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        yaml-ts-mode-syntax--query start end))
        (let ((node (cdr capture)))
          (pcase (car capture)
            ('comment
             (put-text-property (treesit-node-start node) (treesit-node-end node)
                                'syntax-table (string-to-syntax "<")))
            ('string
             (let ((quotes (list (treesit-node-child-by-field-name node "opening")
                                 (treesit-node-child-by-field-name node "closing"))))
               (put-text-property (treesit-node-start node) (treesit-node-end node)
                                  'syntax-table yaml-ts-mode-syntax--text-table)
               (unless (or (memq nil quotes)
                           (treesit-node-check node 'has-error)
                           (treesit-search-subtree node "^syntax_issue$"))
                 (dolist (quote quotes)
                   (put-text-property (treesit-node-start quote)
                                      (treesit-node-end quote)
                                      'syntax-table (string-to-syntax "|"))))))
            ('delimiter
             (put-text-property (treesit-node-start node) (treesit-node-end node)
                                'syntax-table
                                (string-to-syntax
                                 (pcase (treesit-node-type node)
                                   ("flow_sequence_open" "(]")
                                   ("flow_sequence_close" ")[")
                                   ("flow_mapping_open" "(}")
                                   ("flow_mapping_close" "){")))))))))))

;;;;; Setup

(defun yaml-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'yaml-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t))

;;;; Comment Commands

(defun yaml-ts-mode-comment--uncomment-region (beg end &optional arg)
  "Uncomment BEG through END using syntax classified before editing.
Pass ARG to `uncomment-region-default'."
  (syntax-propertize end)
  (unwind-protect
      (let ((syntax-propertize-function nil))
        (uncomment-region-default beg end arg))
    (syntax-ppss-flush-cache beg)))

(defun yaml-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local uncomment-region-function #'yaml-ts-mode-comment--uncomment-region))

;;;; Electric Pair

(defun yaml-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil for a flow collection delimiter pair around the newline."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (treesit-node-match-p owner yaml-ts-mode--flow-regexp)
           (treesit-node-eq opening (treesit-node-child-by-field-name owner "opening"))
           (treesit-node-eq closing (treesit-node-child-by-field-name owner "closing"))))))

(defun yaml-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\[ . ?\]) (?{ . ?})
                 (?\" . ?\") (?\' . ?\')))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (yaml-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst yaml-ts-mode-font-lock--feature-list
  '((comment)
    (property string type preprocessor)
    (constant number escape)
    (punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun yaml-ts-mode-font-lock--settings ()
  "Return font-lock settings for the current buffer."
  (treesit-font-lock-rules
   :default-language 'yaml

   :feature 'comment
   '([(comment_marker) (comment_text)] @font-lock-comment-face)

   :feature 'property
   '((_ key: [(plain_scalar (scalar_text) @font-lock-property-name-face)
              (single_quoted_scalar (scalar_text) @font-lock-property-name-face)
              (double_quoted_scalar (scalar_text) @font-lock-property-name-face)
              (node_with_properties
               content: [(plain_scalar (scalar_text) @font-lock-property-name-face)
                         (single_quoted_scalar
                          (scalar_text) @font-lock-property-name-face)
                         (double_quoted_scalar
                          (scalar_text) @font-lock-property-name-face)])]))

   :feature 'string
   '([(scalar_text) (quote_open) (quote_close) (directive_parameter)]
     @font-lock-string-face)

   :feature 'type
   '([(tag_handle_name) (tag_suffix_text) (uri_text)] @font-lock-type-face)

   :feature 'preprocessor
   '([(directive_indicator) (directive_name)] @font-lock-preprocessor-face)

   :feature 'constant
   '([(anchor_name) (alias_name)] @font-lock-constant-face)

   :feature 'number
   '([(yaml_version) (indentation_indicator)] @font-lock-number-face)

   :feature 'escape
   '([(quoted_escape) (escaped_quote) (escape_indicator) (uri_escape)]
     @font-lock-escape-face)

   :feature 'punctuation
   '([(sequence_indicator) (key_indicator) (value_indicator) (flow_separator)
      (anchor_indicator) (alias_indicator) (primary_tag_handle)
      (secondary_tag_handle) (tag_handle_open) (tag_handle_close)
      (verbatim_tag_open) (verbatim_tag_close) (non_specific_tag)
      (document_start) (document_end) (literal_indicator) (folded_indicator)
      (chomping_indicator)]
     @font-lock-punctuation-face)

   :feature 'bracket
   '([(flow_sequence_open) (flow_sequence_close)
      (flow_mapping_open) (flow_mapping_close)]
     @font-lock-bracket-face)))

;;;;; Setup

(defun yaml-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              yaml-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (yaml-ts-mode-font-lock--settings)))

;;;; Navigation

(defun yaml-ts-mode-navigation--document-content-p (node)
  "Return non-nil if NODE is document content through property wrappers."
  (let ((parent (treesit-node-parent node)))
    (while (and (equal (treesit-node-type parent) "node_with_properties")
                (equal (treesit-node-field-name node) "content"))
      (setq node parent
            parent (treesit-node-parent parent)))
    (and (equal (treesit-node-type parent) "document")
         (equal (treesit-node-field-name node) "content"))))

(defun yaml-ts-mode-navigation--defun-p (node)
  "Return non-nil if NODE is an outermost YAML item."
  (let ((parent (treesit-node-parent node)))
    (or (and (treesit-node-match-p parent yaml-ts-mode--collection-regexp)
             (yaml-ts-mode-navigation--document-content-p parent))
        (and (equal (treesit-node-type parent) "document")
             (equal (treesit-node-field-name node) "content")
             (progn
               (while (equal (treesit-node-type node) "node_with_properties")
                 (setq node (treesit-node-child-by-field-name node "content")))
               (not (treesit-node-match-p node yaml-ts-mode--collection-regexp)))))))

(defconst yaml-ts-mode-navigation--settings
  `((yaml
     (sexp (or ,yaml-ts-mode--collection-regexp
               ,yaml-ts-mode--scalar-regexp
               ,(rx string-start
                    (or "block_mapping_pair" "flow_mapping_pair"
                        "block_sequence_entry" "alias" "node_with_properties")
                    string-end)))
     (defun (and sexp yaml-ts-mode-navigation--defun-p))))
  "Tree-sitter thing definitions for YAML 1.2.2.")

(defun yaml-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              yaml-ts-mode-navigation--settings)
  (setq-local treesit-defun-skipper nil))

;;;; Imenu

(defun yaml-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (and (equal (treesit-node-type node) "anchor")
             (treesit-node-child-by-field-name node "name"))
    (treesit-node-text (treesit-node-child-by-field-name node "name") t)))

(defconst yaml-ts-mode-imenu--settings
  '(("Anchor" "^anchor$" yaml-ts-mode-imenu--name nil))
  "Tree-sitter Imenu settings for YAML 1.2.2.")

(defun yaml-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'yaml-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              yaml-ts-mode-imenu--settings))

;;;; Indentation

(defcustom yaml-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'yaml-ts)

;;;;; Helpers

(defun yaml-ts-mode-indent--column (position)
  "Return the column of POSITION."
  (save-excursion (goto-char position) (current-column)))

(defun yaml-ts-mode-indent--line-start (position)
  "Return the first non-whitespace position on the line of POSITION."
  (save-excursion (goto-char position) (back-to-indentation) (point)))

(defun yaml-ts-mode-indent--largest-node (position)
  "Return the largest node that starts at POSITION."
  (let ((node (treesit-node-at position)))
    (while (and (treesit-node-parent node)
                (= (treesit-node-start (treesit-node-parent node)) position))
      (setq node (treesit-node-parent node)))
    node))

(defun yaml-ts-mode-indent--previous-token (bol)
  "Return the last node before BOL that is not layout or comment."
  (let* ((node (treesit-node-at (max (point-min) (1- bol)) treesit-primary-parser))
         (predicate (lambda (candidate)
                      (and (yaml-ts-mode-indent--content-leaf-p candidate)
                           (<= (treesit-node-end candidate) bol)))))
    (if (funcall predicate node) node
      (treesit-search-forward node predicate t t))))

(defun yaml-ts-mode-indent--content-leaf-p (node)
  "Return non-nil if NODE is a non-layout leaf outside a comment."
  (and node (zerop (treesit-node-child-count node))
       (< (treesit-node-start node) (treesit-node-end node))
       (not (treesit-parent-until
             node
             (rx string-start
                 (or "line_break" "separation" "line_prefix" "comment"
                     "scalar_line_break" "scalar_line_prefix"
                     "scalar_line_suffix" "block_header_break")
                 string-end)
             t))))

(defun yaml-ts-mode-indent--content (owner node bol)
  "Return the indentation of NODE at BOL as content of OWNER."
  (if (equal (treesit-node-type owner) "document")
      (cons bol 0)
    (cons (treesit-node-start owner)
          (if (and (equal (treesit-node-type node) "block_sequence")
                   (equal (treesit-node-type owner) "block_mapping_pair")
                   (= (yaml-ts-mode-indent--column (treesit-node-start node))
                      (yaml-ts-mode-indent--column (treesit-node-start owner))))
              0
            yaml-ts-mode-indent-offset))))

(defun yaml-ts-mode-indent--flow (flow closing)
  "Return the indentation of a line in FLOW.
CLOSING non-nil means that the line starts with the closing delimiter."
  (let* ((opening (yaml-ts-mode-indent--line-start (treesit-node-start flow)))
         (owner (treesit-parent-until flow yaml-ts-mode--owner-regexp))
         (block (and owner
                     (not (equal (treesit-node-type owner) "document"))
                     (<= (yaml-ts-mode-indent--column opening)
                         (yaml-ts-mode-indent--column (treesit-node-start owner)))
                     (treesit-node-start owner))))
    (cons (or block opening)
          (if (and closing (not block)) 0 yaml-ts-mode-indent-offset))))

;;;;; Rules

(defun yaml-ts-mode-indent--blank-line (_node _parent bol)
  "Return the indentation of the blank line at BOL."
  (when (save-excursion (goto-char bol) (eolp))
    (let* ((token (yaml-ts-mode-indent--previous-token bol))
           (parent (and token (treesit-node-parent token)))
           (flow (and token
                      (treesit-parent-until
                       (if (equal (treesit-node-field-name token) "closing")
                           parent
                         token)
                       yaml-ts-mode--flow-regexp)))
           (properties (and token (treesit-parent-until
                                   token "^node_with_properties$")))
           (header (and token (treesit-parent-until
                               token "^block_scalar_header$" t))))
      (cond
       ((null token)
        (cons (save-excursion (goto-char bol) (line-beginning-position)) 0))
       (flow (yaml-ts-mode-indent--flow flow nil))
       ((and header (treesit-node-child-by-field-name header "indentation"))
        (let ((owner (treesit-parent-until header yaml-ts-mode--owner-regexp))
              (width (treesit-node-child-by-field-name header "indentation")))
          (cons (if (equal (treesit-node-type owner) "document")
                    (save-excursion (goto-char bol) (line-beginning-position))
                  (treesit-node-start owner))
                (string-to-number (treesit-node-text width t)))))
       ((or (and (member (treesit-node-type token)
                         '("value_indicator" "sequence_indicator"))
                 (not (treesit-node-child-by-field-name parent "value")))
            (and (equal (treesit-node-type token) "key_indicator")
                 (not (treesit-node-child-by-field-name parent "key")))
            (and properties
                 (not (treesit-node-child-by-field-name properties "content")))
            header)
        (yaml-ts-mode-indent--content
         (treesit-parent-until token yaml-ts-mode--owner-regexp) nil bol))
       (t (cons (yaml-ts-mode-indent--line-start (treesit-node-start token)) 0))))))

(defun yaml-ts-mode-indent--comment-line (node _parent bol)
  "Return the indentation of the content line after the comment NODE at BOL."
  (when (equal (treesit-node-type node) "comment")
    (let ((next (treesit-search-forward
                 node #'yaml-ts-mode-indent--content-leaf-p nil t)))
      (if next
          (progn
            (setq next (yaml-ts-mode-indent--largest-node (treesit-node-start next)))
            (treesit-simple-indent next (treesit-node-parent next)
                                   (treesit-node-start next)))
        (cons bol 0)))))

(defun yaml-ts-mode-indent--scalar-line (node parent bol)
  "Return the indentation of BOL at NODE in PARENT within a scalar."
  (let ((scalar (treesit-parent-until
                 (or node parent) yaml-ts-mode--scalar-regexp t)))
    (when (and scalar (< (treesit-node-start scalar) bol)
               (not (save-excursion (goto-char bol) (and (bolp) (eolp)))))
      (let* ((anchor (yaml-ts-mode-indent--line-start (treesit-node-start scalar)))
             (offset (- (yaml-ts-mode-indent--column bol)
                        (yaml-ts-mode-indent--column anchor))))
        (if (< offset 0) (cons bol 0) (cons anchor offset))))))

(defun yaml-ts-mode-indent--flow-line (node _parent _bol)
  "Return the indentation of NODE within a flow collection."
  (let ((flow (treesit-parent-until node yaml-ts-mode--flow-regexp)))
    (when flow
      (yaml-ts-mode-indent--flow
       flow (and (treesit-node-eq (treesit-node-parent node) flow)
                 (equal (treesit-node-field-name node) "closing"))))))

(defun yaml-ts-mode-indent--block-line (node parent bol)
  "Return the indentation of NODE at BOL in PARENT within a block structure."
  (cond
   ((member (treesit-node-type parent) '("block_mapping" "block_sequence"))
    (cons (treesit-node-start parent) 0))
   ((equal (treesit-node-type node) "value_indicator")
    (cons (treesit-node-start parent) 0))
   (t
    (let ((owner (treesit-parent-until node yaml-ts-mode--owner-regexp)))
      (when owner
        (yaml-ts-mode-indent--content owner node bol))))))

(defun yaml-ts-mode-indent--keep (_node _parent bol)
  "Return BOL to preserve the current indentation."
  bol)

(defconst yaml-ts-mode-indent--rules
  '((yaml
     yaml-ts-mode-indent--scalar-line
     yaml-ts-mode-indent--blank-line
     yaml-ts-mode-indent--comment-line
     yaml-ts-mode-indent--flow-line
     yaml-ts-mode-indent--block-line
     (catch-all yaml-ts-mode-indent--keep 0)))
  "Tree-sitter indentation rules for YAML 1.2.2.")

;;;;; Setup

(defun yaml-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local indent-tabs-mode nil)
  (setq-local treesit-simple-indent-rules
              yaml-ts-mode-indent--rules))

;;;; Mode

(defun yaml-ts-mode--setup ()
  "Configure `yaml-ts-mode' in the current buffer."
  (yaml-ts-mode--ensure-grammar 'yaml)
  (setq-local treesit-primary-parser (treesit-parser-create 'yaml))
  (yaml-ts-mode-syntax--setup)
  (yaml-ts-mode-comment--setup)
  (yaml-ts-mode-electric-pair--setup)
  (yaml-ts-mode-font-lock--setup)
  (yaml-ts-mode-navigation--setup)
  (yaml-ts-mode-imenu--setup)
  (yaml-ts-mode-indent--setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode yaml-ts-mode prog-mode "YAML-TS"
  "Major mode for editing YAML 1.2.2."
  :syntax-table yaml-ts-mode-syntax-table
  :group 'yaml-ts
  (yaml-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "." (or "yaml" "yml" "clangd" "clang-format") string-end)
                   'yaml-ts-mode))

(provide 'yaml-ts-mode)

;;; yaml-ts-mode.el ends here
