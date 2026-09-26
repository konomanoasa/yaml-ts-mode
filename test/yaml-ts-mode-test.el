;;; yaml-ts-mode-test.el --- Tests for yaml-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'yaml-ts-mode)

(dolist (language '(yaml))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun yaml-ts-mode-test--position (fragment)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward fragment nil t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun yaml-ts-mode-test--face (fragment &optional offset)
  (get-text-property (+ (yaml-ts-mode-test--position fragment) (or offset 0)) 'face))

(defun yaml-ts-mode-test--syntax-class (fragment &optional offset)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (yaml-ts-mode-test--position fragment)
                                 (or offset 0)))))

(defun yaml-ts-mode-test--indent (source &optional offset)
  (with-temp-buffer
    (insert source)
    (yaml-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local yaml-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun yaml-ts-mode-test--new-line-column (source)
  (with-temp-buffer
    (insert source)
    (yaml-ts-mode)
    (goto-char (point-max))
    (electric-indent-local-mode 1)
    (call-interactively (key-binding (kbd "RET")))
    (current-column)))

(defun yaml-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun yaml-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (yaml-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (yaml-ts-mode))
      (should (equal state (yaml-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest yaml-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source yaml-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (yaml-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest yaml-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (should-error (yaml-ts-mode) :type 'user-error)
            (should-not (treesit-parser-list))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest yaml-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "key: value\n")
    (yaml-ts-mode)
    (should (eq major-mode 'yaml-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'yaml))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "stream"))
    (should-not indent-tabs-mode)
    (yaml-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

;;;; Mode Selection

(ert-deftest yaml-ts-mode-selects-files ()
  (dolist (entry '(("/tmp/config.yaml" . t)
                   ("/tmp/config.clangd" . t)
                   ("/tmp/config.clang-format" . t)
                   ("/tmp/.clangd" . t)
                   ("/tmp/.clang-format" . t)
                   ("/tmp/.github/workflows/ci.yml" . t)
                   ("config.yml" . t)
                   ("/tmp/.clang" . nil)
                   ("/tmp/config.clang-format.txt" . nil)
                   ("/tmp/config.yaml.txt" . nil)
                   ("/tmp/config.yamlx" . nil)
                   ("/tmp/yaml" . nil)))
    (with-temp-buffer
      (setq buffer-file-name (car entry))
      (let ((auto-mode-alist
             (delq nil
                   (mapcar (lambda (registration)
                             (and (eq (cdr registration) 'yaml-ts-mode)
                                  registration))
                           auto-mode-alist))))
        (set-auto-mode))
      (should (eq (eq major-mode 'yaml-ts-mode) (cdr entry))))))

(ert-deftest yaml-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "yaml-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "yaml-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'yaml-ts-mode" "(add-to-list 'auto-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest yaml-ts-mode-classifies-owned-delimiters-and-quotes ()
  (with-temp-buffer
    (insert "a: {b: [\"[\", 'it''s', c]}\nd: e[f]\ng: |\n  {x}\n")
    (yaml-ts-mode)
    (pcase-dolist (`(,fragment ,offset ,class)
                   '(("{b" 0 4) ("[\"" 0 4) ("\"[\"" 0 15) ("\"[\"" 1 1) ("\"[\"" 2 15)
                     ("'it" 0 15) ("''s" 0 1) ("''s" 1 1) ("', c" 0 15)
                     ("]}" 0 5) ("}\n" 0 5) ("[f]" 0 1) ("f]" 1 1)
                     ("{x}" 0 1) ("x}" 1 1)))
      (ert-info ((format "%S at %d" fragment offset))
        (should (= (yaml-ts-mode-test--syntax-class fragment offset) class))))
    (should (= (scan-sexps (yaml-ts-mode-test--position "{b") 1)
               (yaml-ts-mode-test--position "\nd:")))
    (should (nth 3 (syntax-ppss (+ (yaml-ts-mode-test--position "[\"") 2))))))

(ert-deftest yaml-ts-mode-keeps-unowned-and-unterminated-delimiters-as-punctuation ()
  (with-temp-buffer
    (insert "a: ]b)\nc: [d\n\"unfinished\n")
    (yaml-ts-mode)
    (dolist (fragment '("]b" ")\n" "\"unfinished"))
      (ert-info ((format "%S" fragment))
        (should (= (yaml-ts-mode-test--syntax-class fragment) 1))))
    (should (= (yaml-ts-mode-test--syntax-class "[d") 4))))

(ert-deftest yaml-ts-mode-classifies-comments ()
  (with-temp-buffer
    (insert "# head\na#b: \"# quoted\" # tail\nc: |\n  # text\nd: e\r# after cr\n")
    (yaml-ts-mode)
    (syntax-propertize (point-max))
    (dolist (entry '(("head" . t) ("tail" . t) ("after cr" . t)
                     ("#b" . nil) ("quoted" . nil) ("# text" . nil) ("d: e" . nil)))
      (ert-info ((format "%S" (car entry)))
        (should (eq (not (null (nth 4 (syntax-ppss
                                       (1+ (yaml-ts-mode-test--position (car entry)))))))
                    (cdr entry)))))
    (should (nth 3 (syntax-ppss (yaml-ts-mode-test--position "quoted"))))))

(ert-deftest yaml-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "key: value\n# note\n")
    (yaml-ts-mode)
    (comment-region 1 11)
    (should (equal (buffer-string) "# key: value\n# note\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-string) "key: value\nnote\n"))))

;;;; Electric Pair

(ert-deftest yaml-ts-mode-pairs-delimiters-and-indents-on-return ()
  (dolist (case '(("" ?\[ "[\n  \n]" 2) ("a: " ?{ "a: {\n  \n  }" 2)))
    (let ((electric-pair-pairs nil)
          (electric-pair-open-newline-between-pairs t))
      (with-temp-buffer
        (yaml-ts-mode)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (electric-pair-local-mode 1)
        (insert (nth 0 case))
        (let ((last-command-event (nth 1 case)))
          (self-insert-command 1))
        (should (= (char-after) (cdr (assq (nth 1 case) electric-pair-pairs))))
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-string) (nth 2 case)))
        (should (= (current-column) (nth 3 case)))))))

(ert-deftest yaml-ts-mode-keeps-pair-preferences-local ()
  (let ((electric-pair-pairs '((?% . ?%)))
        (electric-pair-mode nil)
        (electric-pair-open-newline-between-pairs nil))
    (with-temp-buffer
      (yaml-ts-mode)
      (should-not electric-pair-mode)
      (should (equal (car electric-pair-pairs) '(?% . ?%)))
      (electric-indent-local-mode 1)
      (electric-pair-local-mode 1)
      (insert "")
      (let ((last-command-event ?\[))
        (self-insert-command 1))
      (call-interactively (key-binding (kbd "RET")))
      (should (= (count-lines (point-min) (point-max)) 2)))
    (should (equal electric-pair-pairs '((?% . ?%))))
    (should-not electric-pair-open-newline-between-pairs)))

(ert-deftest yaml-ts-mode-respects-custom-pair-closings ()
  (let ((electric-pair-pairs '((?\[ . ?!) (?{ . ?@))))
    (dolist (case '((?\[ "[!") (?{ "{@")))
      (with-temp-buffer
        (yaml-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event (car case))) (self-insert-command 1))
        (should (equal (buffer-string) (cadr case)))))
    (should (equal electric-pair-pairs '((?\[ . ?!) (?{ . ?@))))))

(ert-deftest yaml-ts-mode-restricts-pair-newlines-to-flow-collections ()
  (let ((electric-pair-open-newline-between-pairs t))
    (dolist (case '(("[]" 2 "[\n\n]")
                    ("key: {}" 7 "key: {\n\n}")
                    ("key: plain[]" 12 "key: plain[\n]")
                    ("key: \"[]\"" 8 "key: \"[\n]\"")
                    ("key: |\n  {}" 11 "key: |\n  {\n}")
                    ("# []" 4 "# [\n]")))
      (with-temp-buffer
        (insert (nth 0 case))
        (yaml-ts-mode)
        (electric-indent-local-mode -1)
        (electric-pair-local-mode 1)
        (goto-char (nth 1 case))
        (let ((last-command-event ?\n)) (self-insert-command 1))
        (should (equal (buffer-string) (nth 2 case)))))))

(ert-deftest yaml-ts-mode-respects-pair-newline-functions ()
  (dolist (setting (list nil (lambda () nil) (lambda () t)))
    (let ((electric-pair-open-newline-between-pairs setting))
      (with-temp-buffer
        (insert "[]")
        (yaml-ts-mode)
        (electric-indent-local-mode -1)
        (electric-pair-local-mode 1)
        (goto-char 2)
        (let ((last-command-event ?\n)) (self-insert-command 1))
        (should (equal (buffer-string)
                       (if (and setting (funcall setting)) "[\n\n]" "[\n]"))))
      (should (eq electric-pair-open-newline-between-pairs setting)))))

;;;; Font Lock

(ert-deftest yaml-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (let ((treesit-font-lock-level level))
      (with-temp-buffer
        (insert "%YAML 1.2\n--- # note\nkey: !!str &a \"v\\n\"\nref: [*a]\n")
        (yaml-ts-mode)
        (font-lock-ensure)
        (pcase-dolist (`(,fragment ,offset ,minimum ,face)
                       '(("note" 0 1 font-lock-comment-face)
                         ("key" 0 2 font-lock-property-name-face)
                         ("\"v" 0 2 font-lock-string-face)
                         ("\"v" 1 2 font-lock-string-face)
                         ("str" 0 2 font-lock-type-face)
                         ("%YAML" 0 2 font-lock-preprocessor-face)
                         ("YAML" 0 2 font-lock-preprocessor-face)
                         ("a \"" 0 3 font-lock-constant-face)
                         ("a]" 0 3 font-lock-constant-face)
                         ("1.2" 0 3 font-lock-number-face)
                         ("\\n" 0 3 font-lock-escape-face)
                         ("---" 0 4 font-lock-punctuation-face)
                         (": !!" 0 4 font-lock-punctuation-face)
                         ("!!" 0 4 font-lock-punctuation-face)
                         ("&" 0 4 font-lock-punctuation-face)
                         ("*" 0 4 font-lock-punctuation-face)
                         ("[" 0 4 font-lock-bracket-face)))
          (ert-info ((format "Level %s: %S at %d" level fragment offset))
            (should (eq (yaml-ts-mode-test--face fragment offset)
                        (and (>= level minimum) face)))))))))

(ert-deftest yaml-ts-mode-fontifies-keys-by-owning-context ()
  (with-temp-buffer
    (insert "plain: 1\n\"dq\\tk\": 2\n'sq': 3\n!t tagged: 4\n? explicit\n: 5\n"
            "{fk: fv, [ck]: cv}: 6\nseq: [sk: sv]\nblock: |\n  # k: v\n"
            "...\n%FOO bar\n---\ntrue: null\n")
    (let ((treesit-font-lock-level 4)) (yaml-ts-mode))
    (font-lock-ensure)
    (pcase-dolist (`(,fragment ,offset ,face)
                   '(("plain" 0 font-lock-property-name-face)
                     ("1\n" 0 font-lock-string-face)
                     ("\"dq" 0 font-lock-string-face)
                     ("dq" 0 font-lock-property-name-face)
                     ("\\t" 0 font-lock-escape-face)
                     ("\\t" 1 font-lock-escape-face)
                     ("k\":" 0 font-lock-property-name-face)
                     ("sq" 0 font-lock-property-name-face)
                     ("'sq" 0 font-lock-string-face)
                     ("tagged" 0 font-lock-property-name-face)
                     ("explicit" 0 font-lock-property-name-face)
                     ("fk" 0 font-lock-property-name-face)
                     ("fv" 0 font-lock-string-face)
                     ("ck" 0 font-lock-string-face)
                     ("cv" 0 font-lock-string-face)
                     ("sk" 0 font-lock-property-name-face)
                     ("sv" 0 font-lock-string-face)
                     ("# k: v" 0 font-lock-string-face)
                     ("k: v" 3 font-lock-string-face)
                     ("|\n" 0 font-lock-punctuation-face)
                     ("FOO" 0 font-lock-preprocessor-face)
                     ("bar" 0 font-lock-string-face)
                     ("true" 0 font-lock-property-name-face)
                     ("null" 0 font-lock-string-face)
                     (" null" 0 nil)))
      (ert-info ((format "%S at %d" fragment offset))
        (should (eq (yaml-ts-mode-test--face fragment offset) face))))))

(ert-deftest yaml-ts-mode-fontifies-tags-and-directives ()
  (with-temp-buffer
    (insert "%TAG !e! tag:example.com,2000:%41pp/\n--- !e!point\n"
            "a: !<tag:x%21> v\nb: ! w\nc: >-2\n  text\n")
    (let ((treesit-font-lock-level 4)) (yaml-ts-mode))
    (font-lock-ensure)
    (pcase-dolist (`(,fragment ,offset ,face)
                   '(("%TAG" 0 font-lock-preprocessor-face)
                     ("!e! " 0 font-lock-punctuation-face)
                     ("!e! " 1 font-lock-type-face)
                     ("!e! " 2 font-lock-punctuation-face)
                     ("tag:example" 0 font-lock-type-face)
                     ("%41" 0 font-lock-escape-face)
                     ("pp/" 0 font-lock-type-face)
                     ("point" 0 font-lock-type-face)
                     ("!<" 0 font-lock-punctuation-face)
                     ("!<" 1 font-lock-punctuation-face)
                     ("tag:x" 0 font-lock-type-face)
                     ("%21" 0 font-lock-escape-face)
                     ("> v" 0 font-lock-punctuation-face)
                     ("! w" 0 font-lock-punctuation-face)
                     (">-2" 0 font-lock-punctuation-face)
                     (">-2" 1 font-lock-punctuation-face)
                     (">-2" 2 font-lock-number-face)
                     ("text" 0 font-lock-string-face)))
      (ert-info ((format "%S at %d" fragment offset))
        (should (eq (yaml-ts-mode-test--face fragment offset) face))))))

;;;; Navigation

(ert-deftest yaml-ts-mode-navigates-nested-sexp-boundaries ()
  (with-temp-buffer
    (insert "first: {inner: [alpha, beta]}\nsecond: &ref 'text'\n")
    (yaml-ts-mode)
    (should (equal (mapcar #'car (cdr (assq 'yaml treesit-thing-settings)))
                   '(sexp defun)))
    (goto-char (point-min))
    (forward-sexp)
    (should (= (point) (point-max)))
    (backward-sexp)
    (should (= (point) (point-min)))
    (dolist (case '(("inner" 0 1 "]}" 1)
                    ("]}" 1 -1 "inner" 0)
                    ("inner" 1 1 "inner" 5)
                    ("[alpha" 0 1 "]}" 1)
                    ("alpha" 0 1 "alpha" 5)
                    ("alpha" 5 -1 "alpha" 0)
                    ("'text'" 0 1 "'text'" 6)
                    ("'text'" 6 -1 "second" 0)))
      (ert-info ((format "%S" case))
        (goto-char (+ (yaml-ts-mode-test--position (nth 0 case)) (nth 1 case)))
        (forward-sexp (nth 2 case))
        (should (= (point) (+ (yaml-ts-mode-test--position (nth 3 case))
                              (nth 4 case))))))))

(ert-deftest yaml-ts-mode-selects-only-outermost-defun-items ()
  (with-temp-buffer
    (insert "--- &root\nfirst:\n  nested: value\nsecond: [one, two]\n"
            "---\n!seq [alpha, {b: c}, d: e]\n---\n!map {f: g, h: i}\n"
            "---\n- one\n- two\n---\n!tag scalar\n---\n*alias\n---\n&empty\n"
            "---\n[]\n---\n{}\n")
    (yaml-ts-mode)
    (let (items)
      (dolist (capture (treesit-query-capture (treesit-buffer-root-node) '((_) @node)))
        (when (treesit-node-match-p (cdr capture) 'defun)
          (push (treesit-node-text (cdr capture) t) items)))
      (should (equal (nreverse items)
                     '("first:\n  nested: value\n" "second: [one, two]"
                       "alpha" "{b: c}" "d: e" "f: g" "h: i"
                       "- one" "- two" "!tag scalar" "*alias" "&empty\n"))))
    (goto-char (yaml-ts-mode-test--position "nested"))
    (beginning-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "first:")))
    (end-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "second:")))
    (narrow-to-region (yaml-ts-mode-test--position "!seq")
                      (yaml-ts-mode-test--position "!map"))
    (goto-char (yaml-ts-mode-test--position "b: c"))
    (beginning-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "{b: c}")))))

(ert-deftest yaml-ts-mode-navigates-flow-collections-and-outermost-items ()
  (with-temp-buffer
    (insert "# head\na: {b: [1, 2], c: \"x y\"}\n---\nd: e\n...\n")
    (yaml-ts-mode)
    (goto-char (yaml-ts-mode-test--position "{b"))
    (forward-sexp)
    (should (= (point) (yaml-ts-mode-test--position "\n---")))
    (goto-char (yaml-ts-mode-test--position "\"x y\""))
    (forward-sexp)
    (should (= (point) (yaml-ts-mode-test--position "}\n")))
    (goto-char (yaml-ts-mode-test--position "c:"))
    (beginning-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "a:")))
    (end-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "---")))
    (goto-char (yaml-ts-mode-test--position "e\n"))
    (beginning-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "d:")))
    (end-of-defun)
    (should (= (point) (yaml-ts-mode-test--position "...")))))

;;;; Imenu

(ert-deftest yaml-ts-mode-indexes-anchors ()
  (with-temp-buffer
    (insert "base: &base\n  a: 1\nnext: &base {b: 2}\nref: *base\nnone: & x\n")
    (yaml-ts-mode)
    (let* ((index (funcall imenu-create-index-function))
           (entries (cdr (assoc "Anchor" index))))
      (should (equal (mapcar #'car index) '("Anchor")))
      (should (equal (mapcar #'car entries) '("base" "base")))
      (should (equal (mapcar (lambda (entry) (marker-position (cdr entry))) entries)
                     (list (yaml-ts-mode-test--position "&base\n")
                           (yaml-ts-mode-test--position "&base {")))))))

(ert-deftest yaml-ts-mode-updates-anchor-names-after-edits ()
  (with-temp-buffer
    (insert "key: &before value\n")
    (yaml-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Anchor" (funcall imenu-create-index-function))))
                   '("before")))
    (goto-char (yaml-ts-mode-test--position "before"))
    (delete-char 6)
    (insert "after")
    (should (equal (mapcar #'car (cdr (assoc "Anchor" (funcall imenu-create-index-function))))
                   '("after")))))

;;;; Indentation

(ert-deftest yaml-ts-mode-indents-block-structures ()
  (should (equal (yaml-ts-mode-test--indent
                  (concat "top:\n      a: 1\n      list:\n      - x\n      -\n          y\n"
                          "      nested:\n       - k: v\n         w: z\n"
                          "      ? key\n      : value\n"
                          "props: &p !!map\n        c: d\n")
                  3)
                 (concat "top:\n   a: 1\n   list:\n   - x\n   -\n      y\n"
                         "   nested:\n      - k: v\n        w: z\n"
                         "   ? key\n   : value\n"
                         "props: &p !!map\n   c: d\n"))))

(ert-deftest yaml-ts-mode-preserves-document-level-indentation ()
  (should (equal (yaml-ts-mode-test--indent
                  (concat "%YAML 1.2\n---\n  a: 1\n  b:\n      c: 2\n...\n"
                          "--- !!seq\n  - x\n---\n  ---\n")
                  2)
                 (concat "%YAML 1.2\n---\n  a: 1\n  b:\n    c: 2\n...\n"
                         "--- !!seq\n  - x\n---\n  ---\n"))))

(ert-deftest yaml-ts-mode-indents-flow-collections ()
  (should (equal (yaml-ts-mode-test--indent
                  (concat "a: [\n      1,\n     [\n    2\n   ],\n ]\n"
                          "---\n- b: {\n   c: d\n   }\n"
                          "---\n[\n x\n   ]\n")
                  2)
                 (concat "a: [\n  1,\n  [\n    2\n  ],\n  ]\n"
                         "---\n- b: {\n    c: d\n    }\n"
                         "---\n[\n  x\n]\n"))))

(ert-deftest yaml-ts-mode-preserves-scalar-content ()
  (should (equal (yaml-ts-mode-test--indent
                  (concat "a:\n    b: |\n      line\n        deeper\n"
                          "    c: plain\n       more\n"
                          "    d: \"quoted\n        text\"\n")
                  2)
                 (concat "a:\n  b: |\n    line\n      deeper\n"
                         "  c: plain\n     more\n"
                         "  d: \"quoted\n      text\"\n")))
  (pcase-dolist (`(,source ,expected)
                 '(("a:\n     b: \"x\n    y\"\n" "a:\n  b: \"x\n    y\"\n")
                   ("k: |2\n    x\n" "k: |2\n    x\n")))
    (ert-info ((format "%S" source))
      (should (equal (yaml-ts-mode-test--indent source 2) expected)))))

(ert-deftest yaml-ts-mode-preserves-scalar-blank-line-content ()
  (dolist (source '("key: |\n  first\n    \n  last\n"
                    "key: \"first\n    \n  last\"\n"))
    (with-temp-buffer
      (insert source)
      (yaml-ts-mode)
      (goto-char (yaml-ts-mode-test--position "    \n"))
      (indent-according-to-mode)
      (should (equal (buffer-string) source)))))

(ert-deftest yaml-ts-mode-indents-comments-with-following-content ()
  (should (equal (yaml-ts-mode-test--indent
                  "a:\n# lead\n    b: 1\n        # before c\nc: [\n# inside\n  1\n  ]\n   # last\n"
                  2)
                 "a:\n  # lead\n  b: 1\n# before c\nc: [\n  # inside\n  1\n  ]\n   # last\n")))

(ert-deftest yaml-ts-mode-indents-new-lines ()
  (pcase-dolist (`(,source ,column)
                 '(("key:" 2) ("key: value" 0) ("- a" 0) ("-" 2) ("- key:" 4)
                   ("? " 2) ("? k\n:" 2) ("key: &a" 2) ("key: |" 2)
                   ("key: >-\n  text" 2) ("key: [" 2) ("- k: {a: 1," 4)
                   ("key: [a]" 0) ("--- !!map" 0) ("a:\n  b: 1 # c" 2)
                   ("a:\n  b:\n  # c" 4) ("[" 2) ("# c" 0)))
    (ert-info ((format "%S" source))
      (should (= (yaml-ts-mode-test--new-line-column source) column))))
  (with-temp-buffer
    (insert "a:\n  b: 1\n    \nc: 2\n")
    (yaml-ts-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) "a:\n  b: 1\n    \nc: 2\n"))
    (goto-char (yaml-ts-mode-test--position "    \n"))
    (indent-according-to-mode)
    (should (equal (buffer-string) "a:\n  b: 1\n  \nc: 2\n"))))

;;;; Updates

(ert-deftest yaml-ts-mode-refreshes-string-syntax-after-issue-repair ()
  (with-temp-buffer
    (insert "key: \"first\n  second\\q\"\ntail: plain`text\n")
    (yaml-ts-mode)
    (should (= (yaml-ts-mode-test--syntax-class "\"first") 1))
    (should (= (yaml-ts-mode-test--syntax-class "\"\ntail") 1))
    (should (= (yaml-ts-mode-test--syntax-class "`") 1))
    (goto-char (yaml-ts-mode-test--position "\\q"))
    (forward-char 1)
    (delete-char 1)
    (insert "n")
    (should (= (yaml-ts-mode-test--syntax-class "\"first") 15))
    (should (= (yaml-ts-mode-test--syntax-class "\"\ntail") 15))
    (yaml-ts-mode-test--should-match-fresh-buffer 3)
    (delete-char -1)
    (insert "q")
    (should (= (yaml-ts-mode-test--syntax-class "\"first") 1))
    (should (= (yaml-ts-mode-test--syntax-class "\"\ntail") 1))
    (yaml-ts-mode-test--should-match-fresh-buffer 3)))

(ert-deftest yaml-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("a: [1, 2] # c\n" "[1, 2]" "\"[1, 2]\"" "1" font-lock-string-face)
                   ("a: \"unfinished\n" "unfinished" "finished\"" "finished"
                    font-lock-string-face)
                   ("a: \"b\"\n" "\"b\"" "\"b" "b" font-lock-string-face)
                   ("a: b\n" "a: b" "a:\n  c: d" "c" font-lock-property-name-face)
                   ("a: |\n  x\n" "|" "x" "x" font-lock-string-face)))
    (ert-info ((format "%S: %S -> %S" source old new))
      (with-temp-buffer
        (insert source)
        (let ((treesit-font-lock-level 4)) (yaml-ts-mode))
        (yaml-ts-mode-test--buffer-state)
        (goto-char (yaml-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (yaml-ts-mode-test--face fragment) face))
        (yaml-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest yaml-ts-mode-preserves-syntax-when-narrowed ()
  (with-temp-buffer
    (insert "# head\na: \"# text\"\n# tail\n")
    (yaml-ts-mode)
    (narrow-to-region (yaml-ts-mode-test--position "a:") (point-max))
    (syntax-propertize (point-max))
    (should (nth 3 (syntax-ppss (yaml-ts-mode-test--position "text"))))
    (should-not (nth 4 (syntax-ppss (yaml-ts-mode-test--position "text"))))
    (should (nth 4 (syntax-ppss (yaml-ts-mode-test--position "tail"))))
    (widen)
    (should (nth 4 (syntax-ppss (yaml-ts-mode-test--position "head"))))))

(provide 'yaml-ts-mode-test)

;;; yaml-ts-mode-test.el ends here
