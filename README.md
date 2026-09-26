# yaml-ts-mode

[![CI](https://github.com/konomanoasa/yaml-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/yaml-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
YAML Ain't Markup Language 1.2.2.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/yaml-ts-mode")
```

## Automatic Activation

Enabled for `.yaml`, `.yml`, `.clangd`, and `.clang-format` files.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                              |
| ----- | -------------------------------------- |
| 1     | Comments                               |
| 2     | Keys, strings, tags, and directives    |
| 3     | Anchors, aliases, numbers, and escapes |
| 4     | Punctuation and brackets               |

## Grammar

[tree-sitter-yaml](https://github.com/konomanoasa/tree-sitter-yaml)

## License

[MIT](LICENSE)
