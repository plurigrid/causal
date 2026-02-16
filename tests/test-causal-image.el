;;; test-causal-image.el --- Causal Image Tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'causal-image-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-image)

(ert-deftest test-causal-image-tmenu ()
  (let ((tmpfile "causal-image-tmenu.txt"))
    (causalt-image-setup)
    (cl-letf (((symbol-function #'causal-image--identify-label) (lambda () "some image info"))
              (causalt-mock #'image-increase-size)
              (causalt-mock #'image-decrease-size)
              (causalt-mock #'image-transform-reset-to-original)
              (causalt-mock #'image-transform-fit-to-window)
              (causalt-mock #'image-transform-set-rotation)
              (causalt-mock #'image-transform-set-percent)
              (causalt-mock #'causal-image--reset-point)

              (causalt-mock #'image-crop)
              (causalt-mock #'image-cut)
              (causalt-mock #'save-buffer)
              (causalt-mock #'image-save)
              (causalt-mock #'rename-visited-file)
              (causalt-mock #'revert-buffer)

              (causalt-mock #'image-previous-line)
              (causalt-mock #'image-next-line)
              (causalt-mock #'image-backward-hscroll)
              (causalt-mock #'image-forward-hscroll)

              (causalt-mock #'image-bol)
              (causalt-mock #'image-eol)
              (causalt-mock #'image-bob)
              (causalt-mock #'image-eob)

              (causalt-mock #'image-previous-file)
              (causalt-mock #'image-next-file)
              (causalt-mock #'dired-jump-other-window)

              (causalt-mock #'image-mode-mark-file)
              (causalt-mock #'image-mode-unmark-file)
              (causalt-mock #'image-mode-copy-file-name-as-kill))

      (let ((test-vectors
             '((:binding "+" :command image-increase-size)
               (:binding "-" :command image-decrease-size)
               (:binding "o" :command image-transform-reset-to-original)
               (:binding "=" :command image-transform-fit-to-window)
               (:binding "R0" :command image-transform-set-rotation)
               (:binding "%50" :command image-transform-set-percent)
               (:binding "." :command causal-image--reset-point)

               (:binding "c" :command image-crop)
               (:binding "f" :command image-cut)
               (:binding "F" :command causal-image--customize-image-cut-color)
               (:binding "r" :command causal-image-resize-tmenu)
               ;; (:binding "s" :command save-buffer) ; TODO: handle buffer-modified-p
               (:binding "C-s" :command image-save)
               (:binding "M-r" :command rename-visited-file)
               (:binding "g" :command revert-buffer)


               (:binding "<up>" :command image-previous-line)
               (:binding "<down>" :command image-next-line)
               (:binding "<left>" :command image-backward-hscroll)
               (:binding "<right>" :command image-forward-hscroll)

               (:binding "a" :command image-bol)
               (:binding "e" :command image-eol)
               (:binding "<" :command image-bob)
               (:binding ">" :command image-eob)

               (:binding "p" :command image-previous-file)
               (:binding "n" :command image-next-file)
               (:binding "d" :command dired-jump-other-window)

               (:binding "m" :command image-mode-mark-file)
               (:binding "u" :command image-mode-unmark-file)

               (:binding "w" :command image-mode-copy-file-name-as-kill)

               (:binding "I" :command causal-image--indentify-verbose)
               (:binding "," :command causal-image-settings-tmenu))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-image-tmenu
                                        '(lambda () (random 5000)))))

    (cl-letf (((symbol-function #'causal-image--identify-label) (lambda () "some image info"))
              ((symbol-function #'buffer-modified-p) (lambda () t))
              (causalt-mock #'save-buffer))

      (let ((test-vectors
             '((:binding "s" :command save-buffer))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-image-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-image-breakdown)))



(provide 'test-causal-image)
;;; test-causal-image.el ends here
