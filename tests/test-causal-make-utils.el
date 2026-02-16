;;; test-causal-make-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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
(require 'causal-make-test-utils)
(require 'causal-make-utils)

(ert-deftest test-causal-make-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-make-unicode-get :previous) "Previous"))
    (should (string-equal (causal-make-unicode-get :next) "Next")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-make-unicode-get :previous) "↑"))
    (should (string-equal (causal-make-unicode-get :next) "↓"))))


(ert-deftest test-causal-make-mode-select-tmenu ()
  (let ((tmpfile "causal-make-mode-select-tmenu.txt"))
    (causalt-make-setup)
    (cl-letf ((causalt-mock #'makefile-automake-mode)
              (causalt-mock #'makefile-bsdmake-mode)
              (causalt-mock #'makefile-gmake-mode)
              (causalt-mock #'makefile-imake-mode)
              (causalt-mock #'makefile-mode)
              (causalt-mock #'makefile-makepp-mode))

      (let ((test-vectors
             '((:binding "a" :command makefile-automake-mode)
               (:binding "b" :command makefile-bsdmake-mode)
               (:binding "g" :command makefile-gmake-mode)
               (:binding "i" :command makefile-imake-mode)
               (:binding "m" :command makefile-mode)
               (:binding "e" :command makefile-makepp-mode))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-make-mode-select-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-make-breakdown)))


(ert-deftest test-causal-make--autovar-description-map ()
  (let ((avmap causal-make--autovar-description-map))
    (should (string= (map-elt avmap "$@") "The target name."))
    (should (string= (map-elt avmap "$(@D)") "The directory part of the target."))
    (should (string= (map-elt avmap "$(@F)") "The file-within-directory part of the target."))
    (should (string= (map-elt avmap "$*") "Stem with which an implicit rule matches."))
    (should (string= (map-elt avmap "$(*D)") "Directory part of stem."))
    (should (string= (map-elt avmap "$(*F)") "File-within-directory part of stem."))
    (should (string= (map-elt avmap "$%") "The target member name, when the target is an archive member."))
    (should (string= (map-elt avmap "$(%D)") "The directory part of the target archive member name."))
    (should (string= (map-elt avmap "$(%F)") "The file-within-directory part of the target archive member name."))
    (should (string= (map-elt avmap "$<") "The name of the first prerequisite."))
    (should (string= (map-elt avmap "$(<D)") "The directory part of the first prerequisite."))
    (should (string= (map-elt avmap "$(<F)") "The file-within-directory part of the first prerequisite."))
    (should (string= (map-elt avmap "$?") "The names of all the prerequisites that are newer than the target."))
    (should (string= (map-elt avmap "$(?D)") "Lists of the directory parts of all prerequisites newer than the target."))
    (should (string= (map-elt avmap "$(?F)") "Lists of the file-within-directory parts of all prerequisites newer than the target."))
    (should (string= (map-elt avmap "$^") "The names of all prerequisites, normalized (de-duplicated)."))
    (should (string= (map-elt avmap "$(^D)") "Lists of the directory parts of all prerequisites, normalized."))
    (should (string= (map-elt avmap "$(^F)") "Lists of the file-within-directory parts of all prerequisites, normalized."))
    (should (string= (map-elt avmap "$+") "The names of all prerequisites, allowing duplicates."))
    (should (string= (map-elt avmap "$(+D)") "Lists of the directory parts of all prerequisites, allowing duplicates."))
    (should (string= (map-elt avmap "$(+F)") "Lists of the file-within-directory parts of all prerequisites, allowing duplicates."))))


(provide 'test-causal-make-utils)
;;; test-causal-make-utils.el ends here
