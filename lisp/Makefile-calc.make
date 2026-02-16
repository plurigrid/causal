##
# Copyright (C) 2024-2025 Charles Y. Choi
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
include Makefile--defines.make

PACKAGE_NAME=causal-calc

ELISP_INCLUDES =				\
causal-calc--calc.el				\
causal-calc-utils.el				\
causal-calc-algebra.el				\
causal-calc-predicates.el			\
causal-calc-labels.el				\
causal-calc-fileio.el				\
causal-calc-radix.el				\
causal-calc-angle-measure.el			\
causal-calc-stack.el				\
causal-calc-variables.el			\
causal-calc-graphics.el

ELISP_PACKAGES=					\
causal-calc-binary.el				\
causal-calc-complex.el				\
causal-calc-conversion.el			\
causal-calc-logarithmic.el			\
causal-calc-random.el				\
causal-calc-rounding.el				\
causal-calc-settings.el				\
causal-calc-time.el				\
causal-calc-trail.el				\
causal-calc-trigonometric.el			\
causal-calc-units.el				\
causal-calc-vector.el				\
causal-calc-financial.el			\
causal-calc-symbolic.el

ELISP_TEST_INCLUDES=causal-calc-test-utils.el

PACKAGE_PATHS=					\
-L $(EMACS_ELPA_DIR)/compat-current		\
-L $(EMACS_ELPA_DIR)/seq-current		\
-L $(EMACS_ELPA_DIR)/transpose-frame-current	\
-L $(EMACS_ELPA_DIR)/transient-current		\
-L $(EMACS_ELPA_DIR)/cond-let-current		\
-L $(EMACS_ELPA_DIR)/magit-current		\
-L $(EMACS_ELPA_DIR)/magit-section-current	\
-L $(EMACS_ELPA_DIR)/dash-current		\
-L $(EMACS_ELPA_DIR)/with-editor-current	\
-L $(EMACS_ELPA_DIR)/symbol-overlay-current	\
-L $(CASUAL_LIB_LISP_DIR)


include Makefile--rules.make
