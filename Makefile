# Makefile --- GNU Makefile to build guile modules

# Copyright © 2017 Alex Kost <alezost@gmail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# The only purpose of this Makefile is to build .scm files to check them
# for potential errors.

MODULES_DIR = modules

MODULES =					\
  $(MODULES_DIR)/daemon-config.scm		\
  $(MODULES_DIR)/daemon-config/osd.scm		\
  $(MODULES_DIR)/daemon-config/osd/global.scm	\
  $(MODULES_DIR)/daemon-config/osd/text.scm	\
  $(MODULES_DIR)/daemon-config/osd/sound.scm	\
  $(MODULES_DIR)/daemon-config/osd/clock.scm	\
  $(MODULES_DIR)/daemon-config/osd/sleep.scm	\
  $(MODULES_DIR)/daemon-config/lirc/client.scm	\
  $(MODULES_DIR)/daemon-config/lirc/keys.scm

GO_FILES = $(MODULES:%.scm=%.go)

GUILEC_ENV =					\
  GUILE_AUTO_COMPILE=0

GUILEC_OPTS =					\
  -Warity-mismatch				\
  -Wformat					\
  -Wunbound-variable

GUILEC_ENV +=								\
  GUILE_LOAD_PATH="$(MODULES_DIR):$$GUILE_LOAD_PATH"			\
  GUILE_LOAD_COMPILED_PATH="$(MODULES_DIR):$$GUILE_LOAD_COMPILED_PATH"

all: $(GO_FILES)

$(GO_FILES): %.go: %.scm
	@$(GUILEC_ENV) guild compile $(GUILEC_OPTS) --output=$@ $<

clean:
	$(RM) -f $(GO_FILES)

.PHONY: clean

# Makefile ends here
