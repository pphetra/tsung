#### CONFIGURE VARIABLE

# export ERLC_EMULATOR to fix a bug in R9B with native compilation
ERLC_EMULATOR=/usr/local/bin/erl
export ERLC_EMULATOR
ERL=/usr/local/bin/erl
ERLC=/usr/local/bin/erlc
SED=/usr/bin/sed
ERL_OPTS=
# FIXME
DIALYZER=dialyzer

ERLDIR=/usr/local/Cellar/erlang/R15B/lib/erlang
export ERLDIR

USEMOCHIWEBLIBS=yes

ERLANG_XMERL_DIR=/usr/local/Cellar/erlang/R15B/lib/erlang/lib/xmerl-1.3/include

raw_erlang_prefix=${exec_prefix}/lib/erlang/

PACKAGE_TARNAME=tsung

prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
libdir=${exec_prefix}/lib
datadir=${datarootdir}
datarootdir=${prefix}/share
docdir=${datarootdir}/doc/${PACKAGE_TARNAME}
TEMPLATES_SUBDIR=tsung/templates

CONFIGURE_DEPENDENCIES=vsn.mk
CONFIG_STATUS_DEPENDENCIES=vsn.mk

VERSION=1.4.2
PACKAGE=tsung
DTD=tsung-1.0.dtd

#### END OF SUBSTITUTION

SVN_REVISION=$Revision$

ERL_COMPILER_OPTIONS="[warn_unused_vars]"
export ERL_COMPILER_OPTIONS

ifeq ($(TYPE),debug)
OPT =+debug_info -DDEBUG
else
 ifeq ($(TYPE),native)
   OPT:=+native
  else
   OPT = +strict_record_tests
  endif
endif

ifeq ($(TYPE),test)
OPT =+export_all
endif


INC = ./include
CC  = $(ERLC)

ESRC = ./src
EBIN = ./ebin
ifeq ($(TYPE),snapshot)
DAY=$(shell date +"%Y%m%d")
distdir = $(PACKAGE)-$(VERSION)-$(DAY)
else
distdir = $(PACKAGE)-$(VERSION)
endif

# installation path
BINDIR    = $(bindir)
LIBDIR    = $(libdir)/tsung/
TOOLS_BINDIR = $(LIBDIR)/bin
CONFDIR   = $(docdir)/examples
SHARE_DIR = $(datadir)/tsung/
TEMPLATES_DIR = $(datadir)/$(TEMPLATES_SUBDIR)
MAN_DIR   = $(datadir)/man/man1/
DOC_DIR   = $(docdir)

# custom behaviours
BEHAVIORS  = ebin/ts_plugin.beam

BUILDER_LOG   = /tmp/builder-tsung.log

ERLANG_LIB_DIR = $(libdir)/erlang/lib

APPLICATION = tsung
CONTROLLER_APPLICATION = tsung_controller
RECORDER_APPLICATION = tsung_recorder

RECORDER_TARGETDIR = $(ERLANG_LIB_DIR)/$(RECORDER_APPLICATION)-$(VERSION)
CONTROLLER_TARGETDIR = $(ERLANG_LIB_DIR)/$(CONTROLLER_APPLICATION)-$(VERSION)
TARGETDIR = $(ERLANG_LIB_DIR)/$(APPLICATION)-$(VERSION)

TEMPLATES = $(wildcard $(ESRC)/templates/*.thtml)
TEMPLATES += $(wildcard $(ESRC)/templates/*.js)
TMP       = $(wildcard *~) $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES = $(wildcard $(INC)/*.hrl)
LIBSRC    = $(wildcard $(ESRC)/lib/[^mochi]*.erl)
ifeq ($(USEMOCHIWEBLIBS),yes)
LIBSRC    += $(wildcard $(ESRC)/lib/mochi*.erl)
endif
TESTSRC    = $(wildcard $(ESRC)/test/*.erl)
SRC       = $(wildcard $(ESRC)/$(APPLICATION)/*.erl)
CONTROLLER_SRC  = $(wildcard $(ESRC)/$(CONTROLLER_APPLICATION)/*.erl)
RECORDER_SRC    = $(wildcard $(ESRC)/$(RECORDER_APPLICATION)/*.erl)
CONFFILE_SRC = $(wildcard examples/*.xml.in)
CONFFILE = $(basename $(CONFFILE_SRC))
TEST_CONFFILE_SRC = $(wildcard src/test/*.xml.in)
TEST_CONFFILE = $(basename $(TEST_CONFFILE_SRC))
USERMANUAL = doc/user_manual.html  doc/IDXDOC.css
USERMANUAL_IMG = $(wildcard doc/images/*.png)
USERMANUAL_SRC = doc/user_manual.tex
MANPAGES   = $(wildcard doc/*.1)
PERL_SCRIPTS_SRC = $(wildcard $(ESRC)/*.pl.in)
PERL_SCRIPTS = $(basename $(PERL_SCRIPTS_SRC))

TSPLOT_SRC = $(wildcard $(ESRC)/tsung-plotter/*.py.in)
TSPLOT = $(basename $(TSPLOT_SRC))
TSUNG_PLOTTER_LIB= $(wildcard $(ESRC)/tsung-plotter/tsung/*.py)
TSUNG_PLOTTER_CONF= $(wildcard $(ESRC)/tsung-plotter/tsung/*.conf) $(wildcard $(ESRC)/tsung-plotter/*.conf)

TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(SRC)))))
LIB_TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(LIBSRC)))))
CONTROLLER_TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(CONTROLLER_SRC)))))
RECORDER_TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(RECORDER_SRC)))))
TEST_TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(TESTSRC)))))
DEBIAN    = debian/changelog debian/control debian/compat debian/copyright debian/docs debian/tsung.dirs debian/rules

SRC_APPFILES   = $(ESRC)/$(APPLICATION)/$(APPLICATION).app.src $(ESRC)/$(APPLICATION)/$(APPLICATION).rel.src
SRC_APPFILES_IN   = $(ESRC)/$(APPLICATION)/$(APPLICATION).app.src.in $(ESRC)/$(APPLICATION)/$(APPLICATION).rel.src
CONTROLLER_SRC_APPFILES   = $(ESRC)/$(CONTROLLER_APPLICATION)/$(CONTROLLER_APPLICATION).app.src $(ESRC)/$(CONTROLLER_APPLICATION)/$(CONTROLLER_APPLICATION).rel.src
CONTROLLER_SRC_APPFILES_IN   = $(ESRC)/$(CONTROLLER_APPLICATION)/$(CONTROLLER_APPLICATION).app.src.in $(ESRC)/$(CONTROLLER_APPLICATION)/$(CONTROLLER_APPLICATION).rel.src
RECORDER_SRC_APPFILES   = $(ESRC)/$(RECORDER_APPLICATION)/$(RECORDER_APPLICATION).app.src $(ESRC)/$(RECORDER_APPLICATION)/$(RECORDER_APPLICATION).rel.src
RECORDER_SRC_APPFILES_IN   = $(ESRC)/$(RECORDER_APPLICATION)/$(RECORDER_APPLICATION).app.src.in $(ESRC)/$(RECORDER_APPLICATION)/$(RECORDER_APPLICATION).rel.src
TGT_APPFILES_E = $(EBIN)/$(APPLICATION).app
CONTROLLER_TGT_APPFILES_E = $(EBIN)/$(CONTROLLER_APPLICATION).app
RECORDER_TGT_APPFILES_E = $(EBIN)/$(RECORDER_APPLICATION).app
TGT_APPFILES_P = priv/$(APPLICATION)*
RECORDER_TGT_APPFILES_P = priv/$(RECORDER_APPLICATION)*
CONTROLLER_TGT_APPFILES_P = priv/$(CONTROLLER_APPLICATION)*

SCRIPT   = $(BINDIR)/tsung
REC_SCRIPT   = $(BINDIR)/tsung-recorder
PWD = $(shell pwd)
BUILD_OPTIONS = '[{systools, \
        [{variables,[ \
         {"TSUNGPATH", "$(PWD)/temp/"}] \
        }]}, \
        {sh_script, none}, \
        {make_app, true }, {make_rel, true}].'

BUILD_OPTIONS_DOT = $(subst $(PWD)/temp/,./,$(BUILD_OPTIONS))

BUILD_OPTIONS_FILE = ./BUILD_OPTIONS

DIST_COMMON=Makefile.in $(CONFFILE_SRC) $(PERL_SCRIPTS_SRC) $(TSPLOT_SRC) tsung.sh.in tsung-recorder.sh.in tsung.spec.in $(CONTROLLER_SRC_APPFILES_IN)

DOC_OPTS={def,{version,\"$(VERSION)\"}}

.PHONY: doc

tsung: Makefile config.status $(PERL_SCRIPTS)  $(TSPLOT) tsung.sh tsung-recorder.sh tsung.spec $(TARGET) $(RECORDER_TARGET) $(CONTROLLER_TARGET) $(LIB_TARGET)  $(CONTROLLER_SRC_APPFILES) $(SRC_APPFILES) $(RECORDER_SRC_APPFILES)

buildtest: $(TEST_TARGET)

fulltest:  clean test

test:
	$(MAKE) TYPE=test dotest

dotest: tsung buildtest $(CONFFILE) $(TEST_CONFFILE)
	$(ERL) -noshell -pa ./ebin -s eunit test ts_test_all -s init stop

edoc:
	$(ERL) -noshell -eval "edoc:application($(APPLICATION), \"./$(ESRC)/$(APPLICATION)\", [$(DOC_OPTS)])" -s init stop
	$(ERL) -noshell -eval "edoc:application($(CONTROLLER_APPLICATION), \
          \"./$(ESRC)/$(CONTROLLER_APPLICATION)\", [$(DOC_OPTS)])" -s init stop
	$(ERL) -noshell -eval "edoc:application($(RECORDER_APPLICATION),  \
          \"./$(ESRC)/$(RECORDER_APPLICATION)\", [$(DOC_OPTS)])" -s init stop

dialyzer:
	$(DIALYZER) -r ebin  -I ./include/

all: clean tsung

debug:
	$(MAKE) TYPE=debug

native:
	$(MAKE) TYPE=native

rpm:	release tsung.spec
	rpmbuild -ta $(distdir).tar.gz

validate: $(CONFFILE)
	@for i in $(CONFFILE); do xmlproc_val $$i; done

deb:
	fakeroot debian/rules clean
	debian/rules build
	fakeroot debian/rules binary

clean:
	-cd priv && rm -f $(shell ls priv | grep -v builder\.erl| grep -v CVS) && cd ..
	-rm -f $(TARGET) $(TMP) $(BUILD_OPTIONS_FILE) builder.beam
	-rm -f $(TGT_APPFILES) $(PERL_SCRIPTS) $(TSPLOT) $(CONFFILE)
	-rm -f ebin/*.beam tsung.sh tsung.spec tsung.xml tsung.sh tsung-recorder.sh
	-rm -f *.xml config.log src/test/*.xml src/test/usersdb.csv

install: doc boot install_recorder install_controller $(CONFFILE)
	-rm -f $(TMP)

	install -d $(DESTDIR)$(TARGETDIR)/priv
	install -d $(DESTDIR)$(TARGETDIR)/ebin
	install -d $(DESTDIR)$(TARGETDIR)/src
	install -d $(DESTDIR)$(TARGETDIR)/include
	install -d $(DESTDIR)$(TOOLS_BINDIR)/
	install -d $(DESTDIR)$(BINDIR)/

	install -m 0644 $(INC_FILES) $(DESTDIR)$(TARGETDIR)/include/
	install -m 0644 $(TARGET) $(DESTDIR)$(TARGETDIR)/ebin/
	install -m 0644 $(LIB_TARGET) $(DESTDIR)$(TARGETDIR)/ebin/
	install -m 0644 builder.beam $(DESTDIR)$(TARGETDIR)/ebin/


	install -m 0644 $(TGT_APPFILES_E) $(DESTDIR)$(TARGETDIR)/ebin/
	install -m 0644 $(TGT_APPFILES_P) $(DESTDIR)$(TARGETDIR)/priv/


	install -m 0644 $(SRC) $(SRC_APPFILES) $(DESTDIR)$(TARGETDIR)/src/
	echo $(BUILD_OPTIONS_DOT) > $(DESTDIR)$(TARGETDIR)/BUILD_OPTIONS


# install the man page & user's manual
	install -d $(DESTDIR)$(MAN_DIR)
	install -m 0644 $(MANPAGES) $(DESTDIR)$(MAN_DIR)
	install -d $(DESTDIR)$(DOC_DIR)/images
	install -m 0644 $(USERMANUAL) $(DESTDIR)$(DOC_DIR)
	install -m 0644 $(USERMANUAL_IMG) $(DESTDIR)$(DOC_DIR)/images

# create startup script
	install -m 0755 tsung.sh $(DESTDIR)$(SCRIPT)
	install -m 0755 tsung-recorder.sh $(DESTDIR)$(REC_SCRIPT)
	install -m 0755 $(PERL_SCRIPTS) $(DESTDIR)$(TOOLS_BINDIR)
# tsung-plotter
	install -m 0755 $(TSPLOT) $(DESTDIR)$(BINDIR)/tsplot
	install -d $(DESTDIR)$(LIBDIR)/tsung_plotter
	install -d $(DESTDIR)$(SHARE_DIR)/tsung_plotter
	install -m 0644 $(TSUNG_PLOTTER_LIB) $(DESTDIR)$(LIBDIR)/tsung_plotter
	install -m 0644 $(TSUNG_PLOTTER_CONF) $(DESTDIR)$(SHARE_DIR)/tsung_plotter

	install -d $(DESTDIR)$(CONFDIR)
	install -m 0644 $(CONFFILE) $(DESTDIR)$(CONFDIR)/

	install -d $(DESTDIR)$(TEMPLATES_DIR)
	install -m 0644 $(TEMPLATES) $(DESTDIR)$(TEMPLATES_DIR)/
	install -m 0644 $(DTD) $(DESTDIR)$(SHARE_DIR)/

install_recorder: boot
	install -d $(DESTDIR)$(RECORDER_TARGETDIR)/priv
	install -d $(DESTDIR)$(RECORDER_TARGETDIR)/ebin
	install -d $(DESTDIR)$(RECORDER_TARGETDIR)/src
	install -d $(DESTDIR)$(RECORDER_TARGETDIR)/include

	install -m 0644 $(INC_FILES) $(DESTDIR)$(RECORDER_TARGETDIR)/include
	install -m 0644 $(RECORDER_TARGET) $(DESTDIR)$(RECORDER_TARGETDIR)/ebin

	install -m 0644 $(RECORDER_TGT_APPFILES_E) $(DESTDIR)$(RECORDER_TARGETDIR)/ebin
	install -m 0644 $(RECORDER_TGT_APPFILES_P) $(DESTDIR)$(RECORDER_TARGETDIR)/priv

	install -m 0644 $(RECORDER_SRC) $(RECORDER_SRC_APPFILES) $(DESTDIR)$(RECORDER_TARGETDIR)/src
	@echo $(BUILD_OPTIONS_DOT) > $(DESTDIR)$(RECORDER_TARGETDIR)/BUILD_OPTIONS

install_controller: boot
	install -d $(DESTDIR)$(CONTROLLER_TARGETDIR)/priv
	install -d $(DESTDIR)$(CONTROLLER_TARGETDIR)/ebin
	install -d $(DESTDIR)$(CONTROLLER_TARGETDIR)/src
	install -d $(DESTDIR)$(CONTROLLER_TARGETDIR)/include
	install -m 0644 $(INC_FILES) $(DESTDIR)$(CONTROLLER_TARGETDIR)/include
	install -m 0644 $(CONTROLLER_TARGET) $(DESTDIR)$(CONTROLLER_TARGETDIR)/ebin

	install -m 0644 $(CONTROLLER_TGT_APPFILES_E) $(DESTDIR)$(CONTROLLER_TARGETDIR)/ebin
	install -m 0644 $(CONTROLLER_TGT_APPFILES_P) $(DESTDIR)$(CONTROLLER_TARGETDIR)/priv

	install -m 0644 $(CONTROLLER_SRC) $(CONTROLLER_SRC_APPFILES) $(DESTDIR)$(CONTROLLER_TARGETDIR)/src
	@echo $(BUILD_OPTIONS_DOT) > $(DESTDIR)$(CONTROLLER_TARGETDIR)/BUILD_OPTIONS

uninstall:
	rm -rf $(TARGETDIR) $(SCRIPT)

boot: tsung priv/tsung.boot priv/tsung_recorder.boot priv/tsung_controller.boot

priv/tsung.boot: builder.beam  $(SRC_APPFILES)
# use builder to make boot file
	@rm -rf temp_ts
	@mkdir -p temp_ts/lib/$(APPLICATION)-$(VERSION)/ebin
	@cp $(TARGET) $(LIB_TARGET) temp_ts/lib/$(APPLICATION)-$(VERSION)/ebin
	@ln -sf $(PWD)/src/$(APPLICATION) temp_ts/lib/$(APPLICATION)-$(VERSION)/src
	@ln -sf $(PWD)/include temp_ts/lib/$(APPLICATION)-$(VERSION)/include
	@ln -sf $(PWD)/priv temp_ts/lib/$(APPLICATION)-$(VERSION)/priv
	@ln -sf $(PWD)/builder.beam temp_ts/lib/$(APPLICATION)-$(VERSION)/
	@ln -sf $(PWD) temp_ts/lib/$(APPLICATION)-$(VERSION)
	@echo -n "build main app boot script ... "
	@(cd temp_ts/lib/$(APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && $(ERL) -noshell -s builder go -s init stop >> $(BUILDER_LOG) 2>&1 \
	)
	@cp temp_ts/lib/$(APPLICATION)-$(VERSION)/ebin/*.app ebin
	@rm -rf temp_ts
	@echo "done"

priv/tsung_controller.boot: builder.beam $(CONTROLLER_SRC_APPFILES)
# use builder to make boot file
	@rm -rf temp_tsc
	@mkdir -p temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/ebin
	@cp $(CONTROLLER_TARGET) temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/ebin
	@ln -sf $(PWD)/src/$(CONTROLLER_APPLICATION) temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/src
	@ln -sf $(PWD)/include temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/include
	@ln -sf $(PWD)/priv temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/priv
	@ln -sf $(PWD)/builder.beam temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/
	@echo -n "build controller boot script ... "
	@(cd temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && $(ERL) -noshell -s builder go -s init stop >> $(BUILDER_LOG) 2>&1 \
	)
	@cp temp_tsc/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/ebin/*.app ebin
	@rm -rf temp_tsc
	@echo "done"

priv/tsung_recorder.boot: builder.beam $(RECORDER_SRC_APPFILES)
# use builder to make boot file
	@rm -rf temp_tsr
	@mkdir -p temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/ebin
	@cp $(RECORDER_TARGET) temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/ebin
	@ln -sf $(PWD)/src/$(RECORDER_APPLICATION) temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/src
	@ln -sf $(PWD)/include temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/include
	@ln -sf $(PWD)/priv temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/priv
	@ln -sf $(PWD)/builder.beam temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/
	@echo -n "build recorder boot script ... "
	@(cd temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && $(ERL) -noshell -s builder go -s init stop  >> $(BUILDER_LOG) 2>&1 \
	)
	@cp temp_tsr/lib/$(RECORDER_APPLICATION)-$(VERSION)/ebin/*.app ebin
	@rm -rf temp_tsr
	@echo "done"


Makefile: Makefile.in config.status
	@$(SHELL) ./config.status --file=$@

%.pl:  %.pl.in vsn.mk
	@$(SHELL) ./config.status --file=$@

%.py:  %.py.in vsn.mk
	@$(SHELL) ./config.status --file=$@

%.spec:  %.spec.in vsn.mk
	@$(SHELL) ./config.status --file=$@

%.xml:  %.xml.in
	@$(SHELL) ./config.status --file=$@

%.sh :%.sh.in vsn.mk
	@$(SHELL) ./config.status --file=$@

%.app.src: %.app.src.in
	@$(SHELL) ./config.status --file=$@

config.status: configure $(CONFIG_STATUS_DEPENDENCIES)
	$(SHELL) ./config.status --recheck


configure: configure.in $(CONFIGURE_DEPENDENCIES)
	@echo "running autoconf"
	@autoconf

doc:
	$(MAKE) -C doc


release: Makefile tsung.spec doc
	rm -fr $(distdir)
	mkdir -p $(distdir)
	tar zcf tmp.tgz $(SRC) $(SRC_APPFILES_IN) $(INC_FILES) $(LIBSRC) \
		$(CONTROLLER_SRC) $(CONTROLLER_SRC_APPFILES_IN) $(TESTSRC) \
		$(RECORDER_SRC_APPFILES_IN) \
		$(RECORDER_SRC) $(RECORDER_SRC_APPFILES) $(TEMPLATES) \
		doc/*.erl doc/*.txt doc/*.dia doc/*.png doc/Makefile doc/*.sgml doc/*.1 \
		$(USERMANUAL) $(USERMANUAL_SRC) $(USERMANUAL_IMG) $(DTD) \
		COPYING README LISEZMOI TODO $(CONFFILE_SRC) $(TEST_CONFFILE_SRC) \
		priv/builder.erl tsung.sh.in vsn.mk  src/test/*.csv src/test/*.txt \
		src/test/*.out \
		$(DEBIAN)  $(PERL_SCRIPTS_SRC) CONTRIBUTORS CHANGES \
		$(TSPLOT_SRC) $(TSUNG_PLOTTER_CONF) $(TSUNG_PLOTTER_LIB)\
		configure configure.in config.guess *.m4 config.sub Makefile.in \
		install-sh tsung.spec.in tsung.spec tsung-recorder.sh.in
	tar -C $(distdir) -zxf tmp.tgz
	mkdir $(distdir)/ebin
	tar zvcf  $(distdir).tar.gz $(distdir)
	rm -fr $(distdir)
	rm -fr tmp.tgz

snapshot:
	$(MAKE) TYPE=snapshot release

builder.beam: priv/builder.erl
	@$(CC) -W0 $(OPT) -I $(INC) $<

ebin/%.beam: src/test/%.erl $(INC_FILES)
	@echo  "Compiling test $< ... "
	@$(CC) -W0 $(OPT) -I $(INC) -I $(ERLANG_XMERL_DIR) -o ebin $<

ebin/%.beam: src/lib/%.erl $(INC_FILES)
	@echo  "Compiling  $< ... "
	@$(CC) -W0 $(OPT) -I $(INC) -I $(ERLANG_XMERL_DIR) -o ebin $<

# to avoid circular dependency
ebin/ts_plugin.beam: src/$(APPLICATION)/ts_plugin.erl $(INC_FILES)
	@echo  "Compiling  $< ... "
	@$(CC) $(OPT) -I $(INC) -I $(ERLANG_XMERL_DIR) -pa ebin -o ebin $<

ebin/%.beam: src/$(APPLICATION)/%.erl $(INC_FILES) $(BEHAVIORS)
	@echo  "Compiling  $< ... "
	@$(CC) $(OPT) -I $(INC) -I $(ERLANG_XMERL_DIR) -pa ebin -o ebin $<

ebin/%.beam: src/$(RECORDER_APPLICATION)/%.erl  $(INC_FILES) $(BEHAVIORS)
	@echo  "Compiling  $< ... "
	@$(CC) $(OPT) -I $(INC) -I $(ERLANG_XMERL_DIR) -pa ebin -o ebin $<

ebin/%.beam: src/$(CONTROLLER_APPLICATION)/%.erl  $(INC_FILES) $(BEHAVIORS)
	@echo "Compiling  $< ... "
	@$(CC) $(OPT) -I $(INC) -I $(ERLANG_XMERL_DIR) -pa ebin -o ebin $<

%:%.sh
# Override makefile default implicit rule
