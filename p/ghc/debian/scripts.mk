#!/usr/bin/make -f

include ghc/mk/version.mk

FILES = debian/ghc6.postinst debian/ghc6.prerm debian/ghc6.postrm \
		debian/flags.xsl

.PHONY: all clean

all: $(FILES)

%: %.in
	sed "s/@VERSION@/$(ProjectVersion)/" $< > $@

clean:
	rm -f $(FILES)

