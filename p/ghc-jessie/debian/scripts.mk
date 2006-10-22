#!/usr/bin/make -f

FILES = debian/ghc6.postinst debian/ghc6.prerm debian/ghc6.postrm \
		debian/flags.xsl

.PHONY: all clean

all: $(FILES)

%: %.in
	sed "s/@VERSION@/$(ProjectVersion)/" $< > $@

clean:
	rm -f $(FILES)
