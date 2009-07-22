#!/usr/bin/make -f

FILES  = debian/ghc6.postinst debian/ghc6.prerm debian/ghc6.postrm
FILES += debian/ghc6-doc.postinst debian/gen_contents_index debian/ghc6-doc.triggers
FILES += debian/ghc6-doc.preinst debian/ghc-pkg6.man

.PHONY: all clean

all: $(FILES)

%: %.in
	sed "s/@VERSION@/$(ProjectVersion)/" $< > $@

clean:
	rm -f $(FILES)
