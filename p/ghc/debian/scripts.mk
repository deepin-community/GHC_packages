#!/usr/bin/make -f

FILES  = debian/ghc.postinst debian/ghc.prerm
FILES += debian/ghc-doc.postinst debian/gen_contents_index debian/ghc-doc.triggers
FILES += debian/ghc-doc.preinst debian/ghc-pkg.man

.PHONY: all clean

all: $(FILES)

%: %.in
	sed "s/@VERSION@/$(ProjectVersion)/" $< > $@

clean:
	rm -f $(FILES)

