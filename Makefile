PERL6=alpha

SOURCES=lib/Yapsi.pm

PIRS=$(SOURCES:.pm=.pir)

all: $(PIRS)

%.pir: %.pm
	env PERL6LIB=`pwd`/lib $(PERL6) --target=pir --output=$@ $<

clean:
	rm -f $(PIRS)

test: all
	env PERL6LIB=`pwd`/lib prove -e '$(PERL6)' -r --nocolor t/
