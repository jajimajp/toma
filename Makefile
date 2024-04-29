TOOL=toma
VERSION = 0.7+PARSABLE
all:
	ghc -O -Wall -Wno-unused-imports -o $(TOOL) Main

clean:
	rm -f *.hi *.o $(TOOL) 

archive:
	git archive --format=tar --prefix=$(TOOL)/ HEAD | gzip > $(TOOL)-$(VERSION).tgz

starexec:
	ghc -optl -fuse-ld=bfd -O -Wall -optl-static -o $(TOOL) Main

coco2022: starexec
	cp toma coco2022/Toma0.2/bin/toma
	cd coco2022/Toma0.2 && zip -r Toma0.2.zip *

casc2022: starexec
	cp toma casc2022/Toma0.2/bin/toma
	cd casc2022/Toma0.2 && zip -r Toma0.2.zip *
