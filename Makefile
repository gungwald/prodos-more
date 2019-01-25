# Require-mints:
#
#    GNU make 
#        - To execute this make file
#
#    merlin32
#        - to assemble the source code
#          https://www.brutaldeluxe.fr/products/crossdevtools/merlin/
#
#    AppleCommander (included) 
#        - To create the Apple II disk image
#
#    AppleWin
#        - To load the disk and test the program
#
ifeq ($(OS),Windows_NT)
    COPY=copy
    APPLEWIN="c:\opt\AppleWin1.26.2.3\applewin.exe"
else
    COPY=cp
    APPLEWIN=applewin
endif

# It is necessary to use this older version of AppleCommander to support
# the PowerBook G4 and iBook G3. This version only requires Java 1.3.
AC=java -jar AppleCommander-1.3.5-ac.jar
SRC=more.s
PGM=more
BASE_DSK=prodos-2.0.3-boot.dsk
VOL=$(PGM)
DSK=$(PGM).dsk

# There is some kind of problem with turning this into a boot disk
# after it is created by AppleCommander. So, copy an existing boot
# disk instead.
#$(AC) -pro140 $(DSK) $(VOL)

$(DSK): $(PGM)
	$(COPY) $(BASE_DSK) $(DSK)
	$(AC) -p $(DSK) $(PGM) BIN 0x0800 < $(PGM)

$(PGM): $(SRC) Makefile
	merlin32 --verbose $(SRC)

clean:
	$(RM) $(DSK) $(PGM) *.zip _FileInformation.txt $(PGM)_Output.txt

test: $(DSK)
	$(APPLEWIN) -d1 $(DSK)

