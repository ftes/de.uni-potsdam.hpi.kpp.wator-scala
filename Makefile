SC=scalac
SOURCES=Wator.scala FishGrid.scala package.scala

all: $(SOURCES)
	$(SC) $(SOURCES)
