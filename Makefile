SC=scalac
SOURCES=Wator.scala FishGrid.scala FishDisplay.scala package.scala RunWator.scala

all: $(SOURCES)
	$(SC) $(SOURCES)
