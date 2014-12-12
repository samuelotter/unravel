OUTDIR=js
TARGET=${OUTDIR}/unravel.js

.PHONY: compile

all: compile

compile: ${OUTDIR} ${TARGET}

clean:
	rm ${TARGET}

${OUTDIR}:
	mkdir -p ${OUTDIR}

${TARGET}: *.elm
	elm-make $< --output ${TARGET}
