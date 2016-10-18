#!/bin/bash
#
total=$2
index=$1

errorcounter() {
	(( errcount++ ))
}

trap errorcounter ERR

: ${EVMS:="24.1 24.2 24.3 24.4 24.5 25.1 git-snapshot"}
evms=$(echo $EVMS | tr ' ' '\n' | awk "NR % $total == $index")
export EMACS=evm-emacs
for ever in $evms; do
	evm install --use emacs-${ever}-travis
	make build
	mkdir -p ${CIRCLE_TEST_REPORTS:-reports}/$ever
	make report JUNIT=${CIRCLE_TEST_REPORTS:-reports}/$ever/junit.xml
done

exit ${errcount:-0}
