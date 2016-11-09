#!/bin/bash
#
total=$2
index=$1
errcount=0

errorcounter() {
	(( errcount++ ))
}

trap errorcounter ERR

# : ${EVMS:="24.3 24.4 24.5 25.1 git-snapshot"}
: ${EVMS:="24.3 24.4 24.5 25.1"}
evms=$(echo $EVMS | tr ' ' '\n' | awk "NR % $total == $index")
export EMACS=evm-emacs
for ever in $evms; do
	echo Switch to evm emacs $ever
	evm use emacs-${ever} || exit 2
	echo "Run 'make build'"
	make build
	reportdir=${CIRCLE_TEST_REPORTS:-reports}/$ever
	[ -d $reportdir ] || mkdir -p $reportdir
	export reportfile=$reportdir/junit.xml
	echo "Run 'make report', writing report to $reportfile"
	make report OUTER="Emacs ${ever}" JUNIT=$reportfile
done

exit ${errcount:-0}
