#!/bin/bash
#
#total=$2
#index=$1
errcount=0

errorcounter() {
	(( errcount++ ))
}

trap errorcounter ERR

: ${EVERS:="24.3 24.4 24.5 25.1 25.2"}
for ever in $EVERS; do
	echo Testing GNU Emacs $ever
	echo "Run 'make build'"
	EMACS=/opt/emacs/$ever/bin/emacs make build
	reportdir=${CIRCLE_TEST_REPORTS:-reports}/$ever
	[ -d $reportdir ] || mkdir -p $reportdir
	reportfile=$reportdir/junit.xml
	echo "Run 'make report', writing report to $reportfile"
	make report OUTER="Emacs ${ever}" JUNIT=$reportfile
done

exit ${errcount:-0}
