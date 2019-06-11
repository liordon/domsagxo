#!/bin/sh

successFlag=0

gitRoot=`git rev-parse --show-toplevel`
sourceDir="$gitRoot/esperanto/src/"
testDir="$gitRoot/esperanto/test/"
if [ -z "$PYTHONPATH" ] ; then
	PYTHONPATH="$sourceDir;$testDir"
else
	PYTHONPATH="$PYTHONPATH;$sourceDir;$testDir"
fi
export PYTHONPATH
echo "python path is: $PYTHONPATH"

astPath="$sourceDir/compilation/abstract_syntax_tree.py"

doubleMethods=`grep "def " $astPath | cut -d"(" -f1 | sort | uniq -c | grep -v " 1 " | wc -l`

if [[ $doubleMethods -ne 0 ]] ; then
	echo "you have several methods with the same name in your AST file:"
	echo "$astPath"
	grep "def " $astPath | cut -d"(" -f1 | sort | uniq -c | grep -v " 1 " 
fi

git stash save --keep-index --include-untracked "set aside untracked changes"

python -m pytest --timeout=2 ${testDir}
pytestRes="$?"


if [[ $pytestRes -ne 0 ]] ; then
	echo "some of your tests failed! :("
	successFlag=$pytestRes
fi

if [[ $successFlag -ne 0 ]] ; then
	echo "YOU SHALL NOT COMMIT!!!"
	git stash pop
	exit $successFlag
fi

git stash pop
exit 0
