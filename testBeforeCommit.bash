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

git stash save --keep-index --include-untracked "set aside untracked changes"

python -m pytest --timeout=2 ${testDir}
pytestRes="$?"

python ${sourceDir}/compilation/abstract_syntax_tree.py -c
conflicts=`grep "reduce/reduce" ${sourceDir}/compilation/parser.out`

if [ ! -z "$conflicts" ]; then
        echo "you have reduce/reduce conflicts! X_X"
	echo $conflicts
	successFlag=1
fi

if [[ $pytestRes -ne 0 ]] ; then
	echo "some of your tests failed! :("
	successFlag=$pytestRes
fi

if [[ $successFlag -ne 0 ]] ; then
	echo "YOU SHALL NOT COMMIT!!!"
	git stash pop
	exit $successFlag
fi

python $sourceDir/scripts/extract_current_rules.py
git add $gitRoot/Manuscripts/MscThesis/raw-grammar-rules.tex

git stash pop
exit 0
