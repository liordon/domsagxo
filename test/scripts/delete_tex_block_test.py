import pytest

from scripts.delete_tex_block import *
from scripts.delete_tex_block import detectBlockCloser

EXAMPLE_ONLY_START = 'hello. my name is inigo montoya'
EXAMPLE_OPENER = '\\begin{accusation}'
EXAMPLE_ONLY_BLOCK = 'you killed my father'
EXAMPLE_CLOSER = '\\end{accusation}'
EXAMPLE_ONLY_END = 'prepare to die'
EXAMPLE_PARTS = [EXAMPLE_ONLY_START, EXAMPLE_OPENER,
    EXAMPLE_ONLY_BLOCK, EXAMPLE_CLOSER, EXAMPLE_ONLY_END]
EXAMPLE_MULTILINE_BLOCK = '\n'.join(EXAMPLE_PARTS)
EXAMPLE_SINGLE_LINE_BLOCK = ' '.join(EXAMPLE_PARTS)
MISLEADING_LINE = 'this is my accusation'

BLOCK_TYPE = "accusation"


def test_canIdentifyBlockOpener():
    assert detectBlockOpener(BLOCK_TYPE, EXAMPLE_OPENER)


def test_doesNotRecognizeUnrelatedLineAsOpener():
    assert not detectBlockOpener(BLOCK_TYPE, EXAMPLE_ONLY_START)
    assert not detectBlockOpener(BLOCK_TYPE, MISLEADING_LINE)


def test_doesNotRecognizeCloserAsOpener():
    assert not detectBlockOpener(BLOCK_TYPE, EXAMPLE_CLOSER)


def test_canIdentifyBlockCloser():
    assert detectBlockCloser(BLOCK_TYPE, EXAMPLE_CLOSER)


def test_doesNotRecognizeUnrelatedLineAsCloser():
    assert not detectBlockCloser(BLOCK_TYPE, EXAMPLE_ONLY_START)
    assert not detectBlockCloser(BLOCK_TYPE, MISLEADING_LINE)


def test_doesNotRecognizeOpenerAsCloser():
    assert not detectBlockCloser(BLOCK_TYPE, EXAMPLE_OPENER)


def test_whenOnlyBlockIsGivenReturnsEmptyString():
    assert deleteBlockOf(BLOCK_TYPE, EXAMPLE_OPENER + EXAMPLE_CLOSER) == ""


def test_doesNotDeleteTextWithoutTexBlock():
    assert deleteBlockOf(BLOCK_TYPE, EXAMPLE_ONLY_START) == EXAMPLE_ONLY_START


def test_deletesOpenerContentAndCloserFromLine():
    cleared_text = deleteBlockOf(BLOCK_TYPE, EXAMPLE_SINGLE_LINE_BLOCK)
    assert EXAMPLE_OPENER not in cleared_text
    assert EXAMPLE_ONLY_BLOCK not in cleared_text
    assert EXAMPLE_CLOSER not in cleared_text


def test_doesNotDeleteTextSurroundingBlock():
    cleared_text = deleteBlockOf(BLOCK_TYPE, EXAMPLE_SINGLE_LINE_BLOCK)
    assert EXAMPLE_ONLY_START in cleared_text
    assert EXAMPLE_ONLY_END in cleared_text


def test_whenLineHasOpenerButNoCloserContentIsDeletedUntilItsEnd():
    cleared_text = deleteBlockOf(BLOCK_TYPE, ' '.join([EXAMPLE_ONLY_START, EXAMPLE_OPENER, EXAMPLE_ONLY_BLOCK]))
    assert cleared_text == EXAMPLE_ONLY_START + ' '


def test_whenLineHasCloserButNoOpenerContentIsDeletedFromTheStart():
    cleared_text = deleteBlockOf(BLOCK_TYPE, ' '.join([EXAMPLE_ONLY_BLOCK, EXAMPLE_CLOSER, EXAMPLE_ONLY_END]))
    assert cleared_text == ' ' + EXAMPLE_ONLY_END


def test_canDeleteBlockSpreadingMultipleLines():
    assert deleteBlockOf(BLOCK_TYPE, EXAMPLE_MULTILINE_BLOCK) == '\n\n'.join([EXAMPLE_ONLY_START, EXAMPLE_ONLY_END])


def test_canFilterEmptyLines():
    assert filterEmptyLines('\n\n'.join([EXAMPLE_ONLY_START, EXAMPLE_ONLY_END])) == '\n'.join(
        [EXAMPLE_ONLY_START, EXAMPLE_ONLY_END])


class TestBlockDeleter(object):
    @pytest.fixture
    def block_deleter(self):
        return BlockDeleter(BLOCK_TYPE)

    def test_canIdentifyBlockOpener(self, block_deleter):
        assert block_deleter.detectBlockOpener(EXAMPLE_OPENER)

    def test_doesNotRecognizeUnrelatedLineAsOpener(self, block_deleter):
        assert not block_deleter.detectBlockOpener(EXAMPLE_ONLY_START)
        assert not block_deleter.detectBlockOpener(MISLEADING_LINE)

    def test_doesNotRecognizeCloserAsOpener(self, block_deleter):
        assert not block_deleter.detectBlockOpener(EXAMPLE_CLOSER)

    def test_canIdentifyBlockCloser(self, block_deleter):
        assert block_deleter.detectBlockCloser(EXAMPLE_CLOSER)

    def test_doesNotRecognizeUnrelatedLineAsCloser(self, block_deleter):
        assert not block_deleter.detectBlockCloser(EXAMPLE_ONLY_START)
        assert not block_deleter.detectBlockCloser(MISLEADING_LINE)

    def test_doesNotRecognizeOpenerAsCloser(self, block_deleter):
        assert not block_deleter.detectBlockCloser(EXAMPLE_OPENER)

    def test_whenOnlyBlockIsGivenReturnsEmptyString(self, block_deleter):
        assert block_deleter.deleteBlockFrom(EXAMPLE_OPENER + EXAMPLE_CLOSER) == ""

    def test_doesNotDeleteTextWithoutTexBlock(self, block_deleter):
        assert block_deleter.deleteBlockFrom(EXAMPLE_ONLY_START) == EXAMPLE_ONLY_START

    def test_deletesOpenerContentAndCloserFromLine(self, block_deleter):
        cleared_text = block_deleter.deleteBlockFrom(EXAMPLE_SINGLE_LINE_BLOCK)
        assert EXAMPLE_OPENER not in cleared_text
        assert EXAMPLE_ONLY_BLOCK not in cleared_text
        assert EXAMPLE_CLOSER not in cleared_text

    def test_doesNotDeleteTextSurroundingBlock(self, block_deleter):
        cleared_text = block_deleter.deleteBlockFrom(EXAMPLE_SINGLE_LINE_BLOCK)
        assert EXAMPLE_ONLY_START in cleared_text
        assert EXAMPLE_ONLY_END in cleared_text

    def test_whenLineHasOpenerButNoCloserContentIsDeletedUntilItsEnd(self, block_deleter):
        cleared_text = block_deleter.deleteBlockFrom(' '.join([EXAMPLE_ONLY_START, EXAMPLE_OPENER, EXAMPLE_ONLY_BLOCK]))
        assert cleared_text == EXAMPLE_ONLY_START + ' '

    def test_whenLineHasCloserButNoOpenerContentIsDeletedFromTheStart(self, block_deleter):
        cleared_text = block_deleter.deleteBlockFrom(' '.join([EXAMPLE_ONLY_BLOCK, EXAMPLE_CLOSER, EXAMPLE_ONLY_END]))
        assert cleared_text == ' ' + EXAMPLE_ONLY_END

    def test_canDeleteBlockSpreadingMultipleLines(self, block_deleter):
        cleared_result = []
        for line in EXAMPLE_PARTS:
            cleared_result += [block_deleter.deleteBlockFrom(line)]
        assert '\n'.join(cleared_result) == '\n\n\n\n'.join([EXAMPLE_ONLY_START, EXAMPLE_ONLY_END])

    def test_canDeleteNestedBlockSpreadingMultipleLines(self, block_deleter):
        cleared_result = []
        for line in [EXAMPLE_ONLY_START, EXAMPLE_OPENER,
            EXAMPLE_OPENER, EXAMPLE_ONLY_BLOCK, EXAMPLE_CLOSER,
            EXAMPLE_ONLY_BLOCK, EXAMPLE_CLOSER, EXAMPLE_ONLY_END]:
            cleared_result += [block_deleter.deleteBlockFrom(line)]
        assert filterEmptyLines('\n'.join(cleared_result)) == '\n'.join([EXAMPLE_ONLY_START, EXAMPLE_ONLY_END])
