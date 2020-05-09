import re
import sys


def detectBlockOpener(block_type: str, text: str):
    return re.search(r"\\begin{" + block_type + "}", text)


def detectBlockCloser(block_type: str, text: str):
    return re.search(r"\\end{" + block_type + "}", text)


def deleteBlockOf(block_type: str, text: str):
    detected_opener = detectBlockOpener(block_type, text)
    detected_closer = detectBlockCloser(block_type, text)
    if detected_opener and detected_closer:
        return text[:detected_opener.start(0)] + text[detected_closer.end(0):]
    elif detected_opener:
        return text[:detected_opener.start(0)]
    elif detected_closer:
        return text[detected_closer.end(0):]
    return text


def filterEmptyLines(text: str):
    return '\n'.join([line for line in text.splitlines() if (line != '')])


class BlockDeleter(object):

    def __init__(self, block_type: str):
        self.block_type = block_type
        self.block_flag = 0

    def detectBlockOpener(self, text: str):
        return detectBlockOpener(self.block_type, text)

    def detectBlockCloser(self, text: str):
        return detectBlockCloser(self.block_type, text)

    def deleteBlockFrom(self, text: str):
        detected_opener = detectBlockOpener(self.block_type, text)
        detected_closer = detectBlockCloser(self.block_type, text)

        if not detected_opener and not detected_closer:
            return "" if self.block_flag > 0 else text

        result = ""
        if detected_opener:
            self.block_flag += 1
            result += text[:detected_opener.start(0)]
        if detected_closer:
            self.block_flag -= 1
            result += text[detected_closer.end(0):]

        return result


if __name__ == "__main__":
    block_type = sys.argv[1]
    block_deleter = BlockDeleter(block_type)
    for line in sys.stdin:
        output_line = block_deleter.deleteBlockFrom(line)
        if output_line != '':
            print(output_line)
