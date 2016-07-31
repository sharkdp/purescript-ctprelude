#!/bin/python
#
# A hacky script to generate the README.md from the PureScript source.

import re

with open("src/CTPrelude.purs") as source:
    with open("README.md", "w") as readme:
        lastLineWasCode = False

        for line in source.readlines():
            isCode = False

            if re.match(r"^\s*$", line):
                # Empty line

                isCode = lastLineWasCode

            elif re.match(r"^-- \|", line):
                # Line with a comment

                # Remove the comment prefix:
                line = re.sub(r"^-- \| ?", "", line)

            elif re.match(r"^-+$", line):
                # Line with separator, print nothing
                line = ""

            else:
                # Line with code

                isCode = True

            if not isCode and lastLineWasCode:
                readme.write("```\n")
            elif isCode and not lastLineWasCode:
                readme.write("``` purescript\n")

            readme.write(line)
            lastLineWasCode = isCode

        if lastLineWasCode:
            readme.write("```\n")
