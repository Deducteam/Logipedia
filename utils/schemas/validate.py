#!/usr/bin/env python
from jsonschema import validate
import sys

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage {}: <SCHEMA> <JSON>".format(sys.argv[0]))
    else:
        with open(sys.argv[1]) as s:
            with open(sys.argv[2]) as j:
                validate(j, s)
