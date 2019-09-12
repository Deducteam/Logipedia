import ast
import os
import subprocess
import sys

if len(sys.argv) != 4:
    print("ERROR: This script should take a module as parameter")
    exit(1)

dkdep = sys.argv[1]
directory = sys.argv[2]

target = sys.argv[3]
name = target
version = "1.0"
description = "Part of the arithmetic library automatically translated from Matita"
author = "François Thiré <francois.thire@inria.fr"
license = "MIT"
show = target

cmd = "{:s} --ignore -s -I {:s} {:s}/*.dk".format(dkdep, directory, directory)
proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
pre_sorted_modules = proc.stdout.read()
sorted_modules = pre_sorted_modules.decode("utf-8").strip('\n').split(" ")
register = dict()


def print_prelude():
    print("name: {:s}".format(name))
    print("version: {:s}".format(version))
    print("description: {:s}".format(description))
    print("author: {:s}".format(author))
    print("license: {:s}".format(license))
    print("show: \"{:s}\"".format(show))

def sanitize(md):
    return md.replace("_","")

def clean(md):
    return os.path.splitext(os.path.basename(md))[0]

def print_module(module):
    print(sanitize(clean(module)))
    print("{")
    for dep in register[module]:
        print("\timport: {:s}".format(sanitize(clean(dep))))
    print("\tarticle: \"{:s}.art\"".format(clean(module)))
    print("}")
    print()

def print_main(modules):
    print("main")
    print("{")
    for i in modules:
        print("\timport: {:s}".format(sanitize(clean(i))))
    print("}")


print_prelude()
for i in sorted_modules:
    cmd = "{:s} --ignore -s -I {:s} {:s}".format(dkdep, directory, i)
    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
    pre_dependencies = proc.stdout.read()
    dependencies = pre_dependencies.decode("utf-8").strip('\n').split(" ")
    sdependencies = set(i for i in dependencies)
    register[i] = sdependencies
    current_deps = set()
    for j in sdependencies:
        register[i] = register[i] | register[j]
    register[i] = (register[i] - {i})
    print_module(i)
else:
    print_main(sorted_modules)
