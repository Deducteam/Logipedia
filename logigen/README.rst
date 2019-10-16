=======
Logigen
=======

***************
Summary
***************
Logigen is a tool that generates a static website from a set of JSON "object" files produced by Logipedia from a set of Dedukti files.

***************
Installing 
***************
From the source directory, run ``pip install .`` to install logigen.

***************
Running
***************
Run ``logigen -i <input dir> -o <output dir>``

Run ``logigen --help`` for more information about the available options.

***************
Developing
***************
To install logigen in edit mode (e.g. to work on the tool itself), run ``pip install -e .[dev]``

Run once ``pre-commit install`` afterwards (``pre-commit`` is installed
automatically during the previous step) to set up the auto formatter
