import logging
from json import dumps
from shutil import which
from subprocess import CalledProcessError, run
import sys


def make_pp(exe, extra_opts=None, allow_failure=False):
    return PrettyPrinterExt(exe, extra_opts, allow_failure)


def fail_if_binary_not_installed(exe):
    if not which(exe):
        print(f"Could not find pretty-printer {exe}", file=sys.stderr)
        print(
            f"To install a pretty-printer, see https://github.com/gabrielhdt/LogiPPedia",
            file=sys.stderr,
        )
        sys.exit(1)


class PrettyPrinterExt:
    """
    An external pretty-printer

    Parameters
    ----------
    exe: str
        name of the pretty printer executable (e.g. logipp or /bin/cat)
    extra_opts: list
        extra options that will be passed to the pretty-printer
    allow_failure: bool
        whether to allow the pretty-printer to fail (default False)
    """

    def __init__(self, exe, extra_opts=None, allow_failure=False):
        fail_if_binary_not_installed(exe)
        self.exe = exe
        self.allow_failure = allow_failure
        self.extra_opts = extra_opts

    def pretty_print(self, ppterm):
        """
        Pretty-print a term
        """
        term_string = dumps(ppterm)
        runc = [self.exe] + [self.extra_opts or ""] 
        try:
            logging.info(f"pretty_print: running {runc}")
            p = run(
                runc,
                shell=True,
                check=not self.allow_failure,
                capture_output=True,
                universal_newlines=True,
                text=True,
                input=term_string,
            )
            if self.allow_failure and p.returncode != 0:
                return "Could not pretty-print term"

            return p.stdout
        except CalledProcessError as e:
            logging.error(
                f"Pretty-printer returned a nonzero exit code while processing term {term_string})"
            )
            raise e
