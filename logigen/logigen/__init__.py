import click
import logging
import os
from pathlib import Path
import shlex

from .logigen import create_db, Logigen
from .pretty_print_ext import make_pp

def _exe_name_unsafe(opt):
    if opt:
        return opt
    else:
        return os.environ.get('LOGIPEDIA_PP', 'logipp-latex')

def exe_name(opt):
    return shlex.quote(_exe_name_unsafe(opt))

@click.command()
@click.option("--input", "-i", type=click.Path(exists=True), help="Input directory")
@click.option("--database", "-db", type=click.Path(exists=True), help="Input database")
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default=Path.cwd() / "gen_website",
    help="Output directory",
)
@click.option(
    "--verbose",
    "-v",
    default=False,
    is_flag=True,
    help="Print more diagnostic messages",
)
@click.option(
    "--pretty-printer",
    "-p",
    default=None,
    help="Pretty-printer executable")
@click.option(
    "--pp-extra",
    default=None,
    help="Pretty-printer extra options")
@click.option(
    "--pp-allow-failure",
    default=False,
    is_flag=True,
    help="Allow pretty-printer to fail")
@click.option(
    "--skip-objects",
    "-s",
    default=False,
    is_flag=True,
    help="Skip proof objects (generate indexes only)"
)
def create_static_website(input, database, output, verbose, pretty_printer, pp_extra, pp_allow_failure, skip_objects):
    if pp_extra is not None:
        pp_extra = shlex.split(pp_extra)
    pretty_printer = make_pp(exe_name(pretty_printer), pp_extra,
            pp_allow_failure)
    if input is None and database is None:
        raise click.UsageError("One of 'input' or 'database' options should be defined")
    if input is not None and database is not None:
        raise click.UsageError("'input' and 'database' are mutually exclusive")
    if verbose:
        logging.basicConfig(level=logging.INFO)
    input_path = Path(input) if input else None
    database = Path(database) if database else None
    output_path = Path(output)

    if database:
        Logigen.create_static_website_from_db(pretty_printer, database, output_path, skip_objects)
    else:
        if not input_path.is_dir():
            raise NotADirectoryError("{} is not a directory".format(input_path))
        Logigen.create_static_website(pretty_printer, input_path, output_path, skip_objects)


@click.command()
@click.argument("input_dir", type=click.Path(exists=True))
@click.argument("db_path", type=click.Path())
@click.option(
    "--verbose",
    "-v",
    default=False,
    is_flag=True,
    help="Print more diagnostic messages",
)
def create_database(input_dir, db_path, verbose):
    if verbose:
        logging.basicConfig(level=logging.INFO)
    input_dir = Path(input_dir)
    db_path = Path(db_path)
    if not input_dir.is_dir():
        raise NotADirectoryError("{} is not a directory".format(input_dir))
    create_db(input_dir, db_path.name)


if __name__ == "__main__":
    create_static_website()
