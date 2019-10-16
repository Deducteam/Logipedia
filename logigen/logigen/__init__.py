import click
import logging
from pathlib import Path

from .logigen import create_db, Logigen


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
def create_static_website(input, database, output, verbose):
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
        Logigen.create_static_website_from_db(database, output_path)
    else:
        if not input_path.is_dir():
            raise NotADirectoryError("{} is not a directory".format(input_path))
        Logigen.create_static_website(input_path, output_path)


@click.command()
@click.argument("input_dir", type=click.Path(exists=True))
@click.argument("db_path", type=click.Path())
def create_database(input_dir, db_path):
    input_dir = Path(input_dir)
    db_path = Path(db_path)
    if not input_dir.is_dir():
        raise NotADirectoryError("{} is not a directory".format(input_dir))
    create_db(input_dir, db_path.name)


if __name__ == "__main__":
    create_static_website()
