from distutils.dir_util import copy_tree
from jinja2 import Environment, PackageLoader, select_autoescape
import json
import logging
from pathlib import Path
from peewee import fn, SQL
from pkg_resources import resource_filename
import sqlite3
from sqlite_utils import Database
from tempfile import NamedTemporaryFile
from tqdm import tqdm

from .filenames import fqn_to_object_filename, module_filename
from .models import init_database, ProofObject
from .object_view import make_view_object


def create_db(input_path, db_path):
    """
    Load a set of JSON files containing proof objects into
    a SQLite database.

    Parameters
    ----------
    input_path : path-like object.
        A path to a folder containing JSON files as exported
        by Logipedia.

    db_path : path-like object.
        Path to a database file that will be created.
    """
    input_path = Path(input_path)
    files_to_process = list(input_path.glob("**/*.json"))
    if not files_to_process:
        logging.warn("No JSON files to process")
    conn = sqlite3.connect(db_path)
    db = Database(conn)
    for p in tqdm(files_to_process):
        with open(p) as json_file:
            logging.info(f"loading file {p} into database")
            objlist = json.load(json_file)
            db["objects"].insert_all(objlist, pk="name")


def _fqn_to_module_id(module_id):
    return module_id.split("/")[0]


def _save_page(page, filename):
    logging.info(f"saving file {filename}")
    with open(filename, "w") as out_file:
        out_file.write(page)


class Logigen:
    """
    Generates a static website.

    Parameters
    ----------
    pp: PrettyPrinterExt
        a pretty-printer instance
    """
    def __init__(self, pp, db_path, output_path):
        self.env = Environment(
            loader=PackageLoader("logigen", "templates"),
            autoescape=select_autoescape(["html", "xml"]),
        )
        self.pp = pp
        self.database = init_database(db_path)
        self.output_path = Path(output_path)
        self.output_path.mkdir(exist_ok=True)

    def _copy_static_resources(self):
        """
        Copy the static assets (e.g. CSS files) embedded in the logigen package 
        to the target directory of the website
        """
        dirname = resource_filename("logigen", "static")
        copy_tree(dirname, str(self.output_path / "static"))

    def _create_website_index(self):
        template = self.env.get_template("index.html")
        return template.render()

    def _create_object_page(self, obj):
        """
        Return the HTML page for an object, as a string.
        """
        template = self.env.get_template("object.html")
        vobj = make_view_object(obj, self.pp)
        return template.render(**vobj)

    def _save_object_page(self, obj, page):
        filename = fqn_to_object_filename(obj.name)
        _save_page(page, self.output_path / filename)

    def _save_module_page(self, module_name, page):
        filename = module_filename(module_name)
        _save_page(page, self.output_path / filename)

    def _list_modules(self):
        return ProofObject.select(
            fn.substr(ProofObject.name, 1, fn.instr(ProofObject.name, "/") - 1).alias(
                "module"
            )
        ).distinct()

    def _list_module_contents(self, module_name):
        return ProofObject.select(
            fn.substr(ProofObject.name, 1, fn.instr(ProofObject.name, "/") - 1).alias(
                "module"
            ),
            ProofObject.name,
            ProofObject.taxonomy,
        ).where(SQL("module") == module_name
        ).order_by(ProofObject.taxonomy)

    def _create_module_index_page(self, module_list):
        template = self.env.get_template("module_index.html")
        return template.render(module_list=module_list,
                filename=module_filename)

    def _create_module_page(self, module_name, module_contents):
        template = self.env.get_template("module.html")
        return template.render(module_name=module_name, module_contents=module_contents,
                filename=fqn_to_object_filename)

    def _ingest_object(self, obj):
        # validate json object against schema?
        page = self._create_object_page(obj)
        self._save_object_page(obj, page)

    def _ingest_db_objects(self):
        for proof_object in tqdm(ProofObject.select().iterator()):
            self._ingest_object(proof_object)

    def _create_static_website(self, skip_objects):
        if not skip_objects:
            self._ingest_db_objects()
        mod_index_page = self._create_module_index_page(self._list_modules())
        _save_page(mod_index_page, self.output_path / "module_index.html")
        for row in self._list_modules():
            module_name = row.module
            module_contents = self._list_module_contents(module_name)
            module_page = self._create_module_page(module_name, module_contents) 
            self._save_module_page(module_name, module_page)
        site_index = self._create_website_index()
        _save_page(site_index, self.output_path / "index.html")
        self._copy_static_resources()

    @classmethod
    def create_static_website(cls, pp, input_path, output_path, skip_objects):
        """
        Create a static website from a Logipedia JSON export folder.

        Parameters
        ----------
        input_path : path-like object.
            A path to a folder containing JSON files as exported
            by Logipedia.
        output_path : path-like object
            Path to an output folder for the generated website.
            Will be created if needed
        """
        with NamedTemporaryFile(prefix="logigen", suffix=".db") as dbfile:
            create_db(input_path, dbfile.name)
            Logigen.create_static_website_from_db(pp, dbfile.name, output_path, skip_objects)

    @classmethod
    def create_static_website_from_db(cls, pp, db_path, output_path, skip_objects):
        """
        Create a static website from a SQLite database.

        Parameters
        ----------
        db_path : path-like object
             Path to an existing SQLite database.
        output_path : path-like object
             Path to an output folder for the generated website.
             Will be created if needed
        """
        cls(pp, db_path, output_path)._create_static_website(skip_objects)
