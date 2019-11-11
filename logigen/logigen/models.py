import logging
from peewee import *
from playhouse.sqlite_ext import *

_database = SqliteDatabase(None)


def init_database(db_path):
    logging.info(f"Initializing SQLite database at path {db_path}")
    _database.init(db_path, pragmas={"journal_mode": "wal"})
    return _database


class UnknownField(object):
    def __init__(self, *_, **__):
        pass


class BaseModel(Model):
    class Meta:
        database = _database


class ProofObject(BaseModel):
    deps = JSONField()
    exp = JSONField()
    label = JSONField()
    name = TextField(primary_key=True)
    taxonomy = TextField()
    term = JSONField()
    term_opt = JSONField(null=True)
    theory = JSONField()

    class Meta:
        table_name = "objects"
