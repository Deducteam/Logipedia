from collections import defaultdict

from .filenames import fqn_to_object_filename
from .pretty_print import pretty_print


def _with_object_url(lst):
    """
    Takes a list of dicts that have a 'name' (fqn) field.
    Returns a list of dicts with an additional 'url' field derived
    from the fully-qualified name of the object.
    """
    return [dict(item, url=fqn_to_object_filename(item["name"])) for item in lst]


# XXX move into model?
def make_view_object(obj):
    """
    Converts a ProofObject into an object that can be used to render
    the website view.
    """
    deps = [{"name": name} for name in obj.deps]
    deps = _with_object_url(deps)
    pp_opt = pretty_print(obj.term_opt) if obj.term_opt else None

    return dict(
        name=obj.name,
        taxonomy=obj.taxonomy,
        term_label=obj.label[0],
        term_opt_label=obj.label[1],
        pp=pretty_print(obj.term),
        pp_opt=pp_opt,
        deps=deps,
        theory=obj.theory,
        exports=obj.exp,
    )
