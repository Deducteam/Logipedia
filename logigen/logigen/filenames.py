def fqn_to_object_filename(fqn):
    """
    Converts a fully qualified Dedukti name (e.g. package.subpackage.id)
    to an html filename that represents this object
    """
    fqn = fqn.replace("/", "_")
    fqn = fqn.replace(":", "_")
    return "{}.html".format(fqn)

def module_filename(module_name):
    module_name = module_name.replace(":", "_")
    return "_logigen_module_{}.html".format(module_name)
