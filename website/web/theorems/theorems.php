<?php
require '../vendor/autoload.php';
$mongo = new MongoDB\Client('mongodb://localhost:27017');

if(isset($_GET['md']) && isset($_GET['id']) && isset($_GET['kind'])) {
    $md=$_GET['md'];
    $id=$_GET['id'];
    $kind=$_GET['kind'];
    $pp_kind=ucfirst($kind);
}
else {
    die();
}
// Compute things to print for each system
$query=$mongo->logipedia->printing->find(['md' => $md, 'id' => $id]);
$entry=array();
foreach($query as $line) {
    $entry[$line["sys"]]=["content" => $line["content"]];
}
// Compute direct dependencies
$query=$mongo->logipedia->dependencies->aggregate([
    ['$match' => ['md' => $md, 'id' => $id]],
    ['$lookup' => ['from' => "items", 'localField' => "idDep", 'foreignField' => 'id', 'as' => 'idKind']], // Make the assumption that all identifiers are unique
    ['$unwind' => '$idKind']]);
$directDeps=array();
$directDeps["axiom"] = [];
$directDeps["theorem"] = [];
$directDeps["constant"] = [];
$directDeps["definition"] = [];
foreach($query as $line) {
     array_push($directDeps[$line['idKind']['kind']], ['md' => $line['mdDep'], "id" => $line['idDep']]);
}

// Compute transitive closure for dependencies
$query=$mongo->logipedia->theory->aggregate([
    ['$match'  => ['md' => $md, 'id' => $id]],
    ['$lookup' => ['from' => "items", 'localField' => "idDep", 'foreignField' => 'id', 'as' => 'idKind']], // Make the assumption that all identifiers are unique
    ['$sort'   => ['order' => 1]],
    ['$unwind' => '$idKind']]);

$theory=array();
$theory["axiom"] = [];
$theory["theorem"] = [];
$theory["constant"] = [];
$theory["definition"] = [];
$theory["tyop"] = [];
$transDeps=array();
foreach($query as $line) {
     array_push($theory[$line['idKind']['kind']], ['md' => $line['mdDep'], "id" => $line['idDep']]);
     array_push($transDeps,[
         'kind' => $line['idKind']['kind'],
         'md'   => $line['mdDep'],
         'id'   => $line['idDep']
         ]);
}

function print_dep($kind, $deps) {
     echo '<div class="card col-md-3">';
     echo '<div class="card-header text-center">'.$kind.'</div>';
     echo '<div class="list-group">';
     foreach ($deps[$kind] as $line)
     {
           echo '<a href="theorems.php?md='.$line["md"].'&id='.$line["id"].'&kind=axiom" class="list-group-item list-group-item-action text-center">'.$line["md"].".".$line["id"].'</a>';
     }
     echo '</div></div>';
}

function print_container($header, $body) {
    echo '<div class="container">';
    echo '<fieldset class="scheduler-border">';
    echo '<legend class="scheduler-border">';
    echo $header;
    echo '</legend>';
    echo '<p class="text-center">';
    echo $body;
    echo "</p></fieldset></div>";
}

function print_constant($body) {
    print_container("Type", $body);
}

function print_axiom($body) {
    print_container("Statement", $body);
}

function print_definition($body) {
    print_container("Body", $body);
}

function print_theorem($body) {
    print_container("Statement", $body);
}

function print_tyOp($body) {
    print_container("Type Operator", $body);
}

function print_entry($kind, $entry) {
     if($kind == "definition") {
          print_definition($entry["content"]);
     }
     else if ($kind == "theorem") {
          print_theorem($entry["content"]);
     }
     else if ($kind == "axiom") {
          print_axiom($entry["content"]);
     }
     else if ($kind == "constant") {
          print_constant($entry["content"]);
     }
}

function print_system($kind,$entry,$system) {
    print_entry($kind, $entry[$system]);
}

function print_download_button($sys, $md, $id) {
     echo '<a class="btn btn-secondary btn-lg down-col" href="download/download.php?lang='.$sys.'&md='.$md.'&id='.$id.'">';
}

?>
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Logipedia</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css" integrity="sha384-9gVQ4dYFwwWSjIDZnLEWnxCjeSWFphJiwGPXr1jddIhOegiu1FwO5qRGvFXOdJZ4" crossorigin="anonymous">
    <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.1.0/css/all.css" integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt" crossorigin="anonymous">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js" integrity="sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm" crossorigin="anonymous"></script>
    <link rel="stylesheet" type="text/css" href="theorems.css">
  </head>
  <body onLoad="document.getElementById('attente').style.display='none'">
    <nav class="navbar navbar-expand-md bg-dark navbar-dark fixed-top">
      <div class="container">
        <a class="navbar-brand" href="../index.php"><i class="fas fa-award"></i> Logipedia</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="collapsibleNavbar">
          <ul class="navbar-nav mr-auto mt-2 mt-lg-0">
            <li class="nav-item">
              <a class="nav-link" href="../about/about.php">About</a>
            </li>
            <li class="nav-item">
              <a class="nav-link" href="../about/modules.php">Modules</a>
            </li>
          </ul>
          <form class="form-inline my-2 my-lg-0" action="../index.php" method="get">
            <input class="form-control mr-sm-2 col-8" type="search" name="search" placeholder="Search" aria-label="Search">
            <button class="btn btn-outline-light my-2 my-sm-0 " type="submit">Search</button>
          </form>
        </div>
      </div>
    </nav>
    // This prints the left floatting menu
    <div id="mySidenav" class="sidenav d-none d-sm-block">
      <div class="container">
        <a href="#dedukti" id="a-dedukti">Dedukti &nbsp; &nbsp;<img src="../picture/dedukti.png" class="img-fluid" alt="Load"></a>
        <a href="#matita" id="a-matita">Matita &nbsp; &nbsp; &nbsp;<img src="../picture/matita.png" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>
        <a href="#coq" id="a-coq">Coq &nbsp; &nbsp; &nbsp; &nbsp; <img src="../picture/coq.png" class="img-fluid" alt="Load" style="margin-left:10px;width:50px;height:50px;"></a>
        <a href="#lean" id="a-lean">Lean &nbsp; &nbsp; &nbsp; &nbsp;<img src="../picture/lean.jpg" class="img-fluid" alt="Load" style="margin-left:5px;width:50px;height:50px;"></a>
        <a href="#pvs" id="a-pvs">PVS &nbsp; &nbsp; &nbsp; &nbsp; <img src="../picture/pvs.jpg" class="img-fluid" alt="Load" style="margin-left:5px;width:50px;height:50px;"></a>
        <a href="#openTheory" id="a-openTheory"><small>OpenTheory</small> <img src="../picture/openTheory.png" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>
      </div>
    </div>
    <div id="dedukti">
      <img src="../picture/dedukti-jumb.jpg" class="img-fluid image" alt="Dedukti-jumb">
      <hr class="my-4">
      <h3 class="text-center"><b>
<?php print_container($pp_kind, $md.".".$id); ?>
      </b></h3>
<?php print_system($kind, $entry, "dedukti"); ?>
      <div class="container">
        <fieldset class="scheduler-border">
          <legend class="scheduler-border"> Dependences </legend>
          <div class="card">
            <div class="card-header" id="headingOne">
                <a class="list-group-item list-group-item-action text-center" data-toggle="collapse" data-target="#collapseOne" aria-expanded="true" aria-controls="collapseOne" id="btHide">
                <i class="fas fa-chevron-down" id="iconDep"></i>
                </a>
            </div>
            <div id="collapseOne" class="collapse" aria-labelledby="headingOne" data-parent="#accordion">
              <div class="row">
     <?php
        print_dep("axiom", $directDeps);
        print_dep("constant", $directDeps);
        print_dep("definition", $directDeps);
        print_dep("theorem", $directDeps);
     ?>

              </div>
            </div>
          </div>
        </fieldset>
      </div>
      <div class="container">
        <fieldset class="scheduler-border">
          <legend class="scheduler-border accordion" id="accordion"> Theory </legend>
          <div class="card">
            <div class="card-header" id="headingTwo">
              <a class="list-group-item list-group-item-action text-center" data-toggle="collapse" data-target="#collapseTwo" aria-expanded="true" aria-controls="collapseTwo" id="btHide2">
                <i class="fas fa-chevron-down" id="iconDep2"></i>
              </a>
            </div>
            <div id="collapseTwo" class="collapse" aria-labelledby="headingTwo" data-parent="#accordion">
              <div class="row">
     <?php
        print_dep("axiom", $theory);
        print_dep("constant", $theory);
        print_dep("definition", $theory);
        print_dep("theorem", $theory);
     ?>
            </div>
          </div>
        </fieldset>
      </div>
    </div>
    <div id="coq">
      <hr class="my-4">
      <img src="../picture/coq-jumb.jpg" class="img-fluid image" alt="Coq-Jumb">
      <hr class="my-4">
      <?php print_system($kind, $entry, "coq"); ?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
            <?php print_download_button("coq",$md, $id) ?>
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="matita">
      <hr class="my-4">
      <img src="../picture/matita-jumb.jpg" class="img-fluid image" alt="Matita-Jumb">
      <hr class="my-4">
      <?php print_system($kind, $entry, "matita"); ?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
            <?php print_download_button("coq",$md, $id) ?>
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="lean">
      <hr class="my-4">
      <img src="../picture/lean-jumb.jpg" class="img-fluid image" alt="Lean-jumb">
      <hr class="my-4">
      <?php print_system($kind, $entry, "lean"); ?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
            <?php print_download_button("coq",$md, $id) ?>
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="pvs">
      <hr class="my-4">
      <img src="../picture/pvs-jumb.jpg" class="img-fluid image" alt="PVS-jumb">
      <hr class="my-4">
      <hr class="my-4">
      <?php print_system($kind, $entry, "pvs"); ?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
            <?php print_download_button("coq",$md, $id) ?>
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="openTheory">
      <hr class="my-4">
      <img src="../picture/openTheory.png" class="img-fluid image" alt="OpenTheory">
      <hr class="my-4">
      <div class="container">
        <fieldset class="scheduler-border">
        <legend class="scheduler-border">
        </legend>
        <p class="text-center">
        Printing for OpenTheory is not working at the moment.
        </p>
    </fieldset>
      </div>
      <div class="container">
        <div class="col-md-12 text-center">
          <a class="btn btn-secondary btn-lg down-col" href="download/download.php?lang=openTheory">
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
      </br>
    </div>

    <script src="theorems.js"></script>
  </body>
</html>
