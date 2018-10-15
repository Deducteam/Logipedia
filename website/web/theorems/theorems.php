<?php
require '../vendor/autoload.php';
$mongo = new MongoDB\Client('mongodb://localhost:27017');

if(isset($_GET['md']) && isset($_GET['id']) && isset($_GET['kind'])) {
    $md=$_GET['md'];
    $id=$_GET['id'];
    $kind=$_GET['kind'];
    switch ($kind) {
    case "definition":
        $collection = $mongo->logipedia->definitions;
        $pp_kind="Definition";
        break;
    case "theorem":
        $collection = $mongo->logipedia->theorems;
        $pp_kind="Theorem";
        break;
    case "constant":
        $collection = $mongo->logipedia->constants;
        $pp_kind="Constant";
        break;
    case "axiom":
        $collection = $mongo->logipedia->axioms;
        $pp_kind="Axiom";
        break;
    default:
        die("Unknown kind '$kind'");
    }
}
else {
    die();
}
// Compute things to print for each system
$query=$collection->find(['md' => $md, 'id' => $id]);
$entry=array();
foreach($query as $line) {
     if($kind == "definition") {
          $entry[$line["sys"]]=["type" => $line["type"], "body" => $line["body"]];
     }
     else if ($kind == "theorem" || $kind == "axiom") {
          $entry[$line["sys"]]=["statement" => $line["statement"]];
     }
     else if ($kind == "constant") {
          $entry[$line["sys"]]=["type" => $line["type"]];
     }
}
// Compute direct dependencies
$query=$mongo->logipedia->idDep->aggregate([
    ['$match' => ['md' => $md, 'id' => $id]],
    ['$lookup' => ['from' => "idKind", 'localField' => "idDep", 'foreignField' => 'id', 'as' => 'idKind']], // Make the assumption that all identifiers are unique
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
$query=$mongo->logipedia->closureIdDep->aggregate([
    ['$match'  => ['md' => $md, 'id' => $id]],
    ['$lookup' => ['from' => "idKind", 'localField' => "idDep", 'foreignField' => 'id', 'as' => 'idKind']], // Make the assumption that all identifiers are unique
    ['$sort'   => ['order' => 1]],
    ['$unwind' => '$idKind']]);

$theory=array();
$theory["axiom"] = [];
$theory["theorem"] = [];
$theory["constant"] = [];
$theory["definition"] = [];
$transDeps=array();
foreach($query as $line) {
     array_push($theory[$line['idKind']['kind']], ['md' => $line['mdDep'], "id" => $line['idDep']]);
     array_push($transDeps,[
         'kind' => $line['idKind']['kind'],
         'md'   => $line['idKind']['mdDep'],
         'id'   => $line['idKind']['idDep']
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

function print_definition($type, $body) {
    print_container("Type", $type);
    print_container("Body", $body);
}

function print_theorem($body) {
    print_container("Statement", $body);
}

function print_entry($kind, $entry) {
     if($kind == "definition") {
          print_definition($entry["type"],$entry["body"]);
     }
     else if ($kind == "theorem") {
          print_theorem($entry["statement"]);
     }
     else if ($kind == "axiom") {
          print_axiom($entry["statement"]);
     }
     else if ($kind == "constant") {
          print_constant($entry["type"]);
     }
}

function print_system($kind,$entry,$system) {
     if($system == "dedukti") {
          print_entry($kind,$entry[1]);
     }
     else if ($kind == "coq") {
          print_entry($kind,$entry[2]);
     }
     else if ($kind == "matita") {
          print_entry($kind,$entry[3]);
     }
     else if ($kind == "lean") {
          print_entry($kind,$entry[4]);
     }
     else if ($kind == "pvs") {
          print_entry($kind,$entry[5]);
     }
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
              <a class="nav-link" href="about/modules.php">Modules</a>
            </li>
          </ul>
          <form class="form-inline my-2 my-lg-0" method="post">
            <input class="form-control mr-sm-2 col-8" type="search" name="search" placeholder="Search" aria-label="Search">
            <button class="btn btn-outline-light my-2 my-sm-0 " type="submit" name="submit">Search</button>
          </form>
        </div>
      </div>
    </nav>
    // This prints the left floatting menu
    <a href="#matita" id="a-matita">Matita &nbsp; &nbsp; &nbsp;<img src="../picture/matita.png" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>
    <a href="#coq" id="a-coq">coq &nbsp; &nbsp; &nbsp; &nbsp; <img src="../picture/coq.png" class="img-fluid" alt="Load" style="margin-left:10px;width:50px;height:50px;"></a>
    <a href="#lean" id="a-lean">Lean &nbsp; &nbsp; &nbsp; &nbsp;<img src="../picture/lean.jpg" class="img-fluid" alt="Load" style="margin-left:5px;width:50px;height:50px;"></a>
    <a href="#pvs" id="a-pvs">PVS &nbsp; &nbsp; &nbsp; &nbsp; <img src="../picture/pvs.jpg" class="img-fluid" alt="Load" style="margin-left:5px;width:50px;height:50px;"></a>
    <a href="#openTheory" id="a-openTheory"><small>openTheory</small> <img src="../picture/openTheory.png" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>
    <div id="mySidenav" class="sidenav d-none d-sm-block">
      <div class="container">
        <a href="#dedukti" id="a-dedukti">Dedukti &nbsp; &nbsp;<img src="../picture/dedukti.png" class="img-fluid" alt="Load"></a>
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
      <?php print_system($kind, $entry, "dedukti"); ?>
<?php
  //Nous bouclons pour chaque module et nous ecrivons selon si l'element courant est un parametre/definitions/etc
  foreach($tabModuleR as $val){
    writeFile2("\nModule ".$val.".\n", $nameOfFile,'coq');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "3"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->constants;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "3"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axioms;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "3"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theorems;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "3"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                writeFile2("\n\tDefinition ".$tabFinal[$cpt][1]. " : ".$entry2['statement']." := ".$entry2['proof'].".\n", $nameOfFile, 'coq');
              }
            }
            else
            {
              writeFile2("\n\tAxiom ".$tabFinal[$cpt][1]. " : ".$entry2['statement'].".\n", $nameOfFile,'coq');
            }
            }
            else
            {
              writeFile2("\n\tParameter ".$tabFinal[$cpt][1]. " : ".$entry2['type'].".\n", $nameOfFile,'coq');
            }
        }
        else{
          writeFile2("\n\tDefinition ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['body'].".\n", $nameOfFile,'coq');
        }
      }
    }
    writeFile2("\nEnd ".$val.".\n", $nameOfFile,'coq');
  }
?>
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
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."_".$_SESSION['tuple'][$id]['id'].".ma";
  }
  else{
    $nameOfFile = $_GET['rechMd']."_".$_GET['rechId'].".ma";
  }
  if(file_exists('download/matita/'.$nameOfFile)){
    unlink('download/matita/'.$nameOfFile);
  }
  $_SESSION['matita'] = 'matita/'.$nameOfFile;
?>
      <div class="container">
<?php
  $collection = $mongo->logipedia->$collect;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'id' => $_SESSION['tuple'][$id]['id'], 'sys' => "2"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'id' => $_GET['rechId'], 'sys' => "2"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof' && $res!="kw"){
?>
        <fieldset class="scheduler-border">
          <legend class="scheduler-border">
<?php

        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
          case "body":
                  echo "Body";
          break;
        }

?>
          </legend>
          <p class="text-center">
<?php
      echo $entry[$res];
?>
          </p>
<?php
    }
    echo "</fieldset>";
  }
?>
      </div>
<?php
  //Nous bouclons pour chaque module et nous ecrivons selon si l'element courant est un parametre/definitions/etc
  foreach($tabModuleR as $val){
    writeFile2("\ninclude \"basics/pts.ma\".\n", $nameOfFile,'matita');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "2"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->constants;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "2"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axioms;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "2"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theorems;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "2"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                writeFile2("\ndefinition ".$tabFinal[$cpt][1]. " : ".$entry2['statement']." := ".$entry2['proof'].".\n", $nameOfFile,'matita');
              }
            }
            else
            {
              writeFile2("\naxiom ".$tabFinal[$cpt][1]. " : ".$entry2['statement'].".\n", $nameOfFile,'matita');
            }
            }
            else
            {
              writeFile2("\naxiom ".$tabFinal[$cpt][1]. " : ".$entry2['type'].".\n", $nameOfFile,'matita');
            }
        }
        else{
          writeFile2("\ndefinition ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['body'].".\n", $nameOfFile,'matita');
        }
      }
    }
  }
?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
          <a class="btn btn-secondary btn-lg down-col" href="download/download.php?lang=matita">
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="lean">
      <hr class="my-4">
      <img src="../picture/lean-jumb.jpg" class="img-fluid image" alt="Lean-jumb">
      <hr class="my-4">
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."_".$_SESSION['tuple'][$id]['id'].".lean";
  }
  else{
    $nameOfFile = $_GET['rechMd']."_".$_GET['rechId'].".lean";
  }
  if(file_exists('download/lean/'.$nameOfFile)){
    unlink('download/lean/'.$nameOfFile);
  }
  $_SESSION['lean'] = 'lean/'.$nameOfFile;
?>

      <div class="container">
<?php
  $collection = $mongo->logipedia->$collect;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'id' => $_SESSION['tuple'][$id]['id'], 'sys' => "4"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'id' => $_GET['rechId'], 'sys' => "4"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof' && $res!='computable' && $res!="kw"){
?>
        <fieldset class="scheduler-border">
          <legend class="scheduler-border">
<?php
      switch ($res) {
                case "statement":
                        echo "Statement";
                break;
                case "type":
                        echo "Type";
                break;
                case "body":
                        echo "Body";
                break;
         }
?>
          </legend>
          <p class="text-center">
<?php
      echo $entry[$res];
?>
          </p>
<?php
    }
    echo "</fieldset>";
  }
?>
      </div>
<?php
  //Nous bouclons pour chaque module et nous ecrivons selon si l'element courant est un parametre/definitions/etc
  foreach($tabModuleR as $val){
    writeFile2("\nnamespace ".$val."\n", $nameOfFile,'lean');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "4"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->constants;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "4"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axioms;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "4"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theorems;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "4"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                if($tabFinal[$cpt][1]=="refl" || $tabFinal[$cpt][1]=="eq" || $tabFinal[$cpt][1]=="pred" || $tabFinal[$cpt][1]=="le" || $tabFinal[$cpt][1]=="lt" || $tabFinal[$cpt][1]=="decidable_lt" || $tabFinal[$cpt][1]=="decidable_le"){
                  writeFile2("\n\ttheorem ".$tabFinal[$cpt][1]. "_ : ".$entry2['statement']." := ".$entry2['proof']."\n", $nameOfFile,'lean');
                }
                else{
                  writeFile2("\n\ttheorem ".$tabFinal[$cpt][1]. " : ".$entry2['statement']." := ".$entry2['proof']."\n", $nameOfFile,'lean');
                }
              }
            }
            else
            {
              if($tabFinal[$cpt][1]=="refl" || $tabFinal[$cpt][1]=="eq" || $tabFinal[$cpt][1]=="pred" || $tabFinal[$cpt][1]=="le" || $tabFinal[$cpt][1]=="lt" || $tabFinal[$cpt][1]=="decidable_lt" || $tabFinal[$cpt][1]=="decidable_le"){
                writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. "_ : ".$entry2['statement']."\n", $nameOfFile,'lean');
              }
              else{
                writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. " : ".$entry2['statement']."\n", $nameOfFile,'lean');
              }
            }
            }
            else
            {
              if($tabFinal[$cpt][1]=="refl" || $tabFinal[$cpt][1]=="eq" || $tabFinal[$cpt][1]=="pred" || $tabFinal[$cpt][1]=="le" || $tabFinal[$cpt][1]=="lt" || $tabFinal[$cpt][1]=="decidable_lt" || $tabFinal[$cpt][1]=="decidable_le"){
                writeFile2("\n\tconstant ".$tabFinal[$cpt][1]. "_ : ".$entry2['type']."\n", $nameOfFile,'lean');
              }
              else{
                writeFile2("\n\tconstant ".$tabFinal[$cpt][1]. " : ".$entry2['type']."\n", $nameOfFile,'lean');
              }
            }
        }
        else{
          if($tabFinal[$cpt][1]=="refl" || $tabFinal[$cpt][1]=="eq" || $tabFinal[$cpt][1]=="pred" || $tabFinal[$cpt][1]=="le" || $tabFinal[$cpt][1]=="lt" || $tabFinal[$cpt][1]=="decidable_lt" || $tabFinal[$cpt][1]=="decidable_le"){
            writeFile2("\n\t".$entry2['kw']." ".$tabFinal[$cpt][1]. "_ : ".$entry2['type']." := ".$entry2['body']."\n", $nameOfFile,'lean');
          }
          else{
            writeFile2("\n\t".$entry2['kw']." ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['body']."\n", $nameOfFile,'lean');
          }
        }
      }
    }
    writeFile2("\nend ".$val."\n", $nameOfFile,'lean');
  }
?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
          <a class="btn btn-secondary btn-lg down-col" href="download/download.php?lang=lean">
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="pvs">
      <hr class="my-4">
      <img src="../picture/pvs-jumb.jpg" class="img-fluid image" alt="PVS-jumb">
      <hr class="my-4">
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."_".$_SESSION['tuple'][$id]['id'].".pvs";
  }
  else{
    $nameOfFile = $_GET['rechMd']."_".$_GET['rechId'].".pvs";
  }
  if(file_exists('download/pvs/'.$nameOfFile)){
    unlink('download/pvs/'.$nameOfFile);
  }
  $_SESSION['pvs'] = 'pvs/'.$nameOfFile;
?>
      <div class="container">
<?php
  $collection = $mongo->logipedia->$collect;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'id' => $_SESSION['tuple'][$id]['id'], 'sys' => "5"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'id' => $_GET['rechId'], 'sys' => "5"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof' && $res!="kw"){
?>
      <fieldset class="scheduler-border">
        <legend class="scheduler-border">
<?php
        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
          case "body":
                  echo "body";
          break;
        }
?>
        </legend>
        <p class="text-center">
<?php
      echo $entry[$res];
?>
        </p>
<?php
    }
    echo "</fieldset>";
  }
?>
      </div>
<?php
  //Nous bouclons pour chaque module et nous ecrivons selon si l'element courant est un parametre/definitions/etc
  foreach($tabModuleR as $val){
    writeFile2("\n".$val."_sttfa : THEORY\nBEGIN\n", $nameOfFile,'pvs');
    $collection = $mongo->logipedia->mdDep;
    $result2 = $collection->find(['isInTransClosure' => "false", 'md' => $val]);

    $n = 0;
    foreach ($result2 as $entry2) {
        if($entry2['mdDep']=="sttfa") {
        }
        else {
            writeFile2("IMPORTING ".$entry2['mdDep']."_sttfa AS ".$entry2['mdDep']."_sttfa_th\n", $nameOfFile,'pvs');

        }
    }
    writeFile2("\n", $nameOfFile,'pvs');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "5"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->constants;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "5"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axioms;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "5"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theorems;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'id' => $tabFinal[$cpt][1], 'sys' => "5"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                writeFile2("\n\t".$tabFinal[$cpt][1]." ".$entry2['statement']." : LEMMA ".$entry2['statement']."\n\t %|- ".$tabFinal[$cpt][1]." : PROOF ".$entry2['proof']."\n\t %|- QED \n", $nameOfFile,'pvs');
              }
            }
            else
            {
              writeFile2("\n\t".$tabFinal[$cpt][1]." ".$entry2['statement']." : AXIOM ".$entry2['statement']."\n", $nameOfFile,'pvs');
            }
            }
            else
            {
              writeFile2("\n\t".$tabFinal[$cpt][1]." ".$entry2['kw'].": ".$entry2['type']."\n", $nameOfFile,'pvs');
            }
        }
        else{
          writeFile2("\n\t".$tabFinal[$cpt][1]." ".$entry2['kw']." : ".$entry2['type']." = ".$entry2['body']."\n", $nameOfFile,'pvs');
        }
      }
    }
    writeFile2("\nEND ".$val."_sttfa\n", $nameOfFile,'pvs');
  }
?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
          <a class="btn btn-secondary btn-lg down-col" href="download/download.php?lang=pvs">
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>

    <div id="openTheory">
      <hr class="my-4">
      <img src="../picture/openTheory.png" class="img-fluid image" alt="OpenTheory">
      <hr class="my-4">
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."_".$_SESSION['tuple'][$id]['id'].".zip";
    $nameOfFile2 = $_SESSION['tuple'][$id]['md']."_".$_SESSION['tuple'][$id]['id'];
    $mod = $_SESSION['tuple'][$id]['md'];
  }
  else{
    $nameOfFile = $_GET['rechMd']."_".$_GET['rechId'].".zip";
    $nameOfFile2=$_GET['rechMd']."_".$_GET['rechId'];
    $mod = $_GET['rechMd'];
  }
  if(file_exists('download/openTheory/'.$nameOfFile)){
    unlink('download/openTheory/'.$nameOfFile);
  }
  $_SESSION['openTheory'] = 'openTheory/'.$nameOfFile;

  /*
        ARCHIVE
  */
$zip = new ZipArchive();
$filename = "download/openTheory/".$nameOfFile;

if ($zip->open($filename, ZipArchive::CREATE)!==TRUE) {
    exit("Impossible d'ouvrir le fichier <$filename>\n");
}

exec("python3 ./gen-thy-file.py ".$mod." > download/openTheory/".$mod.".thy");
$zip->addFile("download/openTheory/".$mod.".thy", $mod.".thy");
foreach($tabModuleR as $val){
    unset($result2);
    unset($entry2);
    $collection = $mongo->logipedia->openTheory;
    $result2 = $collection->find(['md' => $val], ['projection' => ['_id' => false]]);
    foreach($result2 as $entry2) {
        break ;
    }
    if($val=="nat") {
        unset($result2);
        $result2 = $collection->find(['md' => $val."0"], ['projection' => ['_id' => false]]);
        foreach ($result2 as $entry2) {
            break;
        }
        writeFile2($entry2['content'], $val.".art",'openTheory');
        unset($result2);
        $result2 = $collection->find(['md' => $val."1"], ['projection' => ['_id' => false]]);
        foreach ($result2 as $entry2) {
            break;
        }
        writeFile2($entry2['content'], $val.".art",'openTheory');
        $zip->addFile("download/openTheory/".$val.".art", $val.".art");
    }
    else if(empty($entry2['content']))
    {
        echo($val);
        die("This should not happen");
    }
    else
    {
        writeFile2($entry2['content'], $val.".art",'openTheory');
        $zip->addFile("download/openTheory/".$val.".art", $val.".art");
    }
}
set_time_limit(300);
$zip->close();
exec("rm download/openTheory/*.thy");
exec("rm download/openTheory/*.art");
?>

      <div class="container">
<?php
    $collection = $mongo->logipedia->$collect;
if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'id' => $_SESSION['tuple'][$id]['id'], 'sys' => "5"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
}
else
{
    $result = $collection->find(['md' => $_GET['rechMd'], 'id' => $_GET['rechId'], 'sys' => "5"], ['projection' => ['_id' => false, 'sys' => false, 'md' => false, 'id' => false]]);
}
foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
}
$keyP=array_keys($array);
foreach ($keyP as $res) {
    if($res!='proof' && $res!="kw"){
        ?>
        <fieldset class="scheduler-border">
        <legend class="scheduler-border">
<?php
        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
          case "body":
                  echo "body";
          break;
        }
?>
        </legend>
        <p class="text-center">
<?php
      echo "Printing for OpenTheory is not working at the moment.";
?>
        </p>
<?php
    }
    echo "</fieldset>";
  }
?>
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
