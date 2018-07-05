<?php
  session_start();
  require '../../vendor/autoload.php';
  $mongo = new MongoDB\Client('mongodb://localhost:27017');
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
<?php
  //Fonction permettant de trouver toutes les dÃ©pendances de manieres recursive
  function Recursive($md,$id, &$tabRetour)
  {
    ob_start();
    $mongo = new MongoDB\Client('mongodb://localhost:27017');
    $collection = $mongo->logipedia->dependances;
    $result = $collection->find(['md' => $md, 'nameID' => $id], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    foreach ($result as $entry) {
      if(!is_null($entry['mdDep']) && !in_array(array($entry['mdDep'],$entry['idDep']), $tabRetour)){
        array_push($tabRetour, array($entry['mdDep'],$entry['idDep']));
        Recursive($entry['mdDep'],$entry['idDep'], $tabRetour);
      }
    }
    ob_end_clean();
  }
  //Fonction permettant de trouver toutes les dépendances des modules de manieres recursive
  function RecursiveMod($md,&$tabRetour,$tab2)
  {
    $mongo = new MongoDB\Client('mongodb://localhost:27017');
    $collection = $mongo->logipedia->dependancesMod;

    $result = $collection->find(['md' => $md], ['projection' => ['_id' => false]]);
    foreach ($result as $entry) {
      if(!is_null($entry['mdDep']) && !in_array($entry['mdDep'], $tabRetour) && in_array($entry['mdDep'],$tab2)){
        RecursiveMod($entry['mdDep'],$tabRetour,$tab2);
        array_push($tabRetour, $entry['mdDep']);
      }
    }
  }
  //Fonction permettant l'ecriture sur fichier
  function writeFile2($fichier_contenu,$fichier_nom,$lang)
  {
    file_put_contents(dirname(__FILE__).'/download/'.$lang.'/'.$fichier_nom, $fichier_contenu, FILE_APPEND);
  }
  //Fonction permettant d'écrire en début de fichier
  function writeFile($arg,$file)
  {
    $oldContents = file_get_contents($file);
    $fr = fopen($file, 'w');
    fwrite($fr, $arg);
    fwrite($fr, $oldContents);
    fclose($fr);
  }

  if(isset($_POST['search']) && isset($_POST['submit']) && !empty($_POST['search'])){
    $_SESSION['search'] = $_POST['search'];
    header("location:../index.php");
    die();
  }
?>

    <nav class="navbar navbar-expand-md bg-dark navbar-dark fixed-top">
      <div class="container">
        <a class="navbar-brand" href="../index.php"><i class="fas fa-award"></i> Logipedia</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="collapsibleNavbar">
          <ul class="navbar-nav mr-auto mt-2 mt-lg-0">
            <li class="nav-item">
              <a class="nav-link" href="../about.php">About</a>
            </li>
            <li class="nav-item dropdown">
              <a class="nav-link dropdown-toggle" href="#" id="navbarDropdown" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                Add <i class="fas fa-ban"></i>
              </a>
              <div class="dropdown-menu" aria-labelledby="navbarDropdown">
                <a class="dropdown-item" href="#">Axiom</a>
                <a class="dropdown-item" href="#">Parameter</a>
                <a class="dropdown-item" href="#">Definition</a>
                <a class="dropdown-item" href="#">Theorem</a>
              </div>
            </li>
          </ul>
          <form class="form-inline my-2 my-lg-0" method="post">
            <input class="form-control mr-sm-2 col-8" type="search" name="search" placeholder="Search" aria-label="Search">
            <button class="btn btn-outline-light my-2 my-sm-0 " type="submit" name="submit">Search</button>
          </form>
        </div>
      </div>
    </nav>

    <div id="mySidenav" class="sidenav d-none d-sm-block">
      <div class="container">
        <a href="#dedukti" id="a-dedukti">Dedukti &nbsp; &nbsp;<img src="../picture/dedukti.png" class="img-fluid" alt="Load"></i></a>
<?php
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $collect=(string)$_GET['collection'];
    $id=(int)$_GET['id'];
    $collection = $mongo->logipedia->$collect;
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID']]);
  }
  else{
    $collection = $mongo->logipedia->definitions;
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false]]);
    foreach ($result as $entry) {
      break;
    }
    if(empty($entry['md']))
    {
      $collection = $mongo->logipedia->parameters;
      $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false]]);
      foreach ($result as $entry) {
        break;
      }
      if(empty($entry['md']))
      {
        $collection = $mongo->logipedia->axiomes;
        $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false]]);
        foreach ($result as $entry) {
          break;
        }
        if(empty($entry['md']))
        {
          $collection = $mongo->logipedia->theoremes;
          $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false]]);
          foreach ($result as $entry) {
            break;
          }
          if(empty($entry['md']))
          {
            echo "ref inexistante";
          }
          else
          {
            $collect='theoremes';
          }
        }
        else
        {
          $collect='axiomes';
        }
      }
      else
      {
        $collect='parameters';
      }
    }
    else{
      $collect='definitions';
    }
    $collection = $mongo->logipedia->$collect;
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']]);
  }
  foreach ($result as $entry) {
    $type = $entry['langID'];
    if($type == '2')
    {
      echo '<a href="#matita" id="a-matita">Matita &nbsp; &nbsp; &nbsp;<img src="../picture/matita.png" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>';
    }
    if($type == '3')
    {
      echo '<a href="#coq" id="a-coq">Coq &nbsp; &nbsp; &nbsp; &nbsp; <img src="../picture/coq.png" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>';
    }
    if($type == '4')
    {
      echo '<a href="#lean" id="a-lean">Lean &nbsp; &nbsp; &nbsp; &nbsp;<img src="../picture/lean.jpg" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>';
    }
    if($type == '5')
    {
      echo '<a href="#pvs" id="a-pvs">PVS &nbsp; &nbsp; &nbsp; &nbsp; <img src="../picture/pvs.jpg" class="img-fluid" alt="Load" style="width:50px;height:50px;"></a>';
    }
  }
?>
      </div>
    </div>
    <div id="dedukti">
      <img src="../picture/dedukti-jumb.jpg" class="img-fluid image" alt="Responsive image">
      <div id='attente' class="text-center">
        <h1>Please wait...</h1>
      </div>
      <hr class="my-4">
      <h1 class="display-5 text-center"><b>
<?php
  switch ($collect) {
    case "definitions":
      $collect2="Definition";
      break;
    case "theoremes":
      $collect2="Theorem";
      break;
    case "parameters":
      $collect2="Parameter";
      break;
    case "axiomes":
      $collect2="Axiom";
      break;
  }
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    echo $collect2." of ".$_SESSION['tuple'][$id]['md'].".".$_SESSION['tuple'][$id]['nameID'];
  }
  else
  {
    echo $collect2." of ".$_GET['rechMd'].".".$_GET['rechId'];
  }
?>
      </b></h1>
      <hr class="my-4">
      <div class="container">
<?php
  $collection = $mongo->logipedia->$collect;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID'], 'langID' => "1"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId'], 'langID' => "1"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof'){
?>
        <fieldset class="scheduler-border">
          <legend class="scheduler-border">
<?php
      if($res=="statement" && $collect=="definitions"){
        echo "Body";
      }
      else{
        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
        }
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
  $collection = $mongo->logipedia->dependances;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    $resultTmp = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
  }
  else{
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    $resultTmp = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
  }
  if(count($result->toArray())>0){
    if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
      $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    }
    else{
      $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    }
    $tabDefinitions=[];
    $tabTheorems=[];
    $tabParameters=[];
    $tabAxioms=[];

    $compteur=0;
    $tabRetour=[];
    foreach ($result as $entry) {
      Recursive($entry['mdDep'],$entry['idDep'], $tabRetour);
      unset($result2);
      unset($entry2);
      $collection = $mongo->logipedia->axiomes;
      $result2 = $collection->find(['md' => $entry['mdDep'], 'nameID' => $entry['idDep']], ['projection' => ['_id' => false]]);
      foreach ($result2 as $entry2) {
        break;
      }
      if(!empty($entry2['md']))
      {
        array_push($tabAxioms,array($entry['mdDep'],$entry['idDep']));
        $compteur++;
      }
      unset($result2);
      unset($entry2);
      $collection = $mongo->logipedia->parameters;
      $result2 = $collection->find(['md' => $entry['mdDep'], 'nameID' => $entry['idDep']], ['projection' => ['_id' => false]]);
      foreach ($result2 as $entry2) {
        break;
      }
      if(!empty($entry2['md']))
      {
        $compteur++;
        array_push($tabParameters,array($entry['mdDep'],$entry['idDep']));
      }
      unset($result2);
      unset($entry2);
      $collection = $mongo->logipedia->definitions;
      $result2 = $collection->find(['md' => $entry['mdDep'], 'nameID' => $entry['idDep']], ['projection' => ['_id' => false]]);
      foreach ($result2 as $entry2) {
        break;
      }
      if(!empty($entry2['md']))
      {
        $compteur++;
        array_push($tabDefinitions,array($entry['mdDep'],$entry['idDep']));
      }
      unset($result2);
      unset($entry2);
      $collection = $mongo->logipedia->theoremes;
      $result2 = $collection->find(['md' => $entry['mdDep'], 'nameID' => $entry['idDep']], ['projection' => ['_id' => false]]);
      foreach ($result2 as $entry2) {
        break;
      }
      if(!empty($entry2['md']))
      {
        $compteur++;
        array_push($tabTheorems,array($entry['mdDep'],$entry['idDep']));
      }
    }
    $collection = $mongo->logipedia->dependances;
    if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
      $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    }
    else{
      $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false, 'md' => false, 'nameID' => false]]);
    }
   $comptSupp=0;
   $tabRetour2 = $tabRetour;
   // tableau qui contiendra les dépendance indirectes, puis les dependances direct puis la declaration ou definition en cours d'analyse (dans cette ordre pour permettre que le fichier type check)
   $tabFinal=[];
   $depSupp=[];
   foreach ($result as $entry) {
     // Si la dependance actuelles est dans notre tableau de dependances nous l'ajoutons a notre tableau depSupp
     if (($key = array_search(array($entry['mdDep'],$entry['idDep']), $tabRetour)) !== false) {
       $depSupp[$comptSupp]=array($entry['mdDep'],$entry['idDep']);
       $comptSupp++;
       unset($tabRetour[$key]);
      }
    }
     if(count($resultTmp->toArray())>10){
?>
      <div class="container">
        <fieldset class="scheduler-border">
          <legend class="scheduler-border"> Dependence </legend>
          <div class="card">
            <div class="card-header" id="headingOne">
                <a class="list-group-item list-group-item-action text-center" data-toggle="collapse" data-target="#collapseOne" aria-expanded="true" aria-controls="collapseOne" id="btHide">
                <i class="fas fa-chevron-down" id="iconDep"></i>
                </a>
            </div>
            <div id="collapseOne" class="collapse" aria-labelledby="headingOne" data-parent="#accordion">
              <div class="row">
                <div class="card col-md-3">
                  <div class="card-header text-center">Axioms</div>
                  <div class="list-group">
<?php
      if(isset ($tabAxioms)){
        foreach ($tabAxioms as $ax)
        {
           echo '<a href="theorems.php?rechMd='.$ax[0].'&rechId='.$ax[1].'" class="list-group-item list-group-item-action text-center">'.$ax[0].".".$ax[1].'</a> </br>';
        }
      }
?>
                  </div>
                </div>
                <div class="card col-md-3">
                  <div class="card-header text-center">Parameters</div>
                  <div class="list-group">
<?php
      if(isset ($tabParameters)){
        foreach ($tabParameters as $para)
        {
           echo '<a href="theorems.php?rechMd='.$para[0].'&rechId='.$para[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center">'.$para[0].".".$para[1].'</a> </br>';
        }
      }
?>
                  </div>
                </div>
                <div class="card col-md-3">
                  <div class="card-header text-center">Definitions</div>
                  <div class="list-group">
<?php
      if(isset ($tabDefinitions)){
        foreach ($tabDefinitions as $def)
        {
           echo '<a href="theorems.php?rechMd='.$def[0].'&rechId='.$def[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center">'.$def[0].".".$def[1].'</a> </br>';
        }
      }
?>
                  </div>
                </div>
                <div class="card col-md-3">
                  <div class="card-header text-center">Theorems</div>
                  <div class="list-group">
<?php
      if(isset ($tabTheorems)){
        foreach ($tabTheorems as $theo)
        {
           echo '<a href="theorems.php?rechMd='.$theo[0].'&rechId='.$theo[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center">'.$theo[0].".".$theo[1].'</a> </br>';
        }
      }
?>
                  </div>
                </div>
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
                <i class="fas fa-chevron-down"></i>
              </a>
            </div>
            <div id="collapseTwo" class="collapse" aria-labelledby="headingTwo" data-parent="#accordion">
              <div class="card-body">
                <ul class="list-group">
<?php
      $compteur2=0;
      foreach ($tabRetour as $dep)
      {
        unset($result2);
        unset($entry2);
        $collection = $mongo->logipedia->axiomes;
        $result2 = $collection->find(['md' => $dep[0], 'nameID' => $dep[1]], ['projection' => ['_id' => false]]);
        foreach ($result2 as $entry2) {
          break;
        }
        if(!empty($entry2['md']))
        {
          echo '<a href="theorems.php?rechMd='.$dep[0].'&rechId='.$dep[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$dep[0].".".$dep[1].'</a> </br>';
          $compteur2++;
        }
        unset($result2);
        unset($entry2);
        $collection = $mongo->logipedia->parameters;
        $result2 = $collection->find(['md' => $dep[0], 'nameID' => $dep[1]], ['projection' => ['_id' => false]]);
        foreach ($result2 as $entry2) {
          break;
        }
        if(!empty($entry2['md']))
        {
          echo '<a href="theorems.php?rechMd='.$dep[0].'&rechId='.$dep[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$dep[0].".".$dep[1].'</a> </br>';
          $compteur2++;
        }
      }
      foreach($tabParameters as $parAx)
      {
        echo '<a href="theorems.php?rechMd='.$parAx[0].'&rechId='.$parAx[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$parAx[0].".".$parAx[1].'</a> </br>';
        $compteur2++;
      }
      foreach($tabAxioms as $tAx)
      {
        echo '<a href="theorems.php?rechMd='.$tAx[0].'&rechId='.$tAx[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$tAx[0].".".$tAx[1].'</a> </br>';
        $compteur2++;
      }
      if($compteur2==0)
      {
        echo '<a href="#" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" > Aucune dependances indirecte ! </a> </br>';
      }
?>
                </ul>
              </div>
            </div>
          </div>
        </fieldset>
      </div>
<?php
    }
    else{
?>
      <div class="container">
        <fieldset class="scheduler-border">
          <legend class="scheduler-border"> Dependence </legend>
          <div class="row">
            <div class="card col-md-3">
              <div class="card-header text-center">Axioms</div>
              <div class="list-group">
<?php
      if(isset ($tabAxioms)){
        foreach ($tabAxioms as $ax)
        {
           echo '<a href="theorems.php?rechMd='.$ax[0].'&rechId='.$ax[1].'" class="list-group-item list-group-item-action text-center">'.$ax[0].".".$ax[1].'</a> </br>';
        }
      }
?>
              </div>
            </div>
            <div class="card col-md-3">
              <div class="card-header text-center">Parameters</div>
              <div class="list-group">
<?php
      if(isset ($tabParameters)){
        foreach ($tabParameters as $para)
        {
           echo '<a href="theorems.php?rechMd='.$para[0].'&rechId='.$para[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center">'.$para[0].".".$para[1].'</a> </br>';
        }
      }
?>
              </div>
            </div>
            <div class="card col-md-3">
              <div class="card-header text-center">Definitions</div>
              <div class="list-group">
<?php
      if(isset ($tabDefinitions)){
        foreach ($tabDefinitions as $def)
        {
           echo '<a href="theorems.php?rechMd='.$def[0].'&rechId='.$def[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center">'.$def[0].".".$def[1].'</a> </br>';
        }
      }
?>
              </div>
            </div>
            <div class="card col-md-3">
              <div class="card-header text-center">Theorems</div>
              <div class="list-group">
<?php
      if(isset ($tabTheorems)){
        foreach ($tabTheorems as $theo)
        {
           echo '<a href="theorems.php?rechMd='.$theo[0].'&rechId='.$theo[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center">'.$theo[0].".".$theo[1].'</a> </br>';
        }
      }
?>
              </div>
            </div>
          </div>
        </fieldset>
      </div>
      <div class="container">
        <fieldset class="scheduler-border">
          <legend class="scheduler-border"> Theory </legend>
          <ul class="list-group">
<?php
      $compteur2=0;
      foreach ($tabRetour as $dep)
      {
        unset($result2);
        unset($entry2);
        $collection = $mongo->logipedia->axiomes;
        $result2 = $collection->find(['md' => $dep[0], 'nameID' => $dep[1]], ['projection' => ['_id' => false]]);
        foreach ($result2 as $entry2) {
          break;
        }
        if(!empty($entry2['md']))
        {
          echo '<a href="theorems.php?rechMd='.$dep[0].'&rechId='.$dep[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$dep[0].".".$dep[1].'</a> </br>';
          $compteur2++;
        }
        unset($result2);
        unset($entry2);
        $collection = $mongo->logipedia->parameters;
        $result2 = $collection->find(['md' => $dep[0], 'nameID' => $dep[1]], ['projection' => ['_id' => false]]);
        foreach ($result2 as $entry2) {
          break;
        }
        if(!empty($entry2['md']))
        {
          echo '<a href="theorems.php?rechMd='.$dep[0].'&rechId='.$dep[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$dep[0].".".$dep[1].'</a> </br>';
          $compteur2++;
        }
      }
      foreach($tabParameters as $parAx)
      {
        echo '<a href="theorems.php?rechMd='.$parAx[0].'&rechId='.$parAx[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$parAx[0].".".$parAx[1].'</a> </br>';
        $compteur2++;
      }
      foreach($tabAxioms as $tAx)
      {
        echo '<a href="theorems.php?rechMd='.$tAx[0].'&rechId='.$tAx[1].'" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" >'.$tAx[0].".".$tAx[1].'</a> </br>';
        $compteur2++;
      }
      if($compteur2==0)
      {
        echo '<a href="#" class="list-group-item list-group-item-action list-group-bg-mar2 text-center" > Aucune dependances indirecte ! </a> </br>';
      }
?>
          </ul>
        </fieldset>
      </div>
<?php
    }
  }
?>
    </div>
<?php
  unset($dep);
  foreach ($tabRetour2 as $dep)
  {
    if($dep[0]!="sttfa"){ //Nous n'ajoutons pas STTFA dans le fichier
      array_push($tabFinal,array($dep[0],$dep[1])); // Nous ajoutons les dependances indirecte au tableau final
    }
  }
  $collection = $mongo->logipedia->dependances;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID']], ['projection' => ['_id' => false]]);
  }
  else{
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId']], ['projection' => ['_id' => false]]);
  }
  foreach ($result as $entry) {
    if($entry['mdDep']!="sttfa" && !in_array(array($entry['mdDep'],$entry['idDep']),$depSupp)){
      array_push($tabFinal,array($entry['mdDep'],$entry['idDep'])); // Nous ajoutons les dependances direct
    }
  }
  $collection = $mongo->logipedia->$collect;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
	  $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID'], 'langID' => "3"], ['projection' => ['_id' => false, 'langID' => false]]);
    $nameID = $_SESSION['tuple'][$id]['nameID'];
    $mdID=$_SESSION['tuple'][$id]['md'];
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId'], 'langID' => "3"], ['projection' => ['_id' => false, 'langID' => false]]);
    $nameID = $_GET['rechId'];
    $mdID = $_GET['rechMd'];
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  array_push($tabFinal,array($entry['md'],$entry['nameID'])); // Nous ajoutons la declaration ou definition courante
  $tabModule=[];
  for($i=0;$i<sizeof($tabFinal);$i++){
    $tabModule[$i]=$tabFinal[$i][0]; // Nous recuperons tout les modules
  }
  $tabModuleNoDB=array_unique($tabModule); // Nous retirons les doublons
  $tabModuleR=[]; //Tableau contenant les dependances des modules
  foreach($tabModuleNoDB as $mod){
    RecursiveMod($mod,$tabModuleR,$tabModuleNoDB);
  }
  // Nous inversons le tableau pour faciliter l'ajout des modules manquants
  $tabModuleR=array_reverse($tabModuleR);
  if(sizeof($tabModuleR) != sizeof($tabModuleNoDB)){
    $tabModuleR= array_merge(array_diff($tabModuleNoDB,$tabModuleR),$tabModuleR); //Nous ajoutons les modules manquants
  }
  $tabModuleR=array_reverse($tabModuleR); // Nous inversons le tableau pour faciliter l'ecriture sur fichier
?>
    <div id="coq">
      <hr class="my-4">
      <img src="../picture/coq-jumb.jpg" class="img-fluid image" alt="Responsive image">
      <hr class="my-4">
<?php
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."-".$_SESSION['tuple'][$id]['nameID'].".v";
    $_SESSION['file'] = $_SESSION['tuple'][$id]['md']."-".$_SESSION['tuple'][$id]['nameID'];
  }
  else{
    $nameOfFile = $_GET['rechMd']."-".$_GET['rechId'].".v";
    $_SESSION['file'] = $_GET['rechMd']."-".$_GET['rechId'];
  }
  if(file_exists('download/coq/'.$nameOfFile)){
    unlink('download/coq/'.$nameOfFile);
  }
  $_SESSION['coq'] = 'coq/'.$nameOfFile;
?>
      <div class="container">
<?php
  $collection = $mongo->logipedia->$collect;
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID'], 'langID' => "3"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId'], 'langID' => "3"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof'){
?>
      <fieldset class="scheduler-border">
        <legend class="scheduler-border">
<?php
      if($res=="statement" && $collect=="definitions"){
        echo "Body";
      }
      else{
        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
        }
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
    writeFile2("\nModule ".$val.".\n", $nameOfFile,'coq');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "3"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->parameters;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "3"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axiomes;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "3"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theoremes;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "3"], ['projection' => ['_id' => false]]);
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
          writeFile2("\n\tDefinition ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['statement'].".\n", $nameOfFile,'coq');
        }
      }
    }
    writeFile2("\nEnd ".$val.".\n", $nameOfFile,'coq');
  }
?>
      </br>
      <div class="container">
        <div class="col-md-12 text-center">
          <a class="btn btn-secondary btn-lg down-col" href="download/download.php?lang=coq">
            <i class="fas fa-file-download"></i>
          </a>
        </div>
      </div>
    </div>
    <div id="matita">
      <hr class="my-4">
      <img src="../picture/matita-jumb.jpg" class="img-fluid image" alt="Responsive image">
      <hr class="my-4">
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."-".$_SESSION['tuple'][$id]['nameID'].".ma";
  }
  else{
    $nameOfFile = $_GET['rechMd']."-".$_GET['rechId'].".ma";
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
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID'], 'langID' => "2"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId'], 'langID' => "2"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof'){
?>
        <fieldset class="scheduler-border">
          <legend class="scheduler-border">
<?php
      if($res=="statement" && $collect=="definitions"){
        echo "Body";
      }
      else{
        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
        }
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
    writeFile2("\nModule ".$val.".\n", $nameOfFile,'matita');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "2"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->parameters;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "2"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axiomes;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "2"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theoremes;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "2"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                writeFile2("\n\tdefinition ".$tabFinal[$cpt][1]. " : ".$entry2['statement']." := ".$entry2['proof'].".\n", $nameOfFile,'matita');
              }
            }
            else
            {
              writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. " : ".$entry2['statement'].".\n", $nameOfFile,'matita');
            }
            }
            else
            {
              writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. " : ".$entry2['type'].".\n", $nameOfFile,'matita');
            }
        }
        else{
          writeFile2("\n\tdefinition ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['statement'].".\n", $nameOfFile,'matita');
        }
      }
    }
    writeFile2("\nEnd ".$val.".\n", $nameOfFile,'matita');
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
      <img src="../picture/lean-jumb.jpg" class="img-fluid image" alt="Responsive image">
      <hr class="my-4">
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."-".$_SESSION['tuple'][$id]['nameID'].".lean";
  }
  else{
    $nameOfFile = $_GET['rechMd']."-".$_GET['rechId'].".lean";
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
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID'], 'langID' => "4"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId'], 'langID' => "4"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof' && $res!='computable'){
?>
        <fieldset class="scheduler-border">
          <legend class="scheduler-border">
<?php
      if($res=="statement" && $collect=="definitions"){
        echo "Body";
      }
      else{
      switch ($res) {
                case "statement":
                        echo "Statement";
                break;
                case "type":
                        echo "Type";
                break;
         }
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
    writeFile2("\nModule ".$val.".\n", $nameOfFile,'lean');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "4"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->parameters;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "4"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axiomes;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "4"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theoremes;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "4"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                writeFile2("\n\ttheorem ".$tabFinal[$cpt][1]. " : ".$entry2['statement']." := ".$entry2['proof'].".\n", $nameOfFile,'lean');
              }
            }
            else
            {
              writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. " : ".$entry2['statement'].".\n", $nameOfFile,'lean');
            }
            }
            else
            {
              writeFile2("\n\tconstant ".$tabFinal[$cpt][1]. " : ".$entry2['type'].".\n", $nameOfFile,'lean');
            }
        }
        else{
          writeFile2("\n\t".$entry2['computable']." ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['statement'].".\n", $nameOfFile,'lean');
        }
      }
    }
    writeFile2("\nEnd ".$val.".\n", $nameOfFile,'lean');
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
      <img src="../picture/pvs-jumb.jpg" class="img-fluid image" alt="Responsive image">
      <hr class="my-4">
<?php
  unset($result);
  unset($entry);
  unset($nameOfFile);
  if(!isset($_GET['rechMd']) && !isset($_GET['rechId'])){
    $nameOfFile = $_SESSION['tuple'][$id]['md']."-".$_SESSION['tuple'][$id]['nameID'].".pvs";
  }
  else{
    $nameOfFile = $_GET['rechMd']."-".$_GET['rechId'].".pvs";
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
    $result = $collection->find(['md' => $_SESSION['tuple'][$id]['md'], 'nameID' => $_SESSION['tuple'][$id]['nameID'], 'langID' => "5"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  else
  {
    $result = $collection->find(['md' => $_GET['rechMd'], 'nameID' => $_GET['rechId'], 'langID' => "5"], ['projection' => ['_id' => false, 'langID' => false, 'md' => false, 'nameID' => false]]);
  }
  foreach ($result as $entry) {
    $array =  (array) $entry;
    break;
  }
  $keyP=array_keys($array);
  foreach ($keyP as $res) {
    if($res!='proof'){
?>
      <fieldset class="scheduler-border">
        <legend class="scheduler-border">
<?php
      if($res=="statement" && $collect=="definitions"){
        echo "Body";
      }
      else{
        switch ($res) {
          case "statement":
                  echo "Statement";
          break;
          case "type":
                  echo "Type";
          break;
        }
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
    writeFile2("\nModule ".$val.".\n", $nameOfFile,'pvs');
    for($cpt=0;$cpt<sizeof($tabFinal);$cpt++){
      unset($result2);
      unset($entry2);
      if($tabFinal[$cpt][0]==$val){
        $collection = $mongo->logipedia->definitions;
        $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "5"], ['projection' => ['_id' => false]]);
        $entry2=[];
        foreach ($result2 as $entry2) {
          break;
        }
        if(empty($entry2['md']))
        {
          $collection = $mongo->logipedia->parameters;
          $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "5"], ['projection' => ['_id' => false]]);
          foreach ($result2 as $entry2) {
            break;
          }
          if(empty($entry2['md']))
          {
            $collection = $mongo->logipedia->axiomes;
            $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "5"], ['projection' => ['_id' => false]]);
            foreach ($result2 as $entry2) {
              break;
            }
            if(empty($entry2['md']))
            {
              $collection = $mongo->logipedia->theoremes;
              $result2 = $collection->find(['md' => $tabFinal[$cpt][0], 'nameID' => $tabFinal[$cpt][1], 'langID' => "5"], ['projection' => ['_id' => false]]);
              foreach ($result2 as $entry2) {
                break;
              }
              if(empty($entry2['md']))
              {
                echo "";
              }
              else
              {
                writeFile2("\n\tdefinition ".$tabFinal[$cpt][1]. " : ".$entry2['statement']." := ".$entry2['proof'].".\n", $nameOfFile,'pvs');
              }
            }
            else
            {
              writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. " : ".$entry2['statement'].".\n", $nameOfFile,'pvs');
            }
            }
            else
            {
              writeFile2("\n\taxiom ".$tabFinal[$cpt][1]. " : ".$entry2['type'].".\n", $nameOfFile,'pvs');
            }
        }
        else{
          writeFile2("\n\tdefinition ".$tabFinal[$cpt][1]. " : ".$entry2['type']." := ".$entry2['statement'].".\n", $nameOfFile,'pvs');
        }
      }
    }
    writeFile2("\nEnd ".$val.".\n", $nameOfFile,'pvs');
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

    <script src="theorems.js"></script>
  </body>
</html>
