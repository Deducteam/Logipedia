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
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js" integrity="sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm" crossorigin="anonymous"></script>
    
    <link rel="stylesheet" type="text/css" href="insert.css">
  </head>
  <body>
    <nav class="navbar navbar-expand-md bg-dark navbar-dark fixed-top">
      <div class="container">
        <a class="navbar-brand" href="../index.html">Logipedia</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="collapsibleNavbar">
          <ul class="navbar-nav">
            <li class="nav-item">
              <a class="nav-link" href="../language/recherche.php">Search</a>
            </li>
    <li class="nav-item dropdown">
        <a class="nav-link dropdown-toggle" href="#" id="navbarDropdown" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          Add
        </a>
        <div class="dropdown-menu" aria-labelledby="navbarDropdown">
          <a class="dropdown-item" href="insertionAx.php">Axiom</a>
          <a class="dropdown-item" href="insertionParam.php">Parameter</a>
          <a class="dropdown-item" href="insertionDef.php">Definition</a>
          <a class="dropdown-item" href="insertionTheo.php">Theorem</a>
        </div>
      </li>
          </ul>
        </div>
      </div>
    </nav>
</br>
</br>
</br>
</br>



<div class="container">
<div class="stepwizard">
    <div class="stepwizard-row setup-panel">
        <div class="stepwizard-step">
            <a href="#step-1" type="button" class="btn btn-primary btn-circle">1</a>
            <p>Step 1</p>
        </div>
        <div class="stepwizard-step">
            <a href="#step-2" type="button" class="btn btn-default btn-circle disabled">2</a>
            <p>Step 2</p>
        </div>
        <div class="stepwizard-step">
            <a href="#step-3" type="button" class="btn btn-default btn-circle disabled">3</a>
            <p>Step 3</p>
        </div>
    </div>
</div>
<form role="form" class="center_div" id="form1" method="POST">
    <div class="row setup-content" id="step-1">
        <div class="col-xs-12">
            <div class="col-md-12">
                <h3 class="text-center"> Name </h3>
                <div class="form-group">
                    <label class="control-label">Module :</label>
                    <input  maxlength="100" type="text" required="required" class="form-control" placeholder="Enter Module" name="module" />
                </div>
                <div class="form-group">
                    <label class="control-label">ID :</label>
                    <input maxlength="100" type="text" required="required" class="form-control" placeholder="Enter ID" name="id" />
                </div>
                 <div class="form-group">
    <label for="language">Language :</label>
    <select class="form-control" id="language" name="language">
      <option>Dedukti</option>
      <option>Coq</option>
      <option>Matita</option>
      <option>Pvs</option>
      <option>Lean</option>
      <option>OpenTheory</option>
    </select>
  </div>
                <button class="btn btn-primary nextBtn btn-lg pull-right" type="button" >Next</button>
            </div>
        </div>
    </div>
    <div class="row setup-content" id="step-2">
        <div class="col-xs-12">
            <div class="col-md-12">
                <h3> Statement </h3>
                <div class="form-group">
    <label for="statement">Statement :</label>
    <textarea class="form-control" required id="statement" name="statement" rows="3" placeholder="Enter Statement"></textarea>
  </div>
                <button class="btn btn-primary nextBtn btn-lg pull-right" type="button" >Next</button>
            </div>
        </div>
    </div>
    <div class="row setup-content" id="step-3">
        <div class="col-xs-12">
            <div class="col-md-12">
                <h3> Thank you !</h3>
                <button class="btn btn-success btn-lg pull-right" type="submit" name="submit">Finish!</button>
            </div>
        </div>
    </div>
</form>

</div>

<hr class="my-4">


<form method="POST" action="upload.php?ref=axiomes" enctype="multipart/form-data" class="form-inline center_div2">
  <div class="form-group mb-2">
    <label for="import">Fichier :</label>
  </div>
  <div class="form-group mx-sm-3 mb-2">
    <input type="file" class="form-control-file" id="import" name="import">
  </div>
  <button type="submit" class="btn btn-primary mb-2" name="envoyer">Confirm</button>
</form>
<?php
  if(isset($_POST['module']) && isset($_POST['id']) && isset($_POST['language']) && isset($_POST['statement']) && isset($_POST['submit'])){
    switch ($_POST['language']) {
    case 'Dedukti':
        $langID="1";
        break;
    case 'Coq':
        $langID="3";
        break;
    case 'Matita':
        $langID="2";
        break;
    case 'Pvs':
        $langID="5";
        break;
    case 'Lean':
        $langID="4";
        break;
    case 'OpenTheory':
        $langID="6";
        break;
}
    $collection = $mongo->logipedia->axiomes;
    $collection -> insertOne(array('langID' => $langID, 'statement' => $_POST['statement'], 'nameID' => $_POST['id'], 'md' => $_POST['module']));
  }
?>

     <script src="insert.js"></script>
  </body>
</html>
