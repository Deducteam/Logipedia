<?php
  require 'vendor/autoload.php';
  $mongo = new MongoDB\Client('mongodb://localhost:27017/?readOnly=true'); //Acces au SGBD
  $mongo->logipedia->items->createIndex(["md" => "text", "id" => "text"]);
  function print_entry($md,$id, $kind)
  {
      echo '<a href="theorems/theorems.php?md='.$md.'&id='.$id.'&kind='.$kind.'" class="list-group-item list-group-item-action text-center list-group-bg-mar\">';
      echo '<h4 class="h4-color"><b>'.$md.'.'.$id.'</b></h4>';
      echo '</a>';
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
    <link rel="stylesheet" type="text/css" href="index.css">
  </head>
  <body>
      <?php require 'header.php' ?>
    <img src="picture/logipedia-jumb.jpg" class="img-fluid image" alt="Logipedia-Jumb">

    <hr class="my-4">
    <div class="form-group">
      <div class="row">
        <form method="get" class="col-md-12">
          <div class="row">
            <div class="col-md-3 col-sm-3 col-3"> </div>
            <input type="search" name="search" class="input-sm form-control col-md-5 col-sm-5 col-5" placeholder="Search" >
            <button type="submit" class="btn btn-secondary btn-sm"><span class="glyphicon glyphicon-eye-open"></span> Search</button>
          </div>
        </form>
      </div>
    </div>
    <hr class="my-4">

    <div class="container">
      <div class="list-group">
<?php
  try
  {
      $collection = $mongo->logipedia->items;
      if (isset($_GET['search'])) {
          $input = $_GET['search'];
          $result = $collection->find(['$text' => ['$search' => "$input", '$caseSensitive' => false]]);
          foreach ($result as $entry) {
              print_entry($entry['md'],$entry['id'],$entry['kind']);
          }
      }
      else{
          $result = $collection->aggregate([
              ['$sample' => ["size"=>10]]
          ]);
          foreach ($result as $entry) {
              print_entry($entry['md'],$entry['id'],$entry['kind']);
          }
      }
  }catch (MongoDB\Driver\Exception\ConnectionTimeoutException $e)
  {
      die("Connection to database failed!");
  }
?>
      </div>
    </div>
    </br>
  <script src="index.js"></script>
  </body>
</html>
