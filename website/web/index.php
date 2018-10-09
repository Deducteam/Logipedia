<?php
  session_start();
  require 'vendor/autoload.php';
  $mongo = new MongoDB\Client('mongodb://localhost:27017'); //Acces au SGBD
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
    <nav class="navbar navbar-expand-md bg-dark navbar-dark fixed-top">
      <div class="container">
        <a class="navbar-brand" href="index.php"><i class="fas fa-award"></i> Logipedia</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="collapsibleNavbar">
          <ul class="navbar-nav">
            <li class="nav-item">
              <a class="nav-link" href="about/about.php">About</a>
            </li>
            <li class="nav-item">
              <a class="nav-link" href="about/modules.php">Modules</a>
            </li>
          </ul>
        </div>
      </div>
    </nav>

    <img src="picture/logipedia-jumb.jpg" class="img-fluid image" alt="Logipedia-Jumb">

    <hr class="my-4">
    <div class="form-group">
      <div class="row">
        <form method="post" class="col-md-12">
          <div class="row">
            <div class="col-md-3 col-sm-3 col-3"> </div>
            <input type="search" name="search_input" class="input-sm form-control col-md-5 col-sm-5 col-5" placeholder="Search" >
            <button type="submit" name="submit" class="btn btn-secondary btn-sm"><span class="glyphicon glyphicon-eye-open"></span> Search</button>
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
    $collection = $mongo->logipedia->definitions; //Recherche sur la collection definitions
    if ((isset($_POST['search_input']) && isset($_POST['submit'])) || isset($_GET['search']) || isset($_SESSION['search'])) { // Si une saisie a ete faite
      if(isset($_POST['search_input'])){
        $tabInp = explode(" ",$_POST['search_input']); // On recupere les mots
      }
      elseif (isset($_GET['search'])) {
        $tabInp = explode(" ",$_GET['search']);
      }
      else{
        $tabInp = explode(" ",$_SESSION['search']);
      }
      if(sizeof($tabInp)==1){ // Si un mot a ete saisie
        $result = $collection->aggregate([ // Envoie de la requête
          array(
            '$match' => array(
              '$or' => array(
                array("id" => $tabInp[0]),
                array("md" => $tabInp[0])
              )
            )
          ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);
        $compt=0;
        $boolea=0;
        $tab=[];
        foreach ($result as $entry) { //affichage des reponses
          if(isset($entry['_id'])){
           $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=definitions" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
            echo '<h4 class="h4-color"><b>'.$entry['_id']['md'].'.'.$entry['_id']['id'].'</b></h4>';
?>
        </a>
<?php
            $tab[$compt]=array('md'=>$entry['_id']['md'], 'id' => $entry['_id']['id']);
            $compt++;
          }
        }
        $result2 = $mongo->logipedia->axioms->aggregate([ // De meme pour la collection axiomes
          array(
            '$match' => array(
              '$or' => array(
                array("id" => $tabInp[0]),
                array("md" => $tabInp[0])
              )
            )
          ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);
        foreach ($result2 as $entry2) {
          if(isset($entry2['_id'])){
            $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=axioms" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
            echo '<h4 class="h4-color"><b>'.$entry2['_id']['md'].'.'.$entry2['_id']['id'].'</b></h4>';
?>
        </a>
<?php
            $tab[$compt]=array('md'=>$entry2['_id']['md'], 'id' => $entry2['_id']['id']);
            $compt++;
          }
        }
        $result3 = $mongo->logipedia->theorems->aggregate([ // De meme pour la collection theoremes
          array(
            '$match' => array(
              '$or' => array(
                array("id" => $tabInp[0]),
                array("md" => $tabInp[0])
              )
            )
          ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);
        foreach ($result3 as $entry3) {
          if(isset($entry3['_id'])){
            $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=theorems" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
            echo '<h4 class="h4-color"><b>'.$entry3['_id']['md'].'.'.$entry3['_id']['id'].'</b></h4>';
?>
        </a>
          <?php
            $tab[$compt]=array('md'=>$entry3['_id']['md'], 'id' => $entry3['_id']['id']);
            $compt++;
          }
        }
        $result4 = $mongo->logipedia->constants->aggregate([ // De meme pour la collection parameters
          array(
            '$match' => array(
              '$or' => array(
                array("id" => $tabInp[0]),
                array("md" => $tabInp[0])
              )
            )
          ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);
        foreach ($result4 as $entry4) {
          if(isset($entry4['_id'])){
            $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=constants" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
            echo '<h4 class="h4-color"><b>'.$entry4['_id']['md'].'.'.$entry4['_id']['id'].'</b></h4>';
?>
        </a>
<?php
            $tab[$compt]=array('md'=>$entry4['_id']['md'], 'id' => $entry4['_id']['id']);
            $compt++;
          }
        }
        if($boolea==0){
          echo '<h3 class="h3-color text-center">There is no result !</h3>';
        }
      }
      else // Si plusieurs mots ont ete saisis
      {
        $inpOne = $tabInp[0]; // on recupere le premier
        $inpTwo = $tabInp[1]; // et le second
        $result = $collection->aggregate([ // On envoie la requete avec le module et l'id
          array(
            '$match' => array(
              '$or' => array(
                array(
                  '$and' => array(
                    array("id" => $inpOne),
                    array("md" => $inpTwo)
                  )
                ),
                array(
                  '$and' => array(
                    array("id" => $inpTwo),
                    array("md" => $inpOne)
                  )
                ),
              )
            )
          ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);
        $tab=[];
        $compt=0;
        $boolea=0;
        foreach ($result as $entry) { // On affiche le resultat
          if(isset($entry['_id'])){
            $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=definitions" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
            echo '<h4 class="h4-color"><b>'.$entry['_id']['md'].'.'.$entry['_id']['id'].'</b></h4>';
?>
        </a>
<?php
            $tab[$compt]=array('md'=>$entry['_id']['md'], 'id' => $entry['_id']['id']);
            $compt++;
          }
        }
        $result2 = $mongo->logipedia->axioms->aggregate([ // On envoie la requete dans la collection axiomes
          array(
            '$match' => array(
              '$or' => array(
                array(
                  '$and' => array(
                    array("id" => $inpOne),
                    array("md" => $inpTwo)
                  )
                ),
                array(
                  '$and' => array(
                    array("id" => $inpTwo),
                    array("md" => $inpOne)
                  )
                ),
              )
            )
          ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);

          foreach ($result2 as $entry2) { // On affiche les resultats
            if(isset($entry2['_id'])){
              $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=axioms" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
              echo '<h4 class="h4-color"><b>'.$entry2['_id']['md'].'.'.$entry2['_id']['id'].'</b></h4>';
?>
        </a>
<?php
              $tab[$compt]=array('md'=>$entry2['_id']['md'], 'id' => $entry2['_id']['id']);
              $compt++;
            }
          }
          $result3 = $mongo->logipedia->theorems->aggregate([ // De meme pour theoremes
            array(
              '$match' => array(
                '$or' => array(
                  array(
                    '$and' => array(
                      array("id" => $inpOne),
                      array("md" => $inpTwo)
                    )
                  ),
                  array(
                    '$and' => array(
                      array("id" => $inpTwo),
                      array("md" => $inpOne)
                    )
                  ),
                )
              )
            ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);

            foreach ($result3 as $entry3) {
              if(isset($entry3['_id'])){
                $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=theorems" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
                echo '<h4 class="h4-color"><b>'.$entry3['_id']['md'].'.'.$entry3['_id']['id'].'</b></h4>';
?>
        </a>
<?php
                $tab[$compt]=array('md'=>$entry3['_id']['md'], 'id' => $entry3['_id']['id']);
                $compt++;
              }
            }
            $result4 = $mongo->logipedia->constants->aggregate([ // De meme pour parameters
              array(
                '$match' => array(
                  '$or' => array(
                    array(
                      '$and' => array(
                        array("id" => $inpOne),
                        array("md" => $inpTwo)
                      )
                    ),
                    array(
                      '$and' => array(
                        array("id" => $inpTwo),
                        array("md" => $inpOne)
                      )
                    ),
                  )
                )
              ),array('$group' => array('_id' => array('md' => '$md', 'id' => '$id'))), array('$sort' =>array('_id'=>1))]);

              foreach ($result4 as $entry4) {
                if(isset($entry4['_id'])){
                  $boolea++;
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=constants" class="list-group-item list-group-item-action text-center list-group-bg-mar">
<?php
                  echo '<h4 class="h4-color"><b>'.$entry4['_id']['md'].'.'.$entry4['_id']['id'].'</b></h4>';
?>
        </a>
<?php
                  $tab[$compt]=array('md'=>$entry4['_id']['md'], 'id' => $entry4['_id']['id']);
                  $compt++;
                }
              }
              if($boolea==0){
                echo '<h3 class="h3-color text-center">There is no result !</h3>';
              }
            }
            $_SESSION['tuple']=$tab; // On enregistre les tuples rechercher dans la session et on y accedera grace au compteur
            unset($_SESSION['search']);
    }
          else{
            $compt=0;
            unset($tab);
            unset($result);
            unset($entry);
            $tabCollection=array("definitions","theorems","constants","axioms");
            $collect=$tabCollection[rand(0,3)];
            $collection = $mongo->logipedia->$collect;
            $result = $collection->aggregate([
            [
              "\$sample" => ["size"=>20]
            ],
            [
              "\$group" =>  ["_id" => ["md" => "\$md", "id" => "\$id"], "unique" => ["\$addToSet"=> "\$should_be_unique"]]
            ]
            ]);
            foreach ($result as $entry) {
              if(isset($entry['_id']) && $compt<10){
?>
        <a href="theorems/theorems.php?id=<?php echo $compt;?>&collection=<?php echo $collect;?>" class="list-group-item list-group-item-action text-center list-group-item-light">
<?php
                echo '<h4 class="h4-color"><b>'.$entry['_id']['md'].'.'.$entry['_id']['id'].'</b></h4>';
?>
        </a>
<?php
                $tab[$compt]=array('md'=>$entry['_id']['md'], 'id' => $entry['_id']['id']);
                $compt++;
              }
            }
            $_SESSION['tuple']=$tab;
          }
?>
      </div>
    </div>
    </br>
    <?php
            }catch (MongoDB\Driver\Exception\ConnectionTimeoutException $e)
            {
              echo "Error";
            }
?>

       <script src="index.js"></script>
  </body>
</html>
