<?php
require '../../vendor/autoload.php';
$mongo = new MongoDB\Client('mongodb://localhost:27017');
$dossier = 'upload/';
$fichier = basename($_FILES['import']['name']);
$taille = filesize($_FILES['import']['tmp_name']);
$extensions = array('.json');
$extension = strrchr($_FILES['import']['name'], '.'); 
//Début des vérifications de sécurité...
if(!in_array($extension, $extensions)) //Si l'extension n'est pas dans le tableau
{
     $erreur = 'Vous devez uploader un fichier de type json !';
}
if(!isset($erreur)) //S'il n'y a pas d'erreur, on upload
{
     //On formate le nom du fichier ici...
     $fichier = strtr($fichier, 
          'ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðòóôõöùúûüýÿ', 
          'AAAAAACEEEEIIIIOOOOOUUUUYaaaaaaceeeeiiiioooooouuuuyy');
     $fichier = preg_replace('/([^.a-z0-9]+)/i', '-', $fichier);
     if(move_uploaded_file($_FILES['import']['tmp_name'], $dossier . $fichier)) //Si la fonction renvoie TRUE, c'est que ça a fonctionné...
     {
          $string = file_get_contents($dossier . $fichier);
          $json = json_decode($string,true);
          $compteur=0;
          if(count($json)>1){
            foreach ($json as $id => $item) {
                  if( $_GET['ref']=="axiomes" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('statement', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->axiomes;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
                   elseif($_GET['ref']=="parameters" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('type', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->parameters;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
                   elseif($_GET['ref']=="definitions" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('type', $item) && array_key_exists('statement', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->definitions;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
                   elseif($_GET['ref']=="theoremes" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('statement', $item) && array_key_exists('proof', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->theoremes;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
              }           
          }
          else{
            if( $_GET['ref']=="axiomes" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('statement', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->axiomes;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
                   elseif($_GET['ref']=="parameters" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('type', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->parameters;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
                   elseif($_GET['ref']=="definitions" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('type', $item) && array_key_exists('statement', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->definitions;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
                   elseif($_GET['ref']=="theoremes" && array_key_exists('md', $item) && array_key_exists('nameID', $item) && array_key_exists('statement', $item) && array_key_exists('proof', $item) && array_key_exists('langID', $item)){
                        $collection = $mongo->logipedia->theoremes;
                        $collection -> insertOne($item);
                        $compteur++;
                   }
          }
          if($compteur==0){
                echo "Insertion impossible, fichier json invalide !";
              }
          else{
            header('Location: ../index.php');
            exit;
          }
     }
     else //Sinon (la fonction renvoie FALSE).
     {
          echo 'Echec de l\'upload !';
     }
}
else
{
     echo $erreur;
}
?>
