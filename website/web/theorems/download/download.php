<?php
require '../vendor/autoload.php';
function writeFile($data,$filename,$sys)
{
    file_put_contents(dirname(__FILE__).'/download/'.$sys.'/'.$filename, $data, FILE_APPEND);
}

function sanitize_lean($id) {
    $reservedName=array("refl","eq","pred","le","lt","decidable_lt","deciable_le"]);
    if(in_array($id,$reservedName))
        return $id."_";
    }
    else {
        return $id;
    }

}

function writeDefinition($file,$system, $kw, $md, $id, $type, $body) {
    if ($system == "coq") {
        $data = "\n\t".$kw." ".$id. " : ".$type." := ".$body.".\n";
    }
    else if ($system == "matita") {
        $data = "\n\t".$kw." ".$id. " : ".$type." := ".$body.".\n";
    }
    else if ($system == "lean") {
        $data = "\n\t".$kw." ".sanitize_lean($id). " : ".$type." := ".$body."\n";
    }
    else if ($system == "pvs") {
        $data = "\n\t".$id." ".$kw." : ".$type." = ".$body."\n";
    }
    writeFile($data, $file, $system);
}

function writeAxiom($file,$system, $kw, $md, $id, $type) {
    if ($system == "coq") {
        $data = "\n\t".$kw." ".$id. " : ".$type.".\n";
    }
    else if ($system == "matita") {
        $data = "\n\t".$kw." ".$id. " : ".$type.".\n";
    }
    else if ($system == "lean") {
        $data = "\n\t".$kw." ".sanitize_lean($id). " : ".$type."\n";
    }
    else if ($system == "pvs") {
        $data = "\n\t".$id." ".$kw." : ".$type."\n";
    }
    writeFile($data, $file, $system);
}

function writeTheorem($file, $system, $kw, $md, $id, $type, $proof) {
    if ($system == "coq") {
        $data = "\n\t".$kw." ".$id." : ".$type." := ".$proof.".\n";
    }
    else if ($system == "matita") {
        $data = "\n\t".$kw." ".$id." : ".$type." := ".$proof.".\n";
    }
    else if ($system == "lean") {
        $data = "\n\t".$kw." ".sanitize_lean($id). " : ".$proof." := ".$proof."\n";
    }
    else if ($system == "pvs") {
        $data = "\n\t".$id." ".$kw." ".$type."\n\t %|- ".$id." : PROOF ".$proof."\n\t %|- QED \n";
    }
    writeFile($data, $file, $system);
}

function writeConstant($file, $system, $kw, $md, $id, $type) {
    if ($system == "coq") {
        $data = "\n\t".$kw." ".$id." : ".$type.".\n";
    }
    else if ($system == "matita") {
        $data = "\n\t".$kw." ".$id." : ".$type.".\n";
    }
    else if ($system == "lean") {
        $data = "\n\t".$kw." ".sanitize_lean($id)." : ".$type."\n";
    }
    else if ($system == "pvs") {
        $data = "\n\t".$id." ".$kw." : ".$type."\n";
    }
    writeFile($data, $file, $system);
}

function getExtension($sys) {
    if ($system == "coq") {
        return ".v";
    }
    else if ($system == "matita") {
        return ".ma";
    }
    else if ($system == "lean") {
        return ".lean";
    }
    else if ($system == "pvs") {
        return ".pvs";
    }
}

function getFileName($sys,$md,$id) {
    $path="download/".$sys;
    if(!is_dir($path)) {
        mkdir($path);
    }
    $ext=getExtension($sys);
    $file=$path."/".$md."_".$id.".".$ext;
    return $file;
}

function genFileCoq($md,$id) {
    $prevModule="";
    die("coucou");
}


  $file = $_SESSION['file'];
  switch ($_GET['lang']) {
    case 'coq':
        $file=$file.'.v';
        break;
    case 'matita':
        $file=$file.'.ma';
        break;
    case 'lean':
        $file=$file.'.lean';
        break;
    case 'pvs':
        $file=$file.'.pvs';
        break;
    case 'openTheory':
        $file=$file.'.zip';
        break;
    }
  $path = $_SESSION[$_GET['lang']];
  if(!file_exists($path)) die("I'm sorry, the file doesn't seem to exist.");

  $type = filetype($path);
  // Get a date and timestamp
  $today = date("F j, Y, g:i a");
  $time = time();
  // Send file headers
  header("Content-type: $type");
  header("Content-Disposition: attachment;filename=".$file);
  header("Content-Transfer-Encoding: binary");
  header('Pragma: no-cache');
  header('Expires: 0');
  // Send the file contents.
  set_time_limit(0);
  readfile($path);
?>
