<?php
require '../../vendor/autoload.php';

if(isset($_GET['md']) && isset($_GET['id']) && isset($_GET['sys'])) {
    $md=$_GET['md'];
    $id=$_GET['id'];
    $sys=$_GET['sys'];
    $file = "files/".$sys.".".$md.".".$id.".zip";
    if(!file_exists($file))
        die("The file does seem to exist or cannot be generated. This error should be reported");
    $type=filetype($file);
    header("Content-type: $type");
    header("Content-Disposition: attachment;filename=".$file);
    header("Content-Transfer-Encoding: binary");
    header('Pragma: no-cache');
    header('Expires: 0');
    // Send the file contents.
    set_time_limit(0);
    readfile($file);
}
else {
    die("hey");
}
?>
