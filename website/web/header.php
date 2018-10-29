<?php
$index_path  = "/index.php";
$about_path  = "/about/about.php";
$module_path = "/about/modules.php";
$faq_path = "/about/faq.php";
?>
    <nav class="navbar navbar-expand-md bg-dark navbar-dark fixed-top">
      <div class="container">
        <a class="navbar-brand" href="<?php echo $index_path; ?>"><i class="fas fa-award"></i> Logipedia</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="collapsibleNavbar">
          <ul class="navbar-nav">
            <li class="nav-item">
              <a class="nav-link" href="<?php echo $module_path; ?>">Modules</a>
            </li>
            <li class="nav-item">
              <a class="nav-link" href="<?php echo $about_path; ?>">About</a>
            </li>
            <li class="nav-item">
              <a class="nav-link" href="<?php echo $faq_path; ?>">FAQ</a>
            </li>
          </ul>
        </div>
          <form class="form-inline my-2 my-lg-0" action="/index.php" method="get">
                <input class="form-control mr-sm-2 col-8" type="search" name="search" placeholder="Search" aria-label="Search">
                <button class="btn pull-right btn-outline-light my-2 my-sm-0 " type="submit">Search</button>
          </form>
      </div>
    </nav>
