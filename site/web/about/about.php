<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Logipedia</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css" integrity="sha384-9gVQ4dYFwwWSjIDZnLEWnxCjeSWFphJiwGPXr1jddIhOegiu1FwO5qRGvFXOdJZ4" crossorigin="anonymous">
    <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.1.0/css/all.css" integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt" crossorigin="anonymous">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js" integrity="sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm" crossorigin="anonymous"></script>
    <link rel="stylesheet" type="text/css" href="about.css">
  </head>
  <body>
    <nav class="navbar navbar-expand-md bg-dark navbar-dark fixed-top">
      <div class="container">
        <a class="navbar-brand" href="../index.php"><i class="fas fa-award"></i> Logipedia</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="collapsibleNavbar">
          <ul class="navbar-nav">
            <li class="nav-item">
              <a class="nav-link" href="about.php">About</a>
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
        </div>
      </div>
    </nav>

    <div class="container">
      <hr class="my-4">
      <div class="page-header">
        <h1 id="timeline" class="text-center">Five things you need to know about <a href="../index.php" style="color:grey;"> Logipedia </a></h1>
      </div>
      <hr class="my-4">
      <ul class="timeline">
        <li>
          <div class="timeline-badge"><i class="fas fa-dice-one"></i></div>
          <div class="timeline-panel">
            <div class="timeline-heading">
              <h4 class="timeline-title"> <a href="../index.php" style="color:grey;">Logipedia</a> is a library of proofs expressed in <a href="https://deducteam.github.io/" style="color:grey;">Dedukti</a>.</h4>
            </div>
            <div class="timeline-body">
              <p></p>
            </div>
          </div>
        </li>
        <li class="timeline-inverted">
          <div class="timeline-badge warning"><i class="fas fa-dice-two"></i></div>
          <div class="timeline-panel">
            <div class="timeline-heading">
              <h4 class="timeline-title"><a href="https://deducteam.github.io/" style="color:grey;">Dedukti</a> is a Logical Framework that provides means to define theories such as <a href="https://en.wikipedia.org/wiki/First-order_logic" style="color:grey;">Predicate logic</a>, <a href="https://en.wikipedia.org/wiki/Type_theory" style="color:grey;">Simple type theory</a>, <a href="https://en.wikipedia.org/wiki/Calculus_of_constructions" style="color:grey;">the Calculus of constructions</a>, the Calculus of Inductive Constructions... and to express proofs in these theories.</h4>
            </div>
            <div class="timeline-body">
              <p></p>
            </div>
          </div>
        </li>
        <li>
          <div class="timeline-badge danger"><i class="fas fa-dice-three"></i></div>
          <div class="timeline-panel">
            <div class="timeline-heading">
              <h4 class="timeline-title">Within <a href="https://deducteam.github.io/" style="color:grey;">Dedukti</a>, some proofs can be translated from one theory to another.</h4>
            </div>
            <div class="timeline-body">
              <p></p>
            </div>
          </div>
        </li>
        <li class="timeline-inverted">
          <div class="timeline-badge success"><i class="fas fa-dice-four"></i></div>
          <div class="timeline-panel">
            <div class="timeline-heading">
              <h4 class="timeline-title">Some proofs expressed in some <a href="https://deducteam.github.io/" style="color:grey;">Dedukti</a> theories can be translated to other proof systems, such as <a href="http://www.cl.cam.ac.uk/~jrh13/hol-light/" style="color:grey;">HOL Light</a>, <a href="https://hol-theorem-prover.org/" style="color:grey;"> HOL 4 </a>, <a href="https://isabelle.in.tum.de/overview.html" style="color:grey;"> Isabelle / HOL </a>, <a href="https://coq.inria.fr/" style="color:grey;"> Coq </a>, <a href="http://matita.cs.unibo.it/" style="color:grey;"> Matita </a>, <a href="https://leanprover.github.io/" style="color:grey;"> Lean </a>, <a href="http://pvs.csl.sri.com/" style="color:grey;"> PVS </a>...</h4>
            </div>
            <div class="timeline-body">
              <p></p>
            </div>
          </div>
        </li>
        <li>
          <div class="timeline-badge info"><i class="fas fa-dice-five"></i></div>
          <div class="timeline-panel">
            <div class="timeline-heading">
              <h4 class="timeline-title"><a href="../index.php?thm=fermat" style="color:grey;"> This proof of Fermat's little theorem </a>, originally developed in <a href="http://matita.cs.unibo.it/" style="color:grey;"> Matita </a>, has been expressed in a formulation of the Calculus of Inductive Constructions in <a href="https://deducteam.github.io/" style="color:grey;">Dedukti</a>, it has then been translated to a formulation of Simple type theory in <a href="https://deducteam.github.io/" style="color:grey;">Dedukti</a>, and exported to <a href="http://opentheory.gilith.com/" style="color:grey;"> Open theory </a>, <a href="https://coq.inria.fr/" style="color:grey;"> Coq </a>, <a href="http://matita.cs.unibo.it/" style="color:grey;"> Matita </a>, <a href="https://leanprover.github.io/" style="color:grey;"> Lean </a>, and <a href="http://pvs.csl.sri.com/" style="color:grey;"> PVS </a>.</h4>
            </div>
            <div class="timeline-body">
              <p></p>
            </div>
          </div>
        </li>
      </ul>
    </div>

    <hr class="my-4">
    <div class="container-fluid">
      <div class="row">
        <div class="col-md-12 col-xs-12 col-sm-12">
          <div class="well">
            <img src="http://www.cabkc.in/resource/Image/img-mail-contact.jpg" height="100px;" width="100px;" class="rounded-circle mx-auto d-block">
            <div class="well-header">
              <hr />
              <h1 class="text-center"> <strong> Contact Form </strong></h1>
              <hr />
            </div>

            <div class="row">
              <div class="col-md-12 col-xs-12 col-sm-12">
                <div class="form-group">
                  <div class="input-group">
                    <div class="input-group-prepend">
                      <div class="input-group-text"><i class="fas fa-user"></i></div>
                    </div>
                    <input type="text" name="cname" placeholder="Enter Your Name" required="" class="form-control">
                  </div>
                </div>
              </div>
            </div>

            <div class="row">
              <div class="col-md-12 col-xs-12 col-sm-12">
                <div class="form-group">
                  <div class="input-group">
                    <div class="input-group-prepend">
                      <div class="input-group-text"><i class="fas fa-envelope"></i></div>
                    </div>
                    <input type="email" required="" class="form-control" name="cemail" placeholder="Enter Email">
                  </div>
                </div>
              </div>
            </div>

            <div class="row">
              <div class="col-md-12 col-xs-12 col-sm-12">
                <div class="form-group">
                  <div class="input-group">
                    <div class="input-group-prepend">
                      <div class="input-group-text"><i class="fas fa-users"></i></div>
                    </div>
                    <select class="custom-select" id="inlineFormCustomSelect">
                      <option selected>Choose...</option>
                      <option value="gilles">Gilles Dowek</option>
                      <option value="francois">François Thiré</option>
                    </select>
                  </div>
                </div>
              </div>
            </div>

            <div class="row">
              <div class="col-md-12 col-xs-12 col-sm-12">
                <div class="form-group">
                  <div class="input-group">
                    <div class="input-group-prepend">
                      <div class="input-group-text"><i class="fas fa-comment"></i></div>
                    </div>
                    <textarea class="form-control" placeholder="Enter Message Here..."></textarea>
                  </div>
                </div>
              </div>
            </div>

            <div class="row">
              <div class="col-md-12 col-xs-12 col-sm-12">
                <button class="btn btn-block btn-lg btn-secondary"> Submit </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <nav class="navbar navbar-dark bg-dark">
      <h5 class="navbar-text" style="color:white;">
        <div class="text-center">
          <a href="https://github.com/Deducteam/Logipedia"><I> <U>GitHub</U> </I></a>
        </div>
        <small>
          <hr class="my-4" style="background-color:white;">
          <a href="../index.php"><I> <U>Logipedia</U> </I></a> has been developed by <strong>Walid Moustaoui</strong>, <strong>François Thiré</strong>, and <strong>Gilles Dowek</strong>.</br>
          It builds on the work of <strong>Ali Assaf</strong>, <strong>Bruno Barras</strong>, <strong>Frédéric Blanqui</strong>, <strong>Mathieu Boespflug</strong>, <strong>Guillaume Burel</strong>, <strong>Quentin Carbonneaux</strong>, <strong>Raphaël Cauderlier</strong>, <strong>Denis Cousineau</strong>, <strong>David Delahaye</strong>, <strong>Catherine Dubois</strong>, <strong>Yacine El Haddad</strong>, <strong>Gaspard Férey</strong>, <strong>Guillaume Genestier</strong>, <strong>Frédéric Gilbert</strong>, <strong>Thérèse Hardin</strong>, <strong>Jean-Pierre Jouannaud</strong>, <strong>Rodolphe Lepigre</strong>, <strong>Pierre Halmagrand</strong>, <strong>Olivier Hermant</strong>, <strong>Claude Kirchner</strong>, <strong>Ronan Saillard</strong>, <strong>Benjamin Werner</strong>, <strong>Robert White</strong>, <strong>César Mûnoz</strong>, <strong>Stephane Graham-Lengrand</strong> and many others.
        </small>
      </h5>
    </nav>
  </body>
</html>
