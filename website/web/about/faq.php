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
       <?php include '../header.php' ?>
       <hr class="my-4">

<div class="container ">
    <div class="panel-group" id="faqAccordion">
        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question1">
                 <h4 class="panel-title">
                    <a href="#" class="ing">What does the field "Theory" means?</a>
              </h4>

            </div>
            <div id="question1" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

      <p>For a theorem, the theory is defined as the set of the constants, type operators and axioms used by this theorem and the transituve closure of its dependencies. It contains also definitions that appear in the statement of axioms. This notion of theory is extended in a natural way to the other kind of objects.</p>
                </div>
            </div>
        </div>
        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question2">
                 <h4 class="panel-title">
                    <a href="#" class="ing">What does the field "Main Dependencies" means?</a>
              </h4>

            </div>
            <div id="question2" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

                    <p> Main dependencies are direct dependencies of a theorem that do not appear in the transitive closure of dependencies of that theorem.
                </div>
            </div>
        </div>

        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question3">
                 <h4 class="panel-title">
                    <a href="#" class="ing">What is a Constant?</a>
              </h4>

            </div>
            <div id="question3" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

      <p> A constant is a symbol that does not have a definition. In general, such symbol comes with axioms it has to satisfy.
      </p>
                </div>
            </div>
        </div>

        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question4">
                 <h4 class="panel-title">
                    <a href="#" class="ing">What is a Type Operator?</a>
              </h4>

            </div>
            <div id="question4" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

                   <p>A Type Operator is an abstract type (such as "nat"). It has not definition and can also be seen as a constant.</p>
                </div>
            </div>
        </div>
        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question5">
                 <h4 class="panel-title">
                    <a href="#" class="ing">Why theorems use so many axioms?</a>
              </h4>

            </div>
            <div id="question5" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

      <p>Logipedia does not use the notion of inductive types nor recursive function. Hence, inductive types and recursive functions are axiomatized.</p>
                </div>
            </div>
        </div>

        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question6">
                 <h4 class="panel-title">
                    <a href="#" class="ing">Why is there no Makefile when I download a file?</a>
              </h4>

            </div>
            <div id="question6" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

      <p>We suppose as the user of the target system, you already know how to build a generic Makefile to compile the files.</p>
                </div>
            </div>
        </div>

        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question7">
                 <h4 class="panel-title">
                    <a href="#" class="ing">Can I add my system into Logipedia?</a>
              </h4>

            </div>
            <div id="question7" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

                    <p>
      If you want to add your system on Logipedia, we invite you to send us an email: <a href="mailto:gilles.dowek@inria.fr">Gilles Dowek</a>, <a href="mailto:francois.thire@inria.fr">François Thiré</a>
                    </p>
                </div>
            </div>
        </div>


        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question8">
                 <h4 class="panel-title">
                    <a href="#" class="ing">Can I add new proofs into Logipedia?</a>
              </h4>

            </div>
            <div id="question8" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

      <p>At the moment, only one logic is supported on Logipedia: <a href="https://arxiv.org/pdf/1807.01873.pdf">STTForall</a>. We want to add more logics in the next few months. If you want discuss how we could add proofs from your logic into Logipedia, send us an email: <a href="mailto:gilles.dowek@inria.fr">Gilles Dowek</a>, <a href="mailto:francois.thire@inria.fr">François Thiré</a></p>
                </div>
            </div>
        </div>
        <div class="panel panel-default ">
            <div class="panel-heading accordion-toggle collapsed question-toggle" data-toggle="collapse" data-parent="#faqAccordion" data-target="#question9">
                 <h4 class="panel-title">
                    <a href="#" class="ing">How can I contribute to Logipedia?</a>
              </h4>

            </div>
            <div id="question9" class="panel-collapse collapse" style="height: 0px;">
                <div class="panel-body">
                     <h5><span class="label label-primary">Answer</span></h5>

      <p>Logipedia is an open source project. You can find us on <a href="https://github.com/Deducteam/Logipedia">GitHub</a>. </p>
                </div>
            </div>
        </div>
    </div>
    <!--/panel-group-->
</div>
       <script src="about.js"></script>
       </body>
       </html>
