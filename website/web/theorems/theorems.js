$(document).ready(function(){
  $('body').append('<div id="toTop" class="btn btn-info"><i class="fas fa-chevron-up"></i></div>');
  $(window).scroll(function () {
    if ($(this).scrollTop() != 0) {
      $('#toTop').fadeIn();
    } else {
      $('#toTop').fadeOut();
    }
  });
  $('#toTop').click(function(){
      $("html, body").animate({ scrollTop: 0 }, 600);
      return false;
  });
});

$("#btHide").click(function(){
if ($( "#iconDep" ).hasClass("fas fa-chevron-down")) {
  $( "#iconDep" ).removeClass("fas fa-chevron-down").addClass("fas fa-chevron-up");

} else {
  $( "#iconDep" ).removeClass("fas fa-chevron-up").addClass("fas fa-chevron-down");
}
});

$("#btHide2").click(function(){
if ($( "#iconDep2" ).hasClass("fas fa-chevron-down")) {
  $( "#iconDep2" ).removeClass("fas fa-chevron-down").addClass("fas fa-chevron-up");

} else {
  $( "#iconDep2" ).removeClass("fas fa-chevron-up").addClass("fas fa-chevron-down");
}
});
