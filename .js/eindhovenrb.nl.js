var kitten_color = false;

$('img').each(
  function () {
    if (kitten_color) {
      $(this).attr('src', 'http://placekitten.com/'+$(this).width()+'/'+$(this).height());
      kitten_color = false;
    } else {
      $(this).attr('src', 'http://placekitten.com/g/'+$(this).width()+'/'+$(this).height());
      kitten_color = true;
    }
  }
);

$('body').append('<img src="http://placekitten.com/50/50/" alt="kitten" id="followkitten" style="position: fixed">');

$('body').bind('mousemove', function (e) {
                 $("#followkitten").css("left", e.pageX + 5).css("top", e.pageY + 5);
               });

$('body').css('font-family', 'Comic Sans MS');
$('body').css('background-color', '#FCC7F9');
$('.fill').css('background-color', '#FCC7F9').css('background-image', 'url(http://placekitten.com/80/50/)');
$('#intro p').replaceWith("<h1>" + $('#intro p').text() + " ZOMG!!!1one</h1>");
$('p, h1, h2, h3').each(function(idx, el){
              $(el).css('color', ['red', 'green',
                                  'blue'][Math.round(Math.random() * 2)]);
            });
$('.brand').css('font-size', '50px').css('color', 'yellow');