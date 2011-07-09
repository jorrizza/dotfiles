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

$('#header').css('background', 'none repeat scroll 0 0 #FF2EF8');
$('.blog-description').css('color', '#EEEEEE').text('NETWORK OF LOVELY PEOPLE WHO LOVE YOU');
$('.widgetcontainer').css('background', 'none repeat-x scroll 0 0 #FFADFC');
$('.widgettitle').css({background: 'none repeat-x scroll 0 0 #C700C0', color: '#EEEEEE'});
$('#nav').css({background: 'none repeat-x scroll 0 0 #C700C0', color: '#EEEEEE'});
$('#nav .sf-menu a').css('color', '#EEEEEE');
$('.twitter_title_link').css('color', '#EEEEEE');
$('#nav-content').css({background: 'none repeat-x scroll 0 0 #C700C0', color: '#EEEEEE'});

$('title').text('Rails Geeks | Network of lovely people who love you OMG PONIES');

$('body').css('font-family', '"Comic Sans MS"');

$('.blogroll li:last').clone().appendTo($('.blogroll'));
$('.blogroll li:last a').text('OMG PONIES!').attr('href', 'http://www.youtube.com/watch?v=ZJfZTr8zMsc');

$('#index-featured1').html('<iframe width="640" height="390" src="http://www.youtube.com/embed/ZJfZTr8zMsc?hd=1" frameborder="0" allowfullscreen></iframe>');