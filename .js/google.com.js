//Google Troll Face
$('#lga > img:first').attr('src', 'http://www.trollface.net/trollface.png').attr('height', '').css('margin-top', '-120px');

// Google changes it's site so often, it's impossible to keep up :(

//Secretly send search query to duckduckgo.com
//$('input[type=text][name=q]').closest('form').attr('action', 'https://duckduckgo.com/');

//Make sure autocomplete is turned off, otherwise our trick won't work
//if (!window.location.search.match('complete=0')) {
//  window.location.replace('http://www.google.com/webhp?complete=0');
//}
