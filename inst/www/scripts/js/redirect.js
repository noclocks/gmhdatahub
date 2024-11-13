$( document ).ready(function() {
  Shiny.addCustomMessageHandler('redirect', function(message) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', "/cookie", true);
    xhr.setRequestHeader("Header-User-Cookie", message);
    xhr.onreadystatechange = function() {
      if (xhr.readyState == 4 && xhr.status == 200)
      window.location = message;
    };
    xhr.send();
  });
});
