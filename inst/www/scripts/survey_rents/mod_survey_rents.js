$(document).on('show.bs.modal', function () {
  // Increase timeout to 300ms
  setTimeout(function () {
    $('.handsontable').each(function () {
      var id = $(this).attr('id');
      var w = HTMLWidgets.find('#' + id);
      var hot = w && w.hot ? w.hot : null;
      if (hot !== null) {
        hot.updateSettings({});  // Force a settings update
        hot.render();
      }
    });
  }, 300);
});

$(document).on('shown.bs.collapse', function(e) {
  setTimeout(function() {
    $('.handsontable').each(function() {
      var id = $(this).attr('id');
      var w = HTMLWidgets.find('#' + id);
      var hot = w && w.hot ? w.hot : null;
      if (hot !== null) {
        hot.render();
      }
    });
  }, 200);
});
