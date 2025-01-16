// src/bindings/url-input/url-input-binding.js

var urlInputBinding = new Shiny.InputBinding();

$.extend(urlInputBinding, {
  find: function (scope) {
    return $(scope).find(".url-input");
  },
  getValue: function (el) {
    return $(el).val();
  },
  setValue: function (el, value) {
    $(el).val(value);
  },
  subscribe: function (el, callback) {
    $(el).on("change.urlInput", function (e) {
      callback();
    });
  },
  unsubscribe: function (el) {
    $(el).off(".urlInput");
  }
});

Shiny.inputBindings.register(urlBinding);
