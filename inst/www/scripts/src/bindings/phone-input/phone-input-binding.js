// src/bindings/phone-input/phone-input-binding.js

var phoneInputBinding = new Shiny.InputBinding();

$.extend(phoneInputBinding, {
  find: function (scope) {
    return $(scope).find(".phone-input");
  },
  getValue: function (el) {
    return $(el).val();
  },
  setValue: function (el, value) {
    $(el).val(value);
  },
  subscribe: function (el, callback) {
    $(el).on("input.phoneInput", function (e) {
      var x = $(this).val()
        .replace(/\D/g, '')
        .match(/(\d{0,3})(\d{0,3})(\d{0,4})/);
      $(this).val(!x[2] ? x[1] : x[1] + '-' + x[2] + (x[3] ? '-' + x[3] : ''));
      callback();
    });
  },
  unsubscribe: function (el) {
    $(el).off(".phoneInput");
  }
});

Shiny.inputBindings.register(phoneBinding);
