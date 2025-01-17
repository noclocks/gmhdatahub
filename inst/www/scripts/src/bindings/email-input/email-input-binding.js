// src/bindings/email-input/email-input-binding.js

var emailInputBinding = new Shiny.InputBinding();

$.extend(emailInputBinding, {
  find: function (scope) {
    return $(scope).find(".email-input");
  },
  getValue: function (el) {
    return $(el).val();
  },
  setValue: function (el, value) {
    $(el).val(value);
  },
  subscribe: function (el, callback) {
    $(el).on("change.emailInput", function (e) {
      callback();
    });
  },
  unsubscribe: function (el) {
    $(el).off(".emailInput");
  }
});

Shiny.inputBindings.register(emailBinding);
