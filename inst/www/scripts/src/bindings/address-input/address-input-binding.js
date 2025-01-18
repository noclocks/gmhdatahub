// src/bindings/address-input/address-input-binding.js

var addressInputBinding = new Shiny.InputBinding();

$.extend(addressInputBinding, {
  find: function (scope) {
    return $(scope).find('.address-autocomplete');
  },

  initialize: function (el) {
    var input = $(el);
    var autocomplete = new google.maps.places.Autocomplete(input[0], {
      types: ['geocode']
    });

    autocomplete.addListener('place_changed', function () {
      var place = autocomplete.getPlace();
      input.trigger('change');
    });
  },

  getValue: function (el) {
    return $(el).val();
  },

  setValue: function (el, value) {
    $(el).val(value);
  },

  subscribe: function (el, callback) {
    $(el).on('change.addressAutocompleteBinding', function (e) {
      callback();
    });
  },

  unsubscribe: function (el) {
    $(el).off('.addressAutocompleteBinding');
  }
});

Shiny.inputBindings.register(addressInputBinding, 'addressAutocomplete');
