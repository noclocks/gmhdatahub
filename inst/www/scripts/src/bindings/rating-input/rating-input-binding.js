// src/bindings/rating-input/rating-input-binding.js

var ratingInputBinding = new Shiny.InputBinding();

$.extend(ratingInputBinding, {
  find: function (scope) {
    return $(scope).find('.rating-input');
  },
  getValue: function (el) {
    return $(el).attr('data-value');
  },
  setValue: function (el, value) {
    $(el).attr('data-value', value);
    this._updateRating(el);
  },
  subscribe: function (el, callback) {
    $(el).on('click.ratingInput', '.rating-star', function (e) {
      var value = $(this).attr('data-value');
      $(el).attr('data-value', value);
      callback();
    });
  },
  unsubscribe: function (el) {
    $(el).off('.ratingInput');
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);
  },
  _updateRating: function (el) {
    var value = $(el).attr('data-value');
    $(el).find('.rating-star').each(function () {
      var starValue = $(this).attr('data-value');
      if (starValue <= value) {
        $(this).addClass('rating-star-active');
      } else {
        $(this).removeClass('rating-star-active');
      }
    });
  }
});

Shiny.inputBindings.register(ratingInputBinding);
