/* mod_market_survey_property_summary_ui() JavaScript */

function mod_market_survey_property_summary_ui(ns_prefix) {

  var card_id = ns_prefix + "card";
  var edit_mode_id = ns_prefix + "edit-mode";
  var view_mode_id = ns_prefix + "view-mode";

  $(document).ready(function() {
    $("#" + edit_mode_id).click(function() {
      $("#" + card_id).addClass('editing');
    });
    $("#" + view_mode_id).click(function() {
      $("#" + card_id).removeClass('editing');
    });
  });

}
