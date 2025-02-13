
function pre_lease_tbl_cell_edit(ns_prefix, index) {

  var cell_edit_id = ns_prefix + "cell_edit";
  Shiny.setInputValue(cell_edit_id, index);
  console.log("[pre_lease_tbl_cell_edit()]: cell_edit_id = " + cell_edit_id + "; " + "index: " + index);

}
