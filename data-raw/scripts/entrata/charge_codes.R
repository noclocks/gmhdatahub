#  ------------------------------------------------------------------------
#
# Title : Entrata Charge Codes (A/R Codes)
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

req <- entrata_arcodes_request()
resp <- entrata_req_perform_save(req)

resp_arcodes_json <- resp |> httr2::resp_body_json()

entrata_charge_codes_tbl <- resp_arcodes_json |>
  purrr::pluck("response", "result", "arcodes", "arcode") |>
  dplyr::bind_rows() |>
  dplyr::select(
    code_id = "id",
    ar_code = "code",
    code_name = "name",
    code_type = "codeType",
    charge_usage = "chargeUsage",
    associated_ledger = "associatedLedger",
    debit_gl_account_id = "debitGlAccountId",
    credit_gl_account_id = "creditGlAccountId",
    display_as = "displayAs",
    is_disabled = "isDisabled",
    is_entrata_disabled = "isEntrataDisabled",
    is_taxable = "isTaxable"
  )
