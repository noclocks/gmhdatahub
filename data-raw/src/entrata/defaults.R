
#  ------------------------------------------------------------------------
#
# Title : Default Values for Entrata
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------


# default endpoint --------------------------------------------------------

entrata_default_endpoint <- "status"

# default endpoint methods ------------------------------------------------

# these are the default endpoint/method combinations that are used when
# no specific endpoint/method is provided:

entrata_default_methods <- list(
  "status" = "getStatus",
  "applications" = "getCompanyApplications",
  "arcodes" = "getArCodes",
  "arpayments" = "getArPayments",
  "artransactions" = "getArInvoices",
  "communications" = "getMarketingPreferencePickList",
  "customers" = "getCustomers",
  "financial" = "getApCodes",
  "leads" = "getLeads",
  "leases" = "getLeases",
  "leasingcenter" = "getCallLogs",
  "maintenance" = "getInspections",
  "pricing" = "getPricingPicklists",
  "properties" = "getProperties",
  "propertyunits" = "getPropertyUnits",
  "queue" = "getResponse",
  "reports" = "getReportList",
  "vendors" = "getVendors"
)

# versions ----------------------------------------------------------------

entrata_default_version <- "v1"


# request -----------------------------------------------------------------

entrata_default_req_body <- list(
  auth = list(
    type = "basic"
  ),
  requestId = 15,
  method = list(
    name = NULL,
    version = "r1",
    params = list(NULL)
  )
)

# response (successful) ---------------------------------------------------

entrata_default_resp_body <- list(
  response = list(
    requestId = 15,
    code = 200,
    result = list()
  )
)
