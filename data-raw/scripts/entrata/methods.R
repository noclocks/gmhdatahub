
#  ------------------------------------------------------------------------
#
# Title : Entrata Methods
#    By : Jimmy Briggs
#  Date : 2024-12-30
#
#  ------------------------------------------------------------------------


# list --------------------------------------------------------------------

entrata_methods_lst <- list(
  "status" = c(
    "getStatus"
  ),
  "applications" = c(
    "getCompanyApplications",
    "sendApplicantGeneralDetails",
    "sendApplication",
    "sendApplicationAddOns",
    "sendApplicationEmployers",
    "sendApplicationPets",
    "sendApplicationVehicles",
    "updateApplication"
  ),
  "arcodes" = c(
    "getArCodes"
  ),
  "arpayments" = c(
    "getArPayments"
  ),
  "artransactions" = c(
    "getArInvoices",
    "getLeaseArTransactions",
    "getMitsLeaseArTransactions",
    "sendLeaseArTransactionReversals",
    "sendLeaseArTransactions"
  ),
  "communications" = c(
    "getMarketingPreferencePickList",
    "getMarketingPreferences"
  ),
  "customers" = c(
    "getCustomers",
    "getCustomerTestimonials",
    "getTestimonialPickLists",
    "searchCustomers",
    "sendCustomerTestimonials",
    "updateCustomers",
    "updateCustomerTestimonials",
    "updatePropertyResponse"
  ),
  "financial" = c(
    "getApCodes",
    "getBankAccounts",
    "getBudgetActuals",
    "getBudgets",
    "getFinancialPickList",
    "getGlTransactions",
    "getGlTrees",
    "getJobCategories",
    "getJobCostBudgets",
    "getJobs",
    "getTransactionTagLists",
    "markGlTransactionsExported",
    "sendBudgets",
    "sendJournalEntries",
    "updateBudgets"
  ),
  "leads" = c(
    "applyQuote",
    "generateQuotes",
    "getLeadEvents",
    "getLeadPickLists",
    "getLeads",
    "getMitsLeads",
    "getQuotes",
    "sendLeads",
    "sendMitsLeads",
    "updateLeads"
  ),
  "leases" = c(
    "cancelLease",
    "getEvictedLeases",
    "getExpiringLeases",
    "getLeaseDetails",
    "getLeaseDocuments",
    "getLeaseDocumentsList",
    "getLeasePickList",
    "getLeases",
    "getMitsCollections",
    "getMitsLeases",
    "getParcelAlerts",
    "getRentersInsurancePolicies",
    "moveInLease",
    "moveOutLease",
    "onNoticeLease",
    "sendLeaseActivities",
    "sendLeaseDocuments",
    "sendLeases",
    "sendRentersInsurancePolicies",
    "sendRoommateGroups",
    "sendScheduledCharges",
    "updateLease",
    "updateScheduledCharges"
  ),
  "leasingcenter" = c(
    "getCallLogs",
    "getLeasingCenterPickLists"
  ),
  "maintenance" = c(
    "getInspections",
    "getInspectionTemplates",
    "getWorkOrderPickLists",
    "getWorkOrders",
    "sendWorkOrders",
    "updateWorkOrders"
  ),
  "pricing" = c(
    "getPricingPicklists",
    "insertPricing",
    "sendBudgetedRent"
  ),
  "properties" = c(
    "getAmenityReservations",
    "getCalendarAvailability",
    "getFloorPlans",
    "getPetTypes",
    "getPhoneNumber",
    "getProperties",
    "getPropertyAddOns",
    "getPropertyAnnouncements",
    "getPropertyPickLists",
    "getRentableItems",
    "getReservableAmenities",
    "getWebsites",
    "sendFloorplans",
    "sendRentableItems"
  ),
  "propertyunits" = c(
    "getAmenities",
    "getMitsPropertyUnits",
    "getPropertyUnits",
    "getSpecials",
    "getUnitsAvailabilityAndPricing",
    "getUnitTypes",
    "sendAmenities",
    "sendPropertyUnits",
    "updateAmenities"
  ),
  "queue" = c("getResponse"),
  "reports" = c(
    "getDependentFilter",
    "getReportData",
    "getReportInfo",
    "getReportList"
  ),
  "vendors" = c(
    "getInvoices",
    "getPoReceivingRecords",
    "getPurchaseOrders",
    "getTaxFormData",
    "getVendorLocations",
    "getVendorPickLists",
    "getVendors",
    "markInvoicesExported",
    "sendInvoices",
    "sendPurchaseOrders",
    "sendVendors",
    "updateInvoices",
    "updateVendors",
    "voidApPayments"
  )
)

# table -------------------------------------------------------------------

entrata_methods_tbl <- tibble::enframe(
  entrata_methods_lst,
  name = "endpoint",
  value = "method_name"
) |>
  tidyr::unnest_longer("method_name")


