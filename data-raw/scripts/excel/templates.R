#  ------------------------------------------------------------------------
#
# Title : Excel Templates
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------


# pre-lease ---------------------------------------------------------------

# initialize workbook
wb_init <- openxlsx2::wb_workbook(
  creator = "No Clocks, LLC",
  title = "GMH Communities Pre-Lease Excel Report",
  subject = "Pre-Lease Report",
  category = "Real-Estate",
  company = "GMH Communities"
)

# add worksheets
openxlsx2::wb_add_worksheet(wb_init, "Cover", tab_color = "black")
openxlsx2::wb_add_worksheet(wb_init, "Index", tab_color = "black")
openxlsx2::wb_add_worksheet(wb_init, "Summary", tab_color = "red")
openxlsx2::wb_add_worksheet(wb_init, "Parameters", tab_color = "blue")
