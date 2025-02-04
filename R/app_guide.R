
app_guide <- function() {
  conductor::Conductor$new()$
    step(
      "Welcome",
      "Let's take a quick tour of the platform's key features."
    )
}

# library(shiny)
# library(conductor)
#
# conductor <- Conductor$
#   new(
#     exitOnEsc = FALSE,
#     keyboardNavigation = FALSE
#   )$
#   step(
#     title = "Hello there",
#     text = "This popover is displayed in the center of the screen."
#   )$
#   step(
#     el = "#test",
#     title = "This is a button",
#     text = "This button has no purpose. Its only goal is to serve as support for demo."
#   )$
#   step(
#     el = "#test",
#     title = "An alternative text"
#   )
#
# ui <- fluidPage(
#   useConductor(),
#   actionButton("test", "Test")
# )
#
# server <- function(input, output, session){
#   conductor$init()$start()
# }
#
# shinyApp(ui, server)
