library(ellmer)
library(dm)
library(shiny)
library(bslib)
library(shinychat)

pool <- db_connect()
conn <- pool::poolCheckout(pool)
on.exit(pool::poolReturn(conn))

gmh_dm <- dm::dm_from_con(con = conn, schema = "gmh")
gmh_schema_info <- capture.output(print(dm::dm_get_tables(gmh_dm)))

sys_prompt <- paste0(
  "You are an assistant that converts natural language questions into SQL queries.",
  "Be sure to that the SQL you return is valid PostgreSQL SQL and is for the database schema provided below, ",
  "and that the SQL query is appropriate for the question asked and that you ",
  "properly quote and use the schema.table_name syntax. All queries will be performed ",
  "against the \"gmh\" schema.\n\n",
  "Your response should be formatted like so:\n\n",
  "To retrieve all data from the `properties` table, use the following SQL query:\n\n",
  "```sql\n",
  "SELECT * FROM properties;\n",
  "```\n\n",
  "Database Schema:\n",
  paste(gmh_schema_info, collapse = "\n"),
  sep = "\n"
)

extract_sql <- function(md_text) {
  pattern <- "```(?:sql)?\\n([\\s\\S]*?)\\n```"
  matches <- regexec(pattern, md_text, perl = TRUE)
  result <- regmatches(md_text, matches)
  if (length(result) == 0 || length(result[[1]]) < 2) {
    return(NULL)
  }
  sql <- result[[1]][2]
  sql <- trimws(sql)
  return(sql)
}

db_chat <- ellmer::chat_openai(system_prompt = sys_prompt)

qry <- db_chat$chat("table of unique properties and their associated partners")
sql <- extract_sql(qry)

tryCatch({
  res <- DBI::dbGetQuery(conn, sql)
  paste(capture.output(print(res)), collapse = "\n")
}, error = function(e) {
  e$message
})

ui <- bslib::page_fillable(
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Chat"),
      shinychat::chat_ui(
        "chat",
        placeholder = "Chat against the database...",
        fill = TRUE
      )
    ),
    bslib::card(
      bslib::card_header("Query Results"),
      bslib::card_body(
        reactable::reactableOutput("query_results")
      )
    )
  )
)

server <- function(input, output, session) {

  db_chat <- db_chat

  chat_history <- shiny::reactiveVal(NULL)
  query_results <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$chat_user_input, {

    response <- db_chat$chat(input$chat_user_input)
    sql <- extract_sql(response)

    tryCatch({
      res <- DBI::dbGetQuery(conn, sql)
      query_results(res)
      shinychat::chat_append(
        "chat",
        paste0(
          "Generated SQL: ",
          sql,
          "\n\nResults shown in table ->"
        )
      )
    }, error = function(e) {
      shinychat::chat_append("chat", paste("Error:", e$message))
    })
  })

  output$query_results <- reactable::renderReactable({
    req(query_results())
    reactable::reactable(
      query_results(),
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      )
    )
  })
}

shiny::shinyApp(ui, server)
