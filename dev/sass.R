
#  ------------------------------------------------------------------------
#
# Title : SASS Styles Compilation
#    By : Jimmy Briggs
#  Date : 2024-09-27
#
#  ------------------------------------------------------------------------

# libs --------------------------------------------------------------------
require(sass)
require(cli)

# vars --------------------------------------------------------------------
input_scss <- "inst/www/styles/scss/index.scss"
output_css <- "inst/www/styles/styles.min.css"

# options -----------------------------------------------------------------
default_opts <- sass::sass_options_get()
sass::sass_options_set(output_style = "compressed")
on.exit(sass::sass_options_set(default_opts))

# compile -----------------------------------------------------------------
output <- sass::sass(
  input = sass::sass_file(input_scss),
  options = sass::sass_options_get(),
  output = output_css,
  write_attachments = FALSE,
  cache = NULL,
  cache_key_extra = NULL
)

cli::cli_bullets(
  c(
    "v" = "Successfully compiled SASS styles (SCSS -> CSS)!",
    "i" = "Input: {.file {input_scss}}",
    "i" = "Output: {.file {output_css}}",
    "i" = "Access compiled styles via {.field gmhdatahub::pkg_sys_assets('styles/styles.min.css')}"
  )
)
