
#  ------------------------------------------------------------------------
#
# Title : Entrata API Schemas
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------

schemas_dir <- "data-raw/src/entrata/schemas"
fs::dir_create(schemas_dir)

# endpoints ---------------------------------------------------------------

entrata_endpoints_schema <- list(
  type = "object",
  properties = list(
    endpoint = list(
      type = "string",
      enum = entrata_endpoints
    )
  ),
  required = "endpoint"
)

jsonlite::write_json(
  entrata_endpoints_schema,
  file.path(schemas_dir, "entrata_endpoints.schema.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  force = TRUE
)

# methods -----------------------------------------------------------------

entrata_methods_schema <- list(
  type = "object",
  properties = list(
    endpoint = list(
      type = "string",
      enum = unique(entrata_methods_tbl$endpoint)
    ),
    method = list(
      type = "string",
      enum = unique(entrata_methods_tbl$method)
    )
  ),
  required = c("endpoint", "method")
)

jsonlite::write_json(
  entrata_methods_schema,
  file.path(schemas_dir, "entrata_methods.schema.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  force = TRUE
)

# versions ----------------------------------------------------------------

entrata_versions_schema <- list(
  type = "object",
  properties = list(
    method = list(
      type = "string",
      enum = unique(entrata_methods_tbl$method)
    ),
    version = list(
      type = "string",
      enum = unique(entrata_methods_tbl$version)
    )
  ),
  required = c("method", "version")
)

jsonlite::write_json(
  entrata_versions_schema,
  file.path(schemas_dir, "entrata_versions.schema.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  force = TRUE
)

# parameters --------------------------------------------------------------

entrata_parameters_schema <- list(
  type = "object",
  properties = list(
    method = list(
      type = "string",
      enum = unique(entrata_methods_tbl$method)
    ),
    version = list(
      type = "string",
      enum = unique(entrata_methods_tbl$version)
    ),
    params = list(
      type = "string",
      enum = unique(entrata_params_tbl$parameter)
    )
  ),
  required = c("method", "version", "parameter")
)

jsonlite::write_json(
  entrata_parameters_schema,
  file.path(schemas_dir, "entrata_parameters.schema.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  force = TRUE
)


# base request ------------------------------------------------------------

# entrata_req_body_schema <- list(
#   # auth object (required): type is always set to basic (const)
#   auth = list(
#     type = "basic"
#   ),
#   # requestId (optional but recommended): a unique identifier for the request;
#   # - the API docs always default to a value of 15
#   # - from experimenting, this value seems like it can be anything (string or integer or null)
#   requestId = c("string", "integer", "null"),
#   # method object (required if the endpoint method requires a body);
#   # has fields name (string, required), version (enum: r1, r2, or r3),
#   # and params (object, optional, specific to the method):
#   method = list(
#     name = "string",
#     version = c("r1", "r2", "r3"),
#     params = list(
#       # method-specific parameters
#     )
#   )
#
# )
