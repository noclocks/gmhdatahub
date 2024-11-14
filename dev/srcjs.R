fs::dir_create("dev/srcjs")
currwd <- getwd()
setwd("dev/srcjs")
on.exit(setwd(currwd))

npm::npm_init()
npm::npm_install(
  scope = "dev",
  c(
    "typescript",
    "@types/rstudio-shiny",
    "esbuild",
    "open-props"
  )
)
    # "eslint",
    # "eslint-config-prettier",
    # "eslint-plugin-prettier",
    # "@typescript-eslint/eslint-plugin",
    # "@typescript-eslint/parser",
    # "prettier",
    # "prettier-plugin-organize-imports",
