
#  ------------------------------------------------------------------------
#
# Title : Asset Registry
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------

files <- fs::dir_ls("inst/www", recurse = TRUE, type = "file")

img_files <- fs::dir_ls("inst/www/images", recurse = TRUE, type = "file")
style_files <- fs::dir_ls("inst/www/styles", recurse = TRUE, type = "file")
js_files <- fs::dir_ls("inst/www/scripts", recurse = TRUE, type = "file")
font_files <- fs::dir_ls("inst/www/fonts", recurse = TRUE, type = "file")

registry <- list(
  brands = list(
    gmh = list(
      logos = list(
        primary = list(
          default = "gmh/logos/logo.svg",
          dark = "gmh/logos/logo-dark.svg",
          light = "gmh/logos/logo-light.svg"
        ),
        divisions = list(
          communities = "gmh/logos/communities/gmh-communities-logo.svg",
          go = "gmh/logos/go/gmh-go-logo.svg",
          innovative = "gmh/logos/innovative-living/gmh-innovative-living-logo.svg",
          residential = "gmh/logos/residential-living/gmh-residential-living-logo.svg",
          student = "gmh/logos/student-living/gmh-student-living-logo.svg"
        )
      )
    ),
    noclocks = list(
      logos = list(
        primary = list(
          default = "noclocks/logos/noclocks-logo-black.svg",
          dark = "noclocks/logos/noclocks-logo-white.svg",
          light = "noclocks/logos/noclocks-logo-black.svg"
        )
      )
    ),
    entrata = list(
      logos = list(
        primary = list(
          default = "entrata/logos/entrata-logo-square-red.svg",
          dark = "entrata/logos/entrata-logo-dark.svg",
          light = "entrata/logos/entrata-logo-light.svg"
        )
      )
    ),
    properties = list(
      # Use standardized property keys
      academy_65 = list(
        logos = list(
          default = "properties/academy-65/academy65-logo.webp",
          black = "properties/academy-65/academy65-logo-black.webp"
        )
      )
    ),

    shared = list(
      app = list(
        icons = "shared/app/icons/app-icon.webp",
        logos = "shared/app/logos/app-logo.svg"
      ),
      favicons = list(
        android = "shared/favicons/android/android-chrome-{size}.png",
        apple = "shared/favicons/apple/apple-touch-icon-{size}.png",
        favicon = "shared/favicons/favicon.ico"
      )
    )
  ),
  images = list(
    entrata = list(
      banner = "images/entrata/entrata-banner.jpeg",
      logo = list(
        dark = "images/entrata/entrata-logo-dark.png",
        light = "images/entrata/entrata-logo-light.png",
        square = "images/entrata/entrata-logo-square-red.jpg",
        white = "images/entrata/entrata-logo-white.svg",
        default = "images/entrata/entrata.png"
      )
    ),
    icons = list(
      app = "images/icons/app-icon.webp",
      gmh = "images/icons/gmh-icon.png",
      noclocks = "images/icons/noclocks-icon-circular.png"
    ),
    logos = list(
      app = "images/logos/app-logo.svg",
      entrata = "images/logos/entrata-logo.png",
      gmh = "images/logos/gmh-logo.svg",
      noclocks = "images/logos/noclocks-logo.svg"
    ),
    placeholders = list(
      default = "images/placeholders/default-image.png"
    )
  ),
  scripts = list(
    main = "scripts/index.js",
    modules = list(
      config = "scripts/js/config.js",
      cookies = "scripts/js/cookies.js",
      gmaps = "scripts/js/gmaps.js",
      idle = "scripts/js/idle.js",
      iframe = "scripts/js/iframe.js",
      init = "scripts/js/init.js",
      redirect = "scripts/js/redirect.js"
    )
  ),
  styles = list(
    css = "styles/css/styles.min.css",
    scss = list(
      elements = list(
        buttons = "styles/scss/elements/_buttons.scss",
        cards = "styles/scss/elements/_cards.scss",
        selectize = "styles/scss/elements/_selectize.scss",
        tables = "styles/scss/elements/_tables.scss"
      ),
      layout = list(
        footer = "styles/scss/layout/_footer.scss",
        navbar = "styles/scss/layout/_navbar.scss",
        page = "styles/scss/layout/_page.scss",
        sidebar = "styles/scss/layout/_sidebar.scss",
        tabs = "styles/scss/layout/_tabs.scss"
      ),
      theme = list(
        colors = "styles/scss/theme/_colors.scss",
        company = "styles/scss/theme/_company.scss",
        logos = "styles/scss/theme/_logos.scss",
        social = "styles/scss/theme/_social.scss",
        spacing = "styles/scss/theme/_spacing.scss",
        typography = "styles/scss/theme/_typography.scss"
      ),
      utility = list(
        gmaps = "styles/scss/utility/_gmaps.scss",
        hover = "styles/scss/utility/_hover.scss",
        settings = "styles/scss/utility/_settings.scss",
        utils = "styles/scss/utility/_utils.scss"
      ),
      main = "styles/scss/index.scss"
    )
  )
)

