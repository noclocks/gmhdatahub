#  ------------------------------------------------------------------------
#
# Title : GMH Portfolios Preparation
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_portfolios_tbl <- tibble::tribble(
  ~portfolio_id, ~portfolio_name, ~portfolio_full_name, ~portfolio_type, ~portfolio_description, ~portfolio_status, ~portfolio_website, ~portfolio_logo_url, ~portfolio_icon_url, ~partner_id,
  1L, "AGC", "AGC Equity Partners", "Equity Partner", "AGC Equity Partners is a global alternative asset investment firm operating from offices in London and the Middle East. AGC Equity Partners invests across alternative assets, including real estate, infrastructure, technology, growth, private equity, among others.", "Active", "https://agcequitypartners.com/", "https://agcequitypartners.com/wp-content/uploads/2024/03/AGC_Logo_Inline_Mar24-1.png", "https://cdn.brandfetch.io/agcequitypartners.com/logo", 2L,
  2L, "CBRE", "CBRE Investment Management", "Equity Partner", "As a leading real assets investment management firm, we strive to realize potential in investments and people in innovative ways, every day. We do so by creating sustainable investment solutions of tomorrow so our clients, people and communities thrive.", "Active", "https://www.cbre.com", "https://cdn.brandfetch.io/cbre.com/logo", "https://cdn.brandfetch.io/cbre.com/icon", 1L,
  3L, "CRG", "CRG Real Estate Solutions", "Equity Partner", "CRG is a recognized leader in development and investment throughout North America.", "Active", "https://www.realcrg.com", "https://cdn.brandfetch.io/realcrg.com/logo", "https://cdn.brandfetch.io/realcrg.com/icon", 3L,
  4L, "Medistar", "Medistar Corporation", "Owner", "Medistar Corporation, a privately held real estate investment and development firm, boasts a remarkable 50-year history of innovation. With an unwavering focus on execution and a proven history of outstanding performance, Medistar stands as a testament to enduring excellence.", "Active", "https://www.medistarcorp.com/", "https://cdn.brandfetch.io/medistarcorp.com/logo", "https://cdn.brandfetch.io/medistarcorp.com/icon", 7L,
  5L, "Principal", "Principal Asset Management", "Equity Partner", "Principal Asset Management", "Active", "https://www.principalam.com/us/investment-teams/principal-real-estate", "https://asset.brandfetch.io/id8-eJ8GSN/idLtuKTQo1.png", "https://asset.brandfetch.io/id8-eJ8GSN/idPVfmaU8b.svg", 5L,
  6L, "AEW", "AEW Capital Management", "Equity Partner", "AEW Capital Management is a real estate investment management firm that provides services to investors worldwide", "Active", "https://www.aew.com", "https://cdn.brandfetch.io/aew.com/w/512/h/122/theme/light/logo?c=1idD1Ep0IvCNcUa3_E3", "https://cdn.brandfetch.io/aew.com/w/400/h/400?c=1idD1Ep0IvCNcUa3_E3", 4L
)

readr::write_csv(gmh_portfolios_tbl, "data-raw/data/working/gmh/gmh_portfolios.csv")
