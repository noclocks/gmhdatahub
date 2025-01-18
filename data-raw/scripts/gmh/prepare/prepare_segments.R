
#  ------------------------------------------------------------------------
#
# Title : GMH Segments Preparation
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_segments_tbl <- tibble::tibble(
  segment_id = as.integer(c(1, 2, 3, 4)),
  segment_name = c(
    "Student Living",
    "Residential Living",
    "Innovative Living",
    "Past Projects"
  ),
  segment_description = c(
    "GMH communities are where you want to live. A successful college experience can be the foundation for a strong and healthy future. There is no greater factor to a student’s educational achievement than their living environment.Our first-class communities provide our residents with leading-edge amenities, community spaces, and apartments to maximize their college experience. When you live in a GMH University Housing community, you gain the independence you’re looking for while still having the comforts of being surrounded by friends and a dedicated team to help when you need it.",
    "GMH Residential Living enables you to live a sophisticated and convenient lifestyle. Our amenities are exceptional and our level of service unrivaled. In a complex and fast-paced world, we provide a sanctuary for you and your family to recharge and enjoy each other’s company. Whether we are steps from the city or down the street from a business hub, you will find the perfect blend of live, work, play in our Residential Living communities.",
    "Innovators strive to accomplish that which has never been done. They are leaders who thrive in diverse and collaborative communities that provide the necessary tools and resources to flourish. Our mission at GMH Innovative Living is to provide exceptional housing, services and amenities to residents within the life sciences, medical, educational and tech communities. As champions of the innovation ecosystem, we allow our residents to focus on their goals and ambitions by fostering a vibrant environment of unique living solutions that cater to the needs of the knowledge community. Our relentless commitment to serving our residents ensures that they can continue to discover, create and grow.",
    "GMH Past Projects no longer actively maintained."
  ),
  segment_url = c(
    "https://www.gmhcommunities.com/student-living/",
    "https://www.gmhcommunities.com/residential-living/",
    "https://www.gmhcommunities.com/innovative-living/",
    "https://www.gmhcommunities.com/past-projects/"
  ),
  segment_logo_url = c(
    "https://storage.googleapis.com/gmh-images/logos/student-living/gmh-student-living-logo.svg",
    "https://storage.googleapis.com/gmh-images/logos/residential-living/gmh-residential-living-logo.svg",
    "https://storage.googleapis.com/gmh-images/logos/innovative-living/gmh-innovative-living-logo.svg",
    "https://storage.googleapis.com/gmh-images/logos/past-projects/gmh-past-projects-logo.svg"
  ),
  segment_banner_url = c(
    "https://storage.googleapis.com/gmh-images/banners/gmh-student-living-banner.png",
    "https://storage.googleapis.com/gmh-images/banners/gmh-residential-living-banner.png",
    "https://storage.googleapis.com/gmh-images/banners/gmh-innovative-living-banner.png",
    "https://storage.googleapis.com/gmh-images/banners/gmh-past-projects-banner.png"
  )
)

readr::write_csv(gmh_segments_tbl, "data-raw/data/working/gmh/gmh_segments.csv")
