amenity_section_icons <- tibble::tibble(
  type = c(
    "Property",
    "Property",
    "Property",
    "Property",
    "Property",
    "Unit",
    "Unit",
    "Unit",
    "Unit",
    "Unit"
  ),
  category = c(
    "Transportation",
    "Fitness & Recreation",
    "Wellness & Beauty",
    "Work & Study",
    "Convenience & Services",
    "Pet Amenities",
    "Unit Amenities",
    "TV Options",
    "Furniture Options",
    "Premium Options"
  ),
  icon = c(
    "truck-front",
    "activity",
    "heart-pulse",
    "laptop",
    "buildings",
    "egg",
    "house-door",
    "tv",
    "box-seam",
    "stars"
  )
)

amenities_data <- dplyr::bind_rows(
  # Transportation
  tibble::tibble(
    type = "Property",
    category = "Transportation",
    amenity = c(
      "University Shuttle",
      "Private Shuttle",
      "Limited Access Gates",
      "EV Charging Stations",
      "Car Sharing Services"
    ),
    icon = c(
      "bus-front",
      "taxi-front",
      "door-closed",
      "plug",
      "share"
    )
  ),
  # Fitness & Recreation
  tibble::tibble(
    type = "Property",
    category = "Fitness & Recreation",
    amenity = c(
      "Fitness Center",
      "Game Room",
      "Pool",
      "Hot Tub",
      "Sauna/Spa",
      "Cycling/Yoga Studio",
      "Sand Volleyball Court",
      "Basketball Court",
      "Outdoor Grill Area"
    ),
    icon = c(
      "universal-access",
      "controller",
      "water",
      "droplet-half",
      "moisture",
      "bicycle",
      "dribbble",
      "trophy",
      "fire"
    )
  ),
  # Continue for other categories...
  tibble::tibble(
    type = "Property",
    category = "Wellness & Beauty",
    amenity = c("Spray Tanning", "UV Tanning", "Wellness Classes"),
    icon = c("droplet", "sun", "heart-pulse")
  ),
  # Work & Study
  tibble::tibble(
    type = "Property",
    category = "Work & Study",
    amenity = c("Computer Lounge", "Co-Working/Study Spaces", "Free Printing"),
    icon = c("pc-display", "people", "printer")
  ),
  # Convenience & Services
  tibble::tibble(
    type = "Property",
    category = "Convenience & Services",
    amenity = c(
      "24hr Package System",
      "Smart Vending",
      "Mini Market",
      "Coffee Bar",
      "Retail",
      "Movie Theatre",
      "Rentable Guest Suite",
      "24hr Concierge"
    ),
    icon = c(
      "box-seam",
      "cart",
      "shop",
      "cup-hot",
      "bag",
      "film",
      "house",
      "person-workspace"
    )
  ),
  # Pet Amenities
  tibble::tibble(
    type = "Property",
    category = "Pet Amenities",
    amenity = c("Pets Allowed", "Dog Wash", "Dog Park"),
    icon = c("patch-check", "tropical-storm", "tree")
  ),
  # Unit Amenities
  tibble::tibble(
    type = "Unit",
    category = "Unit Amenities",
    amenity = c(
      "Private Bathrooms",
      "Walk-in Closets",
      "Washer / Dryer in Unit",
      "Smart Home Technology",
      "Smart Bedroom Locks",
      "Smart Unit Locks",
      "Energy Efficient Appliances",
      "Stainless Steel Appliances",
      "Balconies",
      "Patios",
      "Backyards"
    ),
    icon = c(
      "water",
      "door-open",
      "infinity",
      "robot",
      "key",
      "key-fill",
      "lightning",
      "star",
      "window",
      "door-open",
      "tree"
    )
  ),
  # TV Options
  tibble::tibble(
    type = "Unit",
    category = "TV Options",
    amenity = c("TV Included in Rent", "TV Available for Rent"),
    icon = c("tv", "tv-fill")
  ),
  # Furniture Options
  tibble::tibble(
    type = "Unit",
    category = "Furniture Options",
    amenity = c("Furniture Included in Rent", "Furniture Available for Rent"),
    icon = c("box-seam", "box")
  )
)

# Split data into property and unit amenities
property_amenities <- amenities_data |>
  dplyr::filter(type == "Property")

unit_amenities <- amenities_data |>
  dplyr::filter(type == "Unit")





unit_amenities_premium_inputs <- tibble::tibble(
  category = c(rep("TV Rental Rates", 2), rep("Premium Options", 5)),
  rate_name = c("Bedroom TV Rate", "Common Area TV Rate",
                "Floor Premium", "Poolside Premium", "Top Floor Premium",
                "View Premium", "Other Premium"),
  value = 0
)

property_amenities_data <- tibble::tibble(
  category = rep(
    c(
      "Transportation", "Fitness & Recreation", "Wellness & Beauty", "Work & Study",
      "Convenience & Services", "Pet Amenities"
    ),
    c(5L, 9L, 3L, 3L, 8L, 3L)
  ),
  amenity = c(
    Transportation1 = "University Shuttle",
    Transportation2 = "Private Shuttle",
    Transportation3 = "Limited Access Gates",
    Transportation4 = "EV Charging Stations",
    Transportation5 = "Car Sharing Services",
    `Fitness & Recreation1` = "Fitness Center",
    `Fitness & Recreation2` = "Game Room",
    `Fitness & Recreation3` = "Pool",
    `Fitness & Recreation4` = "Hot Tub",
    `Fitness & Recreation5` = "Sauna/Spa",
    `Fitness & Recreation6` = "Cycling/Yoga Studio",
    `Fitness & Recreation7` = "Sand Volleyball Court",
    `Fitness & Recreation8` = "Basketball Court",
    `Fitness & Recreation9` = "Outdoor Grill Area",
    `Wellness & Beauty1` = "Spray Tanning",
    `Wellness & Beauty2` = "UV Tanning",
    `Wellness & Beauty3` = "Wellness Classes",
    `Work & Study1` = "Computer Lounge",
    `Work & Study2` = "Co-Working/Study Spaces",
    `Work & Study3` = "Free Printing",
    `Convenience & Services1` = "24hr Package System",
    `Convenience & Services2` = "Smart Vending",
    `Convenience & Services3` = "Mini Market",
    `Convenience & Services4` = "Coffee Bar",
    `Convenience & Services5` = "Retail",
    `Convenience & Services6` = "Movie Theatre",
    `Convenience & Services7` = "Rentable Guest Suite",
    `Convenience & Services8` = "24hr Concierge",
    `Pet Amenities1` = "Pets Allowed",
    `Pet Amenities2` = "Dog Wash",
    `Pet Amenities3` = "Dog Park"
  ),
  value = c(
    `Transportation.University Shuttle` = FALSE,
    `Transportation.Private Shuttle` = FALSE,
    `Transportation.Limited Access Gates` = FALSE,
    `Transportation.EV Charging Stations` = FALSE,
    `Transportation.Car Sharing Services` = FALSE,
    `Fitness & Recreation.Fitness Center` = TRUE,
    `Fitness & Recreation.Game Room` = TRUE,
    `Fitness & Recreation.Pool` = FALSE,
    `Fitness & Recreation.Hot Tub` = FALSE,
    `Fitness & Recreation.Sauna/Spa` = FALSE,
    `Fitness & Recreation.Cycling/Yoga Studio` = FALSE,
    `Fitness & Recreation.Sand Volleyball Court` = FALSE,
    `Fitness & Recreation.Basketball Court` = FALSE,
    `Fitness & Recreation.Outdoor Grill Area` = TRUE,
    `Wellness & Beauty.Spray Tanning` = FALSE,
    `Wellness & Beauty.UV Tanning` = FALSE,
    `Wellness & Beauty.Wellness Classes` = FALSE,
    `Work & Study.Computer Lounge` = TRUE,
    `Work & Study.Co-Working/Study Spaces` = TRUE,
    `Work & Study.Free Printing` = TRUE,
    `Convenience & Services.24hr Package System` = TRUE,
    `Convenience & Services.Smart Vending` = TRUE,
    `Convenience & Services.Mini Market` = FALSE,
    `Convenience & Services.Coffee Bar` = TRUE,
    `Convenience & Services.Retail` = TRUE,
    `Convenience & Services.Movie Theatre` = FALSE,
    `Convenience & Services.Rentable Guest Suite` = FALSE,
    `Convenience & Services.24hr Concierge` = FALSE,
    `Pet Amenities.Pets Allowed` = FALSE,
    `Pet Amenities.Dog Wash` = FALSE,
    `Pet Amenities.Dog Park` = FALSE
  ),
  icon = c(
    `Transportation.University Shuttle` = "bus-front",
    `Transportation.Private Shuttle` = "taxi-front",
    `Transportation.Limited Access Gates` = "door-closed",
    `Transportation.EV Charging Stations` = "plug",
    `Transportation.Car Sharing Services` = "share",
    `Fitness & Recreation.Fitness Center` = "universal-access",
    `Fitness & Recreation.Game Room` = "controller",
    `Fitness & Recreation.Pool` = "water",
    `Fitness & Recreation.Hot Tub` = "droplet-half",
    `Fitness & Recreation.Sauna/Spa` = "moisture",
    `Fitness & Recreation.Cycling/Yoga Studio` = "bicycle",
    `Fitness & Recreation.Sand Volleyball Court` = "dribbble",
    `Fitness & Recreation.Basketball Court` = "trophy",
    `Fitness & Recreation.Outdoor Grill Area` = "fire",
    `Wellness & Beauty.Spray Tanning` = "droplet",
    `Wellness & Beauty.UV Tanning` = "sun",
    `Wellness & Beauty.Wellness Classes` = "heart-pulse",
    `Work & Study.Computer Lounge` = "pc-display",
    `Work & Study.Co-Working/Study Spaces` = "people",
    `Work & Study.Free Printing` = "printer",
    `Convenience & Services.24hr Package System` = "box-seam",
    `Convenience & Services.Smart Vending` = "cart",
    `Convenience & Services.Mini Market` = "shop",
    `Convenience & Services.Coffee Bar` = "cup-hot",
    `Convenience & Services.Retail` = "bag",
    `Convenience & Services.Movie Theatre` = "film",
    `Convenience & Services.Rentable Guest Suite` = "house",
    `Convenience & Services.24hr Concierge` = "person-workspace",
    `Pet Amenities.Pets Allowed` = "patch-check",
    `Pet Amenities.Dog Wash` = "tropical-storm",
    `Pet Amenities.Dog Park` = "tree"
  ),
)


survey_amenities_tbl <- tibble::tibble(
  amenity_id = 1:35,
  amenity_type = rep(c("Property", "Unit"), c(31L, 4L)),
  amenity_category = rep(
    c(
      "Transportation", "Fitness & Recreation", "Wellness & Beauty", "Work & Study",
      "Convenience & Services", "Pet Amenities", "Unit Amenities"
    ),
    c(5L, 9L, 3L, 3L, 8L, 3L, 4L)
  ),
  amenity_name = c(
    "University Shuttle", "Private Shuttle", "Limited Access Gates",
    "EV Charging Stations", "Car Sharing Services", "Fitness Center", "Game Room",
    "Pool", "Hot Tub", "Sauna/Spa", "Cycling/Yoga Studio",
    "Sand Volleyball Court", "Basketball Court", "Outdoor Grill Area",
    "Spray Tanning", "UV Tanning", "Wellness Classes", "Computer Lounge",
    "Co-Working/Study Spaces", "Free Printing", "24hr Package System",
    "Smart Vending", "Mini Market", "Coffee Bar", "Retail", "Movie Theatre",
    "Rentable Guest Suite", "24hr Concierge", "Pets Allowed", "Dog Wash",
    "Dog Park", "Private Bathrooms", "Walk-in Closets", "Washer / Dryer in Unit",
    "Smart Home Technology"
  ),
  amenity_icon = c(
    "bus-front", "taxi-front", "door-closed", "plug", "share", "universal-access",
    "controller", "water", "droplet-half", "moisture", "bicycle", "dribbble",
    "trophy", "fire", "droplet", "sun", "heart-pulse", "pc-display", "people",
    "printer", "box-seam", "cart", "shop", "cup-hot", "bag", "film", "house",
    "person-workspace", "patch-check", "tropical-storm", "tree", "water",
    "door-open", "infinity", "robot"
  )
)
