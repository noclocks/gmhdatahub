users <- tibble::tribble(
  ~user_id, ~user_email,
  "771915ed-df41-4fff-97d3-3ab9aadaefb7",              "pat@noclocks.dev",
  "f82021f9-003e-432e-ad76-de45ae43262b",  "rnotowich@gmhcommunities.com",
  "d7c0137d-2c55-4a1d-ba0d-ebf03ad6a864",              "dev@noclocks.dev",
  "04ad92a2-6b44-46e4-abce-0b86ec4de352",     "arippy@gmhcommunities.com",
  "09e5fa90-4386-4caf-b6a5-aeda1f480376",          "patrick@noclocks.dev",
  "96026b05-5412-4627-9c75-3ce4a0af5140",   "patrickhoward2011@gmail.com",
  "715f6342-074b-449d-9075-01803258fa1f",           "dhamilton@gmhcp.com",
  "7db7e249-1077-448c-b2a6-70156668956c",           "rginsburg@gmhcp.com",
  "f2b41204-34c2-493f-8c9c-17e4460ecdb8",        "amccardell@gmh-inc.com",
  "4250208b-8013-4e82-a636-180ff9326a25",     "jevers@gmhcommunities.com",
  "7e7b0bf0-7342-4812-80b2-4fef66c2a39f",            "apiquero@gmhcp.com",
  "68af9fe4-305d-4001-ac01-bbe7b7f0f550",    "bwillis@gmhcommunities.com",
  "f0573d49-020f-415e-ab64-07972d1ba803",     "bmccovy@gmhuniversity.com",
  "58591ff5-d357-4bb1-b1dc-dc9c64171b68", "bmawhinney@gmhcommunities.com",
  "b6cd1eab-081c-41a3-8df6-216dd4bdfcd8",     "rheidel@gmhuniversity.com",
  "1eea4f94-91bc-416d-b448-a0c482d59476",    "lbremer@gmhcommunities.com",
  "4bfd3fc8-5095-4c78-a887-2a0f1fc7cbdf",             "adoucet@gmhcp.com",
  "6e3b927c-3d1f-4161-8c00-8a2f5c9dc0bf",     "lbremer@gmhuniversity.com",
  "971d03e1-a265-4fcb-a8e9-74c61ea3935d",            "drusso@gmh-inc.com",
  "81d2f81f-a2d2-4caa-ad93-227f5988c576",   "severett@gmhcommunities.com"
)

pool::dbAppendTable(
  pool,
  DBI::SQL("survey.users"),
  users,
  overwrite = FALSE
)
