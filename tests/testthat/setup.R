# Runs before the tests: load the app's functions from the R/ folder.
project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
options(triangulator.app_dir = project_root)
for (f in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

# The sample data, prepared exactly as the app does it.
sample_prepared <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      raw <- read_claims_file(file.path(project_root, "www", "sample_database.xlsx"), "sample_database.xlsx")
      mapping <- guess_mapping(raw)
      std <- standardise_claims(raw, mapping)
      fin <- finalise_claims(std$data, mapping$groupings)
      cache <<- list(raw = raw, mapping = mapping, std = std$data, claims = fin$data, groupings = fin$groupings)
    }
    cache
  }
})

# A tiny made-up dataset whose triangles are easy to work out by hand.
#   Claim A: incident 2020; seen in 2020 (paid 100, case 50) and 2021 (paid 200, settled).
#   Claim B: incident 2021; seen in 2021 (case 300) and twice in 2022 - the later
#            2022 row (paid 150, case 250) is the one that counts.
tiny_claims <- function(year_shift = 0) {
  d <- function(x) as.Date(x) + round(year_shift * 365.25)
  data.frame(
    claim_ref  = c("A", "A", "B", "B", "B"),
    policy_ref = c("P1", "P1", "P2", "P2", "P2"),
    line       = c("x", "x", "x", "x", "x"),
    issued     = d(c("2020-01-01", "2020-01-01", "2020-06-01", "2020-06-01", "2020-06-01")),
    incident   = d(c("2020-03-01", "2020-03-01", "2021-02-01", "2021-02-01", "2021-02-01")),
    reported   = d(c("2020-04-01", "2020-04-01", "2021-03-01", "2021-03-01", "2021-03-01")),
    valued     = d(c("2020-06-30", "2021-06-30", "2021-12-31", "2022-05-01", "2022-11-01")),
    paid_amt   = c(100, 200, 0, 100, 150),
    case_amt   = c(50, 0, 300, 250, 250),
    stringsAsFactors = FALSE
  )
}

tiny_mapping <- function(groupings = "line") {
  list(claim_id = "claim_ref", policy_id = "policy_ref", date_stamp = "valued",
       date_issue = "issued", date_incident = "incident", date_reported = "reported",
       paid = "paid_amt", case = "case_amt", groupings = groupings)
}

prepare_tiny <- function(groupings = "line", year_shift = 0) {
  raw <- tiny_claims(year_shift)
  std <- standardise_claims(raw, tiny_mapping(groupings))
  finalise_claims(std$data, groupings)
}

# Pull one measure out of a triangle as "origin/development = value".
cells <- function(tri, measure) {
  x <- tri[tri$variable == measure, ]
  x <- x[order(x$origin, x$development), ]
  stats::setNames(x$value, paste0(x$origin, "/", x$development))
}
