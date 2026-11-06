# Reading the uploaded file and matching its columns ----------------------------

MAX_ROWS <- 2e6

# The items Triangulator needs, with the help text shown next to each one.
FIELDS <- list(
  claim_id      = list(label = "Claim ID",
                       help  = "Identifies each claim. The same claim appears on several rows, one for each valuation date."),
  policy_id     = list(label = "Policy ID (optional)", optional = TRUE,
                       help  = "The policy the claim belongs to. Shown in the claim tables."),
  date_stamp    = list(label = "Valuation date",
                       help  = "The date the figures on that row were recorded (also called the observation or snapshot date)."),
  date_issue    = list(label = "Policy start date (optional)", optional = TRUE,
                       help  = "When the policy started (inception). Needed for underwriting triangles and the unexpired risk (URR) estimate."),
  date_incident = list(label = "Incident date",
                       help  = "When the loss happened. Used for accident triangles."),
  date_reported = list(label = "Report date",
                       help  = "When the claim was reported to you. Used for reported triangles."),
  paid          = list(label = "Paid to date",
                       help  = "Total paid on the claim so far (a running total, not the payment in that period)."),
  case          = list(label = "Outstanding case estimate",
                       help  = "The case reserve still held on the claim at that date. Zero means the claim is settled.")
)
SINGLE_FIELDS <- names(FIELDS)
OPTIONAL_FIELDS <- names(FIELDS)[vapply(FIELDS, function(f) isTRUE(f$optional), logical(1))]
DATE_FIELDS   <- c("date_stamp", "date_issue", "date_incident", "date_reported")
AMOUNT_FIELDS <- c("paid", "case")

field_label <- function(field) FIELDS[[field]]$label

# Names the app uses internally, so grouping columns can't have them.
RESERVED_NAMES <- c(SINGLE_FIELDS, "PTOT", "ITOT", "NTOT", "STOT", "origin", "origin_index",
                    "stamp_index", "development", "development_to", "variable", "value",
                    "row_in_file", "all_claims", "group", "bucket")

# ---- Reading files -------------------------------------------------------------

file_extension <- function(file_name) tolower(tools::file_ext(file_name))

# Sheet names in an Excel file (NULL for CSV).
list_sheets <- function(path, file_name) {
  if (file_extension(file_name) == "csv") return(NULL)
  tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
}

read_claims_file <- function(path, file_name, sheet = NULL) {
  ext <- file_extension(file_name)
  if (!ext %in% c("xlsx", "xls", "csv")) {
    stop_friendly("Please upload an Excel file (.xlsx or .xls) or a CSV file (.csv).")
  }
  df <- tryCatch(
    if (ext == "csv") {
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                      na.strings = c("", "NA", "N/A", "#N/A"), strip.white = TRUE,
                      fileEncoding = "UTF-8-BOM")
    } else {
      readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet, guess_max = 100000)
    },
    error = function(e) {
      stop_friendly("We couldn't read this file. Please check it isn't open in another program, ",
                    "password-protected or damaged. (Details: ", conditionMessage(e), ")")
    }
  )
  # CSVs saved with European settings use ";" between columns and "," for decimals.
  if (ext == "csv" && ncol(df) == 1 && grepl(";", names(df)[1], fixed = TRUE)) {
    df <- utils::read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE,
                           na.strings = c("", "NA", "N/A", "#N/A"), strip.white = TRUE,
                           fileEncoding = "UTF-8-BOM")
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(df) == 0) stop_friendly("The file (or sheet) has column headings but no rows of data.")
  if (nrow(df) > MAX_ROWS) {
    stop_friendly("The file has ", scales::comma(nrow(df)), " rows. Triangulator can handle up to ",
                  scales::comma(MAX_ROWS), " rows. Please split the file, for example by year.")
  }
  # Make sure every column has a sensible, unique name.
  nms <- trimws(names(df))
  nms[is.na(nms) | nms == ""] <- paste0("column_", which(is.na(nms) | nms == ""))
  names(df) <- make.unique(nms, sep = "_")
  df
}

# ---- Guessing the column mapping ----------------------------------------------

empty_mapping <- function() {
  m <- stats::setNames(as.list(rep("", length(SINGLE_FIELDS))), SINGLE_FIELDS)
  m$groupings <- character(0)
  m
}

# Make a best guess at which column is which, from the column names.
guess_mapping <- function(df) {
  cols <- names(df)
  norm <- tolower(gsub("[^A-Za-z0-9]+", "_", cols))
  patterns <- c(
    claim_id      = "claim.*(id|no|num|ref|key|code)|^claims?$",
    policy_id     = "polic.*(id|no|num|ref|key|code)|^polic(y|ies)$",
    date_stamp    = "stamp|valuation|observ|snapshot|as_?at|as_?of|extract|val_?date|period_?end|bordereau|bdx|run_?date",
    date_issue    = "issue|incept|start|underwr|written|commenc",
    date_incident = "incident|accident|loss_?date|date_?of_?loss|occur|event",
    date_reported = "report|notif",
    paid          = "paid|payment",
    case          = "case|outstanding|reserve|estimate|(^|_)o_?s($|_)"
  )
  m <- empty_mapping()
  used <- character(0)
  for (field in names(patterns)) {
    hit <- cols[grepl(patterns[[field]], norm) & !cols %in% used]
    if (length(hit)) {
      m[[field]] <- hit[1]
      used <- c(used, hit[1])
    }
  }
  # Groupings: unused text columns with only a handful of different values.
  sample_rows <- utils::head(df, 50000)
  candidates <- setdiff(cols, used)
  candidates <- candidates[vapply(candidates, function(cl) {
    x <- sample_rows[[cl]]
    (is.character(x) || is.factor(x)) &&
      !grepl("(^|_)(id|no|ref|key|code)$", tolower(cl)) &&
      dplyr::between(dplyr::n_distinct(x, na.rm = TRUE), 1, 30)
  }, logical(1))]
  m$groupings <- utils::head(candidates, 3)
  m
}

# ---- Saving and loading the mapping (same CSV format as earlier versions) -------

MAPPING_FILE_COLUMNS <- c(
  policy_id = "user_policy_id", claim_id = "user_claim_id", groupings = "user_groupings",
  date_incident = "user_date_incident", date_stamp = "user_date_stamp", date_issue = "user_date_issue",
  date_reported = "user_date_reported", paid = "user_paid", case = "user_case"
)

write_mapping_file <- function(mapping, file) {
  out <- lapply(names(MAPPING_FILE_COLUMNS), function(field) {
    if (field == "groupings") paste(mapping$groupings, collapse = ",") else mapping[[field]] %||% ""
  })
  out <- as.data.frame(stats::setNames(out, MAPPING_FILE_COLUMNS), stringsAsFactors = FALSE)
  utils::write.csv(out, file, row.names = FALSE)
}

# Returns list(mapping, problems). Columns that aren't in the data are left blank.
read_mapping_file <- function(path, data_columns) {
  raw <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                                  colClasses = "character"),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0 || !all(MAPPING_FILE_COLUMNS %in% names(raw))) {
    stop_friendly("This doesn't look like a Triangulator column-mapping file. ",
                  "It should be a CSV saved with the 'Save mapping' button.")
  }
  m <- empty_mapping()
  problems <- character(0)
  for (field in names(MAPPING_FILE_COLUMNS)) {
    value <- raw[[MAPPING_FILE_COLUMNS[[field]]]][1]
    value <- if (is.na(value)) "" else trimws(value)
    if (field == "groupings") {
      g <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
      g <- g[g != ""]
      missing <- setdiff(g, data_columns)
      if (length(missing)) {
        problems <- c(problems, paste0("Grouping column '", missing, "' isn't in your data, so it was skipped."))
      }
      m$groupings <- intersect(g, data_columns)
    } else if (value != "" && !value %in% data_columns) {
      problems <- c(problems, paste0("Column '", value, "' (for ", field_label(field),
                                     ") isn't in your data. Please choose it yourself."))
    } else {
      m[[field]] <- value
    }
  }
  list(mapping = m, problems = problems)
}

# ---- Checking the mapping ---------------------------------------------------------

# Returns a character vector of problems (empty if all is well).
validate_mapping <- function(df, mapping) {
  problems <- character(0)
  cols <- names(df)
  for (field in SINGLE_FIELDS) {
    value <- mapping[[field]] %||% ""
    if (value == "" && field %in% OPTIONAL_FIELDS) {
      next
    } else if (value == "") {
      problems <- c(problems, paste0("Choose a column for ", field_label(field), "."))
    } else if (!value %in% cols) {
      problems <- c(problems, paste0("The column '", value, "' (for ", field_label(field), ") isn't in your data."))
    }
  }
  chosen <- unlist(mapping[SINGLE_FIELDS])
  chosen <- chosen[!is.na(chosen) & chosen != ""]
  for (dup in unique(chosen[duplicated(chosen)])) {
    uses <- vapply(names(chosen)[chosen == dup], field_label, character(1))
    problems <- c(problems, paste0("The column '", dup, "' is chosen for ", paste(uses, collapse = " and "),
                                   ". Each item needs its own column."))
  }
  groupings <- mapping$groupings %||% character(0)
  for (g in intersect(groupings, chosen)) {
    problems <- c(problems, paste0("'", g, "' is chosen both as a grouping and as ",
                                   field_label(names(chosen)[chosen == g][1]), "."))
  }
  for (g in intersect(groupings, RESERVED_NAMES)) {
    problems <- c(problems, paste0("The grouping column '", g, "' has a name Triangulator uses internally. ",
                                   "Please rename that column in your file (for example to '", g, "_group')."))
  }
  for (g in setdiff(groupings, cols)) {
    problems <- c(problems, paste0("The grouping column '", g, "' isn't in your data."))
  }
  problems
}

# ---- Converting values -------------------------------------------------------------

DATE_TEXT_FORMATS <- c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y", "%d.%m.%Y",
                       "%m/%d/%Y", "%d/%m/%y", "%d %b %Y", "%d-%b-%Y", "%d %B %Y", "%Y%m%d")

plausible_dates <- function(d) {
  y <- as.integer(format(d[!is.na(d)], "%Y"))
  all(y >= 1900 & y <= 2200)
}

examples_of <- function(x) paste0("'", utils::head(unique(x), 3), "'", collapse = ", ")

# Convert a column to dates. Returns list(value = <Date vector>, problem = NULL or text).
as_claim_date <- function(x) {
  ok  <- function(v) list(value = v, problem = NULL)
  bad <- function(msg) list(value = NULL, problem = msg)
  if (inherits(x, "Date")) return(ok(x))
  if (inherits(x, "POSIXt")) return(ok(as.Date(format(x, "%Y-%m-%d"))))
  if (is.logical(x) && all(is.na(x))) return(ok(as.Date(rep(NA_real_, length(x)))))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA
    present <- x[!is.na(x)]
    if (length(present) == 0) return(ok(as.Date(rep(NA_real_, length(x)))))
    if (all(grepl("^[0-9]+(\\.[0-9]+)?$", present))) {
      x <- as.numeric(x)
    } else {
      best_failures <- present
      for (fmt in DATE_TEXT_FORMATS) {
        d <- as.Date(x, format = fmt)
        failures <- x[!is.na(x) & is.na(d)]
        if (length(failures) == 0 && plausible_dates(d)) return(ok(d))
        if (length(failures) < length(best_failures)) best_failures <- failures
      }
      if (length(best_failures) == 0) best_failures <- present
      return(bad(paste0("some values couldn't be read as dates, for example ",
                        examples_of(best_failures),
                        ". Use a format like 2023-01-31 or 31/01/2023.")))
    }
  }
  if (is.numeric(x)) {
    present <- x[!is.na(x)]
    if (length(present) == 0) return(ok(as.Date(rep(NA_real_, length(x)))))
    # Whole numbers written as YYYYMMDD, e.g. 20230131
    if (all(present >= 19000101 & present <= 22001231 & present == floor(present))) {
      d <- as.Date(as.character(x), format = "%Y%m%d")
      if (!anyNA(d[!is.na(x)])) return(ok(d))
    }
    # Excel's internal date numbers (days since 30 Dec 1899)
    if (all(present >= 1 & present < 109575)) {
      d <- as.Date(floor(x), origin = "1899-12-30")
      if (plausible_dates(d)) return(ok(d))
    }
    return(bad(paste0("this column contains numbers that aren't dates, for example ", examples_of(present), ".")))
  }
  bad("this column doesn't contain dates.")
}

# Convert a column to numbers. Returns list(value, problem).
as_claim_amount <- function(x) {
  if (is.numeric(x)) return(list(value = as.numeric(x), problem = NULL))
  if (is.logical(x) && all(is.na(x))) return(list(value = as.numeric(x), problem = NULL))
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) return(list(value = NULL, problem = "this column doesn't contain numbers."))
  cleaned <- trimws(x)
  cleaned[cleaned == ""] <- NA
  negative <- grepl("^\\(.*\\)$", cleaned)              # (123) means -123 in accounting
  cleaned <- gsub("[(),\u00a3$\u20ac[:space:]]", "", cleaned)
  value <- suppressWarnings(as.numeric(cleaned))
  value[negative & !is.na(value)] <- -value[negative & !is.na(value)]
  failed <- !is.na(cleaned) & is.na(value)
  if (any(failed)) {
    return(list(value = NULL, problem = paste0("some values aren't numbers, for example ",
                                               examples_of(x[failed]), ".")))
  }
  list(value = value, problem = NULL)
}

as_id <- function(x) {
  out <- if (is.numeric(x)) ifelse(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE, digits = 15))
         else trimws(as.character(x))
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

# ---- Building the standard claims table ----------------------------------------------

# Pick out the mapped columns, give them standard names and convert their types.
# Returns list(data, problems). Missing values are kept so the data checks can see them.
standardise_claims <- function(df, mapping) {
  out <- data.frame(row_in_file = seq_len(nrow(df)))
  provided <- function(field) !is.null(mapping[[field]]) && mapping[[field]] != ""
  out$claim_id  <- as_id(df[[mapping$claim_id]])
  out$policy_id <- if (provided("policy_id")) as_id(df[[mapping$policy_id]]) else NA_character_
  for (g in mapping$groupings) {
    out[[g]] <- trimws(as.character(df[[g]]))
  }
  problems <- character(0)
  for (field in DATE_FIELDS) {
    if (!provided(field)) {
      out[[field]] <- as.Date(rep(NA_real_, nrow(df)))
      next
    }
    res <- as_claim_date(df[[mapping[[field]]]])
    if (is.null(res$problem)) out[[field]] <- res$value
    else problems <- c(problems, paste0(field_label(field), " ('", mapping[[field]], "'): ", res$problem))
  }
  for (field in AMOUNT_FIELDS) {
    res <- as_claim_amount(df[[mapping[[field]]]])
    if (is.null(res$problem)) out[[field]] <- res$value
    else problems <- c(problems, paste0(field_label(field), " ('", mapping[[field]], "'): ", res$problem))
  }
  list(data = if (length(problems)) NULL else dplyr::as_tibble(out), problems = problems)
}

# Final tidy-up once the checks have been seen: fill blanks, add the four base
# measures, and make sure there is at least one grouping.
#   PTOT = paid to date, ITOT = incurred (paid + case),
#   NTOT = 1 per reported claim, STOT = 1 if settled (case estimate is zero).
finalise_claims <- function(std, groupings) {
  d <- std[!is.na(std$claim_id) & !is.na(std$date_stamp), ]
  if (nrow(d) == 0) stop_friendly("No rows have both a claim ID and a valuation date.")
  if (length(groupings) == 0) {
    d$all_claims <- "All claims"
    groupings <- "all_claims"
  }
  for (g in groupings) d[[g]][is.na(d[[g]]) | d[[g]] == ""] <- "(blank)"
  d$paid[is.na(d$paid)] <- 0
  d$case[is.na(d$case)] <- 0
  d$PTOT <- d$paid
  d$ITOT <- d$paid + d$case
  d$NTOT <- 1
  d$STOT <- as.numeric(d$case == 0)
  list(data = d, groupings = groupings)
}
