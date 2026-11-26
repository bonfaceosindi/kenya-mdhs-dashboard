#!/usr/bin/env Rscript
# =============================================================================
# extract_tables.R
# Turn tables in the 2025–26 Kenya Mini DHS final report (PDF) into tidy data
# for the Shiny dashboard.
#
# Run from the project root:
#   Rscript extract/extract_tables.R                    # build app/data/ from the registry
#   Rscript extract/extract_tables.R --draft 2.4C 3.9C  # add draft registry rows (guessed headers)
#   Rscript extract/extract_tables.R --draft-county     # draft every county ("C") table not yet in the registry
#
# How it works
#   1. Read every page's text layer with pdftools (layout is preserved).
#   2. Find each table by its title line ("Table 7.4.2C ...") and cut out its block.
#   3. Parse data rows: a label followed by exactly n values, n = number of columns
#      in the registry. Track panel banners (ALL CAPS) and group headings.
#   4. Column names come from extract/table_registry.csv. A draft of each row is
#      guessed from the header layout and must be checked by hand (reviewed = TRUE).
#   5. Validate, then write app/data/kmdhs.rds, catalog.csv, counties.rds, qa_report.csv.
# =============================================================================

suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

PDF         <- "KMDHS_2025-26_Final_Report/KMDHS_2025-26_Final_Report.pdf"
REGISTRY    <- "extract/table_registry.csv"
OUT_DIR     <- "app/data"
PAGE_OFFSET <- 40   # printed page number + 40 = PDF page index
GEO_API     <- "https://www.geoboundaries.org/api/current/gbOpen/KEN/ADM1/"

if (!file.exists(PDF)) stop("Run this script from the project root (", PDF, " not found).")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

CHAPTERS <- c(
  "1" = "Survey methodology", "2" = "Housing & household population",
  "3" = "Respondents & health insurance", "4" = "Marriage & sexual activity",
  "5" = "Fertility", "6" = "Fertility preferences", "7" = "Family planning",
  "8" = "Infant & child mortality", "9" = "Maternal & newborn health",
  "10" = "Child health", "11" = "Child nutrition & feeding", "12" = "Malaria",
  "13" = "Water, sanitation & hygiene", "14" = "Gender-based violence",
  "15" = "Adult & maternal mortality"
)

# 47 counties in official KNBS code order (the order used in every county table)
COUNTIES <- c(
  "Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu", "Taita Taveta", "Garissa",
  "Wajir", "Mandera", "Marsabit", "Isiolo", "Meru", "Tharaka-Nithi", "Embu",
  "Kitui", "Machakos", "Makueni", "Nyandarua", "Nyeri", "Kirinyaga", "Murang'a",
  "Kiambu", "Turkana", "West Pokot", "Samburu", "Trans Nzoia", "Uasin Gishu",
  "Elgeyo-Marakwet", "Nandi", "Baringo", "Laikipia", "Nakuru", "Narok",
  "Kajiado", "Kericho", "Bomet", "Kakamega", "Vihiga", "Bungoma", "Busia",
  "Siaya", "Kisumu", "Homa Bay", "Migori", "Kisii", "Nyamira", "Nairobi City"
)
county_key <- function(x) gsub("[^a-z]", "", tolower(x))
COUNTY_LOOKUP <- setNames(COUNTIES, county_key(COUNTIES))
COUNTY_LOOKUP[c("nairobi", "taitataveta", "elgeyomarakwet", "tharakanithi", "tharaka", "muranga")] <-
  c("Nairobi City", "Taita Taveta", "Elgeyo-Marakwet", "Tharaka-Nithi", "Tharaka-Nithi", "Murang'a")
std_county <- function(x) unname(COUNTY_LOOKUP[county_key(x)])   # NA if not a county

# ---- 1. Read the PDF ---------------------------------------------------------
message("Reading PDF text ...")
pages <- suppressMessages(pdf_text(PDF)) |>
  str_replace_all("[‘’`]", "'") |>
  str_replace_all(" ", " ")
page_lines <- map(pages, \(p) str_split(p, "\n")[[1]])

# ---- 2. Line helpers -----------------------------------------------------------
VALUE_RE <- "^(\\(?-?[0-9][0-9,]*(\\.[0-9]+)?\\)?|\\*|-|na|\\(na\\)|<0\\.1|a)$"

tokens <- function(line) {
  m <- gregexpr("\\S+", line)[[1]]
  if (m[1] == -1) return(list(tok = character(), s = integer(), e = integer()))
  len <- attr(m, "match.length")
  list(tok = regmatches(line, list(m))[[1]], s = as.integer(m), e = as.integer(m + len - 1))
}

n_trailing_values <- function(tok) {
  n <- 0
  for (t in rev(tok)) if (str_detect(t, VALUE_RE)) n <- n + 1 else break
  n
}

clean_label <- function(x) {
  x |>
    str_squish() |>
    str_replace("(?<=[A-Za-z\\)])\\d+(,\\d+)*$", "") |>   # footnote digits glued to a word
    str_replace_all("(?<=[A-Za-z])- (?=[a-z])", "") |>      # "sterili- sation" -> "sterilisation"
    str_squish()
}

# Panel banners are ALL CAPS ("CURRENTLY MARRIED WOMEN"), allowing a plural "s" ("NON-ITNs")
is_caps_line <- function(x) {
  letters_only <- gsub("[^A-Za-z]", "", x)
  nchar(letters_only) >= 4 && nchar(gsub("[^A-Z]", "", x)) / nchar(letters_only) >= 0.75
}
panel_name <- function(x) {
  x <- clean_label(x)
  if (grepl("[a-z]", x)) x else str_to_sentence(x)
}

parse_value <- function(v) {
  tibble(
    value = suppressWarnings(as.numeric(str_remove_all(v, "[(),<]"))),
    flag  = case_when(
      str_detect(v, "^\\(.*\\)$") & v != "(na)" ~ "25-49 cases",
      v == "*"                                  ~ "suppressed",
      v %in% c("-", "na", "(na)")               ~ "not applicable",
      v == "a"                                  ~ "not calculated",
      v == "<0.1"                               ~ "less than 0.1",
      TRUE                                      ~ NA_character_
    )
  ) |>
    mutate(value = if_else(flag %in% c("suppressed", "not applicable", "not calculated"), NA_real_, value))
}

# ---- 3. Locate tables -----------------------------------------------------------
TITLE_ANY <- "^\\s*Table \\d+(\\.\\d+)+[A-Z]?(\\s|—|$)"

is_real_title <- function(line) {
  # Reject in-text mentions such as "Table 2.20 presents ..." or "Table 11.9)."
  rest <- str_remove(str_trim(line), "^Table \\d+(\\.\\d+)+[A-Z]?")
  str_detect(rest, "^(\\s+[A-Z(]|—|$)") && !str_detect(rest, "^\\)")
}

# Returns a list of blocks (character vectors), one per page the table appears on.
find_blocks <- function(id) {
  pat <- paste0("^\\s*Table ", str_replace_all(id, "\\.", "\\\\."), "(\\s|—|$)")
  out <- list()
  for (p in seq_along(page_lines)[-(1:PAGE_OFFSET)]) {
    L <- page_lines[[p]]
    starts <- which(str_detect(L, pat) & map_lgl(L, is_real_title))
    for (s in starts) {
      later  <- which(str_detect(L, TITLE_ANY) & map_lgl(L, is_real_title))
      later  <- later[later > s]
      end    <- if (length(later)) later[1] - 1 else length(L)
      out[[length(out) + 1]] <- list(lines = L[s:end], pdf_page = p)
    }
  }
  out
}

subtitle_end <- function(L) {
  # The subtitle ends with "Kenya Mini DHS 2025–26", which sometimes wraps ("... Mini DHS" / "2025–26")
  hit <- which(str_detect(head(L, 12), "Mini DHS,? 2025|^\\s*(DHS,? )?2025–26\\s*$"))
  if (length(hit)) max(hit) else 1
}

table_meta <- function(block) {
  L <- block$lines
  se <- subtitle_end(L)
  title <- str_squish(L[1]) |> str_remove("^Table \\S+\\s*")
  # Some titles wrap onto a 2nd line before the subtitle ("... by county")
  universe <- str_squish(paste(L[2:se], collapse = " "))
  list(title = title, universe = universe,
       pdf_page = block$pdf_page, page = block$pdf_page - PAGE_OFFSET)
}

# ---- 4. Parse a block's data rows ------------------------------------------------
STUB_RE <- "^\\s*(County|Background|characteristic|Characteristic)\\b"
END_RE  <- "^\\s*(Notes?:|\\d+$|[A-Za-z/-]+ = |CI = |.*• *\\d+ *$|\\d+ *•)"

parse_block <- function(L, ncol) {
  tk  <- map(L, tokens)
  nv  <- map_int(tk, \(t) n_trailing_values(t$tok))
  nt  <- map_int(tk, \(t) length(t$tok))
  is_data <- nv >= ncol & nt > ncol
  se <- subtitle_end(L)
  first_data <- which(is_data & seq_along(L) > se)[1]
  if (is.na(first_data)) return(NULL)

  stub <- which(str_detect(L, STUB_RE) & seq_along(L) > se & seq_along(L) < first_data)
  start <- if (length(stub)) max(stub) + 1 else first_data
  end_hits <- which(str_detect(L, END_RE) & seq_along(L) > first_data)
  end <- if (length(end_hits)) end_hits[1] - 1 else length(L)
  idx <- start:end

  # Repair rows where a value slipped onto its own line (e.g. Table 14.4C "Total"):
  # merge a values-only line into the row above when together they make ncol values.
  for (i in idx[-1]) {
    p <- i - 1
    while (p > start && nt[p] == 0) p <- p - 1
    if (nt[i] > 0 && nt[i] == nv[i] && nt[i] < ncol && nt[p] > nv[p] && nv[p] + nt[i] == ncol) {
      lab_n <- nt[p] - nv[p]
      vals  <- order(c(tk[[p]]$s[-(1:lab_n)], tk[[i]]$s))
      tk[[p]] <- list(tok = c(head(tk[[p]]$tok, lab_n), c(tail(tk[[p]]$tok, nv[p]), tk[[i]]$tok)[vals]),
                      s   = c(head(tk[[p]]$s, lab_n),   c(tail(tk[[p]]$s, nv[p]),   tk[[i]]$s)[vals]),
                      e   = c(head(tk[[p]]$e, lab_n),   c(tail(tk[[p]]$e, nv[p]),   tk[[i]]$e)[vals]))
      nt[p] <- length(tk[[p]]$tok); nv[p] <- ncol; is_data[p] <- TRUE
      tk[i] <- list(list(tok = character(), s = integer(), e = integer())); nt[i] <- 0L; nv[i] <- 0L
    }
  }

  data_idx <- idx[is_data[idx]]
  base <- min(map_int(tk[data_idx], \(t) t$s[1]))
  indent <- \(i) tk[[i]]$s[1]

  rows <- list(); panel <- NA_character_; group <- NA_character_
  prefix <- NULL; last_was_group <- FALSE
  nonblank <- idx[nt[idx] > 0]

  for (k in seq_along(nonblank)) {
    i <- nonblank[k]; t <- tk[[i]]
    nxt <- if (k < length(nonblank)) nonblank[k + 1] else NA

    if (is_data[i]) {
      lab <- paste(head(t$tok, nt[i] - ncol), collapse = " ")
      if (!is.null(prefix)) {
        lab <- if (str_ends(prefix, "[-/]")) paste0(prefix, lab) else paste(prefix, lab)
        prefix <- NULL
      }
      rows[[length(rows) + 1]] <- c(list(panel = panel, group = group, category = clean_label(lab)),
                                    as.list(setNames(tail(t$tok, ncol), paste0("v", seq_len(ncol)))))
      last_was_group <- FALSE
    } else if (nv[i] == 0) {
      txt <- str_squish(L[i])
      if (is_caps_line(txt)) {
        panel <- panel_name(txt); group <- NA_character_; last_was_group <- FALSE
      } else if (i < first_data && indent(i) > base) {
        next   # leftover header text below the stub line (e.g. "last night")
      } else if (str_ends(txt, "[-/]")) {
        prefix <- txt
      } else if (indent(i) <= base + 0) {
        group <- clean_label(txt); prefix <- NULL; last_was_group <- TRUE
      } else if (!is.na(nxt) && is_data[nxt] && indent(nxt) > indent(i)) {
        prefix <- if (is.null(prefix)) txt else paste(prefix, txt)   # wrapped category label
      } else if (last_was_group) {
        group <- clean_label(paste(group, txt))                        # wrapped group heading
      } else {
        prefix <- if (is.null(prefix)) txt else paste(prefix, txt)
      }
    }
  }
  bind_rows(rows)
}

# ---- 5. Guess column headers (draft registry rows) ----------------------------------
guess_columns <- function(L, ncol) {
  tk <- map(L, tokens)
  nv <- map_int(tk, \(t) n_trailing_values(t$tok))
  nt <- map_int(tk, \(t) length(t$tok))
  se <- subtitle_end(L)
  data_idx <- which(nv >= ncol & nt > ncol & seq_along(L) > se)
  first_data <- data_idx[1]
  spans <- map(data_idx, \(i) {
    t <- tk[[i]]; j <- (nt[i] - ncol + 1):nt[i]; cbind(t$s[j], t$e[j])
  })
  col_s <- apply(sapply(spans, \(m) m[, 1]), 1, median)
  col_e <- apply(sapply(spans, \(m) m[, 2]), 1, median)
  centers <- (col_s + col_e) / 2

  words <- list()
  for (i in (se + 1):(first_data - 1)) {
    t <- tk[[i]]
    if (!length(t$tok) || all(str_detect(t$tok, "^[0-9,]+$"))) next
    for (w in seq_along(t$tok)) {
      if (t$e[w] < col_s[1] - 1) next   # stub column
      j <- which.min(abs((t$s[w] + t$e[w]) / 2 - centers))
      words[[length(words) + 1]] <- tibble(col = j, line = i, s = t$s[w], word = t$tok[w])
    }
  }
  w <- bind_rows(words)
  map_chr(seq_len(ncol), \(j) {
    x <- w |> filter(col == j) |> arrange(line, s) |> pull(word)
    if (!length(x)) return(paste0("col", j))
    x <- str_replace(x, "(?<=[a-z\\)])\\d+(,\\d+)*$", "")
    str_squish(str_replace_all(paste(x, collapse = " "), "(?<=\\w)- (?=\\w)", "")) |>
      str_to_sentence(locale = "en") |> str_replace_all("\\bitn\\b", "ITN")
  })
}

guess_ncol <- function(L) {
  nv <- map_int(L, \(l) n_trailing_values(tokens(l)$tok))
  nv <- nv[nv >= 1]
  as.integer(names(sort(table(nv), decreasing = TRUE))[1])
}

read_registry <- function() {
  if (!file.exists(REGISTRY)) {
    return(tibble(table_id = character(), reviewed = logical(), columns = character(),
                  sum_cols = character(), unit = character(), missing_ok = character(), notes = character()))
  }
  read_csv(REGISTRY, col_types = cols(.default = col_character()), na = "") |>
    mutate(reviewed = as.logical(reviewed))
}

draft_rows <- function(ids) {
  reg <- read_registry()
  new <- setdiff(ids, reg$table_id)
  drafts <- map(new, \(id) {
    b <- find_blocks(id)
    if (!length(b)) { message("  ", id, ": not found"); return(NULL) }
    L <- b[[1]]$lines
    n <- guess_ncol(L)
    cols <- tryCatch(guess_columns(L, n), error = \(e) paste0("col", seq_len(n)))
    message(sprintf("  %-8s p.%d  %d columns", id, b[[1]]$pdf_page - PAGE_OFFSET, n))
    tibble(table_id = id, reviewed = FALSE, columns = paste(cols, collapse = " | "),
           sum_cols = NA, unit = "percent", missing_ok = NA, notes = "draft: check column names")
  })
  reg <- bind_rows(reg, bind_rows(drafts))
  write_csv(reg, REGISTRY, na = "")
  message("Registry now has ", nrow(reg), " rows (", sum(!reg$reviewed), " to review).")
}

all_table_ids <- function() {
  ids <- map(page_lines[-(1:PAGE_OFFSET)], \(L) {
    L <- L[str_detect(L, TITLE_ANY) & map_lgl(L, is_real_title)]
    str_match(L, "Table (\\d+(\\.\\d+)+[A-Z]?)")[, 2]
  })
  unique(unlist(ids))
}

# ---- 6. Build one table ---------------------------------------------------------------
split_cols <- function(x) str_trim(str_split(x, "\\|")[[1]])

unit_for <- function(indicator, table_unit) {
  case_when(
    str_detect(indicator, regex("^Number", TRUE))                 ~ "count",
    str_detect(indicator, regex("fertility rate", TRUE))          ~ "births per woman",
    str_detect(indicator, regex("mortality", TRUE))               ~ "per 1,000",
    str_detect(indicator, regex("^(Mean|Median)", TRUE))          ~ "number",
    TRUE                                                          ~ table_unit
  )
}

build_table <- function(r) {
  blocks <- find_blocks(r$table_id)
  if (!length(blocks)) stop("Table ", r$table_id, " not found in the PDF.")
  cols <- split_cols(r$columns)
  if (anyDuplicated(cols)) stop("Table ", r$table_id, ": duplicate column names in registry.")
  ncol <- length(cols)
  meta <- table_meta(blocks[[1]])

  wide <- map(blocks, \(b) parse_block(b$lines, ncol)) |> bind_rows()
  if (!nrow(wide)) stop("Table ", r$table_id, ": no data rows with ", ncol, " values.")
  names(wide)[-(1:3)] <- cols

  # County tables: standardise county names and set the group
  wide <- wide |>
    mutate(
      county = std_county(category),
      group  = case_when(!is.na(county)                    ~ "County",
                         str_detect(category, "^Total")    ~ "Total",
                         TRUE                              ~ group),
      category = coalesce(county, category)
    ) |>
    select(-county)

  long <- wide |>
    mutate(row = row_number()) |>
    pivot_longer(all_of(cols), names_to = "indicator", values_to = "raw") |>
    mutate(col = match(indicator, cols))

  # Denominator for each indicator = nearest "Number ..." column to its right
  num_cols <- which(str_detect(cols, regex("^Number", TRUE)))
  denom_of <- map_int(seq_along(cols), \(j) { k <- num_cols[num_cols > j]; if (length(k)) k[1] else NA_integer_ })
  n_lookup <- long |> filter(col %in% num_cols) |>
    transmute(row, col, n = parse_value(raw)$value)

  long |>
    mutate(ncol_ref = denom_of[col]) |>
    left_join(n_lookup, by = c("row", "ncol_ref" = "col")) |>
    bind_cols(parse_value(long$raw)) |>
    mutate(
      table_id = r$table_id,
      chapter  = as.integer(str_extract(r$table_id, "^\\d+")),
      topic    = unname(CHAPTERS[as.character(chapter)]),
      title    = meta$title,
      universe = meta$universe,
      page     = meta$page,
      unit     = unit_for(indicator, coalesce(r$unit, "percent")),
      is_denominator = col %in% num_cols,
      is_total_col   = str_detect(indicator, "^Total( \\d)?$")   # the "100.0" columns
    ) |>
    select(table_id, chapter, topic, title, page, panel, group, category,
           indicator, col, value, flag, n, unit, is_denominator, is_total_col, universe, row)
}

# ---- 7. Validation ---------------------------------------------------------------------
check <- function(table_id, check, pass, detail = "") {
  tibble(table_id = table_id, check = check, pass = pass, detail = detail)
}

validate_table <- function(d, r) {
  id <- r$table_id
  out <- list(check(id, "rows found", nrow(d) > 0, paste(n_distinct(d$row), "rows")))

  # Every panel that contains counties must contain all 47, once each
  cty <- d |> filter(group == "County") |> distinct(panel, row, category)
  if (nrow(cty)) {
    by_panel <- cty |> group_by(panel) |>
      summarise(n = n(), missing = paste(setdiff(COUNTIES, category), collapse = ", "),
                dup = any(duplicated(category)), .groups = "drop")
    # Counties absent from the PDF itself are listed in the registry's missing_ok column
    ok_missing <- if (!is.na(r$missing_ok %||% NA)) str_trim(str_split(r$missing_ok, ";")[[1]]) else character()
    by_panel <- by_panel |>
      mutate(missing = map_chr(missing, \(m) paste(setdiff(str_split(m, ", ")[[1]], c(ok_missing, "")), collapse = ", ")))
    miss <- by_panel$missing[nzchar(by_panel$missing)]
    expected <- 47 - length(ok_missing)
    out <- c(out, list(check(id, "47 counties", all(by_panel$n == expected & !by_panel$dup),
                             paste0(paste(by_panel$n, collapse = "/"),
                                    if (length(miss)) paste(" missing:", paste(miss, collapse = "; ")) else ""))))
  }

  # Unique keys
  dup <- d |> count(panel, group, category, indicator) |> filter(n > 1)
  out <- c(out, list(check(id, "unique keys", nrow(dup) == 0,
                           if (nrow(dup)) paste(head(dup$category, 3), collapse = ", ") else "")))

  # Percent distributions sum to 100
  sum_cols <- if (!is.na(r$sum_cols) && nzchar(r$sum_cols)) split_cols(r$sum_cols) else NULL
  cols <- split_cols(r$columns)
  if (is.null(sum_cols) && "Total" %in% cols) {
    tot <- match("Total", cols)
    trow <- d |> filter(str_detect(category, "^Total")) |> filter(row == min(row))
    for (s in rev(seq_len(tot - 1))[-1]) {
      v <- trow$value[match(cols[s:(tot - 1)], trow$indicator)]
      if (!anyNA(v) && abs(sum(v) - 100) <= 0.3) { sum_cols <- cols[s:(tot - 1)]; break }
    }
  }
  if (!is.null(sum_cols)) {
    sums <- d |> filter(indicator %in% sum_cols, !flag %in% "suppressed") |>
      group_by(row, category) |> summarise(s = sum(value), k = n(), .groups = "drop") |>
      filter(k == length(sum_cols))
    tol <- suppressWarnings(as.numeric(r$sum_tol %||% NA))   # per-table override, documented in notes
    bad <- sums |> filter(abs(s - 100) > coalesce(tol, 0.35))
    out <- c(out, list(check(id, "distribution sums to 100", nrow(bad) == 0,
                             if (nrow(bad)) paste0(bad$category, "=", bad$s, collapse = "; ")
                             else paste(nrow(sums), "rows,", length(sum_cols), "cols"))))
  } else if ("Total" %in% cols) {
    out <- c(out, list(check(id, "distribution sums to 100", FALSE,
                             "has a Total column but no columns sum to 100; set sum_cols (and sum_tol) in the registry")))
  }
  bind_rows(out)
}

validate_totals <- function(data) {
  # County table Total must equal a Total in its national table, for shared indicators
  ids <- unique(data$table_id)
  pairs <- ids[str_ends(ids, "C") & str_remove(ids, "C$") %in% ids]
  map(pairs, \(cid) {
    nid <- str_remove(cid, "C$")
    ct <- data |> filter(table_id == cid, group == "Total", !is_denominator) |>
      filter(row == min(row)) |> select(indicator, c = value)
    nt <- data |> filter(table_id == nid, group == "Total", !is_denominator) |>
      group_by(indicator) |> summarise(n = list(value), .groups = "drop")
    j <- inner_join(ct, nt, by = "indicator") |>
      mutate(ok = map2_lgl(c, n, \(a, b) any(abs(a - b) <= 0.05, na.rm = TRUE)))
    check(cid, paste("total matches", nid), nrow(j) > 0 && all(j$ok),
          paste0(sum(j$ok), "/", nrow(j), " shared indicators agree"))
  }) |> bind_rows()
}

# ---- 8. County boundaries (geoBoundaries ADM1) --------------------------------------------
build_counties <- function(path = file.path(OUT_DIR, "counties.rds")) {
  if (file.exists(path)) return(invisible(readRDS(path)))
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is needed once to build the county map.")
  message("Downloading county boundaries from geoBoundaries ...")
  meta <- jsonlite::fromJSON(GEO_API)
  shp  <- sf::read_sf(meta$simplifiedGeometryGeoJSON) |> sf::st_make_valid()
  shp$county <- std_county(shp$shapeName)
  if (anyNA(shp$county) || n_distinct(shp$county) != 47)
    stop("County names did not match: ", paste(shp$shapeName[is.na(shp$county)], collapse = ", "))
  # Store plain coordinates so the app needs no 'sf' (works in Shinylive/webR)
  geo <- map(seq_len(nrow(shp)), \(i) {
    xy <- sf::st_coordinates(sf::st_cast(sf::st_geometry(shp)[i], "MULTIPOLYGON"))
    ring <- interaction(xy[, "L1"], xy[, "L2"], xy[, "L3"], drop = TRUE)
    parts <- split(as.data.frame(xy[, 1:2]), ring)
    lng <- unlist(map(parts, \(p) c(p$X, NA))); lat <- unlist(map(parts, \(p) c(p$Y, NA)))
    ctr <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(shp)[i])))
    tibble(county = shp$county[i], lng = list(unname(lng)), lat = list(unname(lat)),
           cx = ctr[1], cy = ctr[2])
  }) |> bind_rows()
  geo <- geo[match(COUNTIES, geo$county), ]
  attr(geo, "source") <- paste("geoBoundaries gbOpen KEN ADM1,", meta$boundaryYearRepresented %||% "", "CC BY 4.0")
  saveRDS(geo, path)
  invisible(geo)
}

# ---- 9. Main ---------------------------------------------------------------------------
# Set KMDHS_NO_MAIN=1 to source() the functions above without running the build.
if (Sys.getenv("KMDHS_NO_MAIN") != "1") {
args <- commandArgs(trailingOnly = TRUE)

if (length(args) && args[1] == "--draft") {
  draft_rows(args[-1]); quit(save = "no")
}
if (length(args) && args[1] == "--draft-county") {
  draft_rows(sort(all_table_ids()[str_ends(all_table_ids(), "C")])); quit(save = "no")
}

reg <- read_registry() |> filter(reviewed)
if (!nrow(reg)) stop("No reviewed tables in ", REGISTRY, ".")
message("Building ", nrow(reg), " tables ...")

data <- map(seq_len(nrow(reg)), \(i) {
  r <- reg[i, ]
  tryCatch(build_table(r), error = \(e) { message("  FAILED ", r$table_id, ": ", conditionMessage(e)); NULL })
}) |> bind_rows()

qa <- bind_rows(
  map(seq_len(nrow(reg)), \(i) {
    r <- reg[i, ]; d <- data |> filter(table_id == r$table_id)
    if (!nrow(d)) return(check(r$table_id, "rows found", FALSE, "table failed to build"))
    validate_table(d, r)
  }),
  validate_totals(data)
)
write_csv(qa, "extract/qa_report.csv")

failed <- qa |> filter(!pass)
if (nrow(failed)) {
  print(failed, n = Inf, width = 200)
  stop(nrow(failed), " validation check(s) failed. See extract/qa_report.csv. Nothing was written to ", OUT_DIR, ".")
}

tidy <- data |>
  filter(!is_total_col, !is_denominator) |>
  select(table_id, chapter, topic, title, page, panel, group, category,
         indicator, value, flag, n, unit)

catalog <- data |>
  group_by(table_id, chapter, topic, title, page, universe) |>
  summarise(
    county_level = any(group == "County"),
    indicators   = paste(unique(indicator[!is_denominator & !is_total_col]), collapse = " | "),
    breakdowns   = paste(unique(na.omit(group)), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(chapter, numeric_version(str_remove(table_id, "C$")), table_id)

saveRDS(tidy, file.path(OUT_DIR, "kmdhs.rds"))
write_csv(catalog, file.path(OUT_DIR, "catalog.csv"))
build_counties()

message(sprintf("Done: %d tables, %s values, %d checks passed.",
                n_distinct(tidy$table_id), format(nrow(tidy), big.mark = ","), nrow(qa)))
}
