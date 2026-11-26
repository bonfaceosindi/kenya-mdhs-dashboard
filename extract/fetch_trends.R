#!/usr/bin/env Rscript
# =============================================================================
# fetch_trends.R
# Multi-year Kenya DHS indicators (1989 to 2025–26) for the Trends tab.
#
# Run from the project root, after extract_tables.R has built app/data/kmdhs.rds:
#   Rscript extract/fetch_trends.R
#
# How it works
#   1. The indicator list is extract/trend_indicators.csv (hand-curated: label,
#      topic, unit, whether higher is better, and the matching 2025–26 report key).
#   2. Values for every Kenya DHS survey come from the DHS Program API, which uses
#      the same indicator definitions in every survey. Counties exist from 2014;
#      earlier surveys report the former provinces only.
#   3. Where the API does not yet have a 2025–26 value (source_2025 = "kmdhs"),
#      it is taken from our extraction of the report.
#   4. Where both exist, the API's 2025–26 values are checked against our
#      extraction (extract/trend_crosscheck.csv). The build stops if they disagree.
#   5. Writes app/data/trends.rds.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(jsonlite)
})

# County names: the 47 counties as stored by extract_tables.R, plus the API's spellings
COUNTIES   <- readRDS("app/data/counties.rds")$county
county_key <- function(x) gsub("[^a-z]", "", tolower(x))
COUNTY_LOOKUP <- setNames(COUNTIES, county_key(COUNTIES))
COUNTY_LOOKUP[c("nairobi", "tharaka", "muranga")] <- c("Nairobi City", "Tharaka-Nithi", "Murang'a")
std_county <- function(x) unname(COUNTY_LOOKUP[county_key(x)])   # NA if not a county

API     <- "https://api.dhsprogram.com/rest/dhs/data"
SURVEYS <- c("KE1989DHS", "KE1993DHS", "KE1998DHS", "KE2003DHS", "KE2008DHS",
             "KE2014DHS", "KE2022DHS", "KE2025DHS")
YEARS   <- tibble(survey_id  = SURVEYS,
                  year       = c(1989, 1993, 1998, 2003, 2008.5, 2014, 2022, 2025.5),
                  year_label = c("1989", "1993", "1998", "2003", "2008\u201309", "2014", "2022", "2025\u201326"))
TOL     <- 0.15   # API vs report: values are published to one decimal

ind <- read_csv("extract/trend_indicators.csv", show_col_types = FALSE)

# ---- DHS Program API ---------------------------------------------------------------------
fetch <- function(breakdown) {
  q <- list(countryIds = "KE", indicatorIds = paste(ind$indicator_id, collapse = ","),
            surveyIds = paste(SURVEYS, collapse = ","), breakdown = breakdown,
            perpage = "5000", f = "json")
  url <- function(page) paste0(API, "?", paste(names(q), vapply(q, URLencode, "", reserved = TRUE),
                                                sep = "=", collapse = "&"), "&page=", page)
  first <- fromJSON(url(1))
  pages <- c(list(first$Data), lapply(seq_len(max(first$TotalPages - 1, 0)) + 1,
                                      function(p) fromJSON(url(p))$Data))
  bind_rows(pages)
}

message("Fetching national values ...")
nat_raw <- fetch("national")
message("Fetching subnational values ...")
sub_raw <- fetch("subnational")

# IsPreferred picks the standard reference period when an indicator has several
# (e.g. births in the 2 years before the survey). Counties are listed with a ".."
# prefix, except Nairobi, which is both a former province and a county.
county_surveys <- c("KE2014DHS", "KE2022DHS", "KE2025DHS")
api <- bind_rows(nat_raw, sub_raw) |>
  filter(IsPreferred == 1) |>
  transmute(
    indicator_id = IndicatorId, survey_id = SurveyId,
    level = case_when(IsTotal == 1 ~ "national",
                      grepl("^\\.\\.", CharacteristicLabel) ~ "county",
                      TRUE ~ "province"),
    area  = case_when(level == "national" ~ "Kenya",
                      level == "county"   ~ std_county(sub("^\\.\\.", "", CharacteristicLabel)),
                      TRUE ~ CharacteristicLabel),
    value = as.numeric(Value),
    n_unweighted = as.numeric(DenominatorUnweighted),
    source = "DHS Program API"
  )
has_counties <- api |> filter(level == "county") |> distinct(indicator_id, survey_id)
api <- bind_rows(
  api,
  api |> filter(level == "province", area == "Nairobi", survey_id %in% county_surveys) |>
    semi_join(has_counties, by = c("indicator_id", "survey_id")) |>
    mutate(level = "county", area = "Nairobi City")
) |>
  # Mortality rates below national level use a different (10-year) period: keep national only
  filter(!(grepl("^CM_", indicator_id) & level != "national")) |>
  distinct(indicator_id, survey_id, level, area, .keep_all = TRUE)

bad <- api |> filter(level == "county", is.na(area))
if (nrow(bad)) stop("Unmatched county names in API data: ", paste(unique(bad$area), collapse = ", "))

# ---- 2025–26 values from our extraction of the report ------------------------------------
kmdhs <- readRDS("app/data/kmdhs.rds")
report <- kmdhs |>
  filter(group %in% c("County", "Total")) |>
  mutate(kmdhs_key = paste(table_id, indicator, sep = " :: ")) |>
  inner_join(ind |> filter(!is.na(kmdhs_key)) |> select(indicator_id, kmdhs_key), by = "kmdhs_key") |>
  group_by(indicator_id, group, category) |> slice(1) |> ungroup() |>
  transmute(indicator_id, survey_id = "KE2025DHS",
            level = if_else(group == "Total", "national", "county"),
            area  = if_else(group == "Total", "Kenya", std_county(category)),
            value, n_unweighted = n,
            source = paste0("KMDHS 2025\u201326 report, Table ", sub(" ::.*", "", kmdhs_key)))

# ---- Cross-check the API's 2025–26 values against the report --------------------------------
check <- api |>
  filter(survey_id == "KE2025DHS", level %in% c("national", "county")) |>
  inner_join(report, by = c("indicator_id", "survey_id", "level", "area"), suffix = c("_api", "_report")) |>
  mutate(diff = abs(value_api - value_report), match = diff <= TOL)

summary_check <- check |>
  group_by(indicator_id) |>
  summarise(compared = n(), matching = sum(match), max_diff = max(diff), .groups = "drop")
write_csv(summary_check, "extract/trend_crosscheck.csv")
print(summary_check, n = Inf)
if (any(!check$match)) {
  print(check |> filter(!match) |> select(indicator_id, area, value_api, value_report), n = 30)
  stop("API and report values disagree; see above.")
}

# ---- Combine -----------------------------------------------------------------------------------
fill_2025 <- ind$indicator_id[ind$source_2025 == "kmdhs"]
trends <- bind_rows(
  api |> filter(!(survey_id == "KE2025DHS" & indicator_id %in% fill_2025)),
  report |> filter(indicator_id %in% fill_2025)
) |>
  filter(!is.na(value)) |>
  left_join(YEARS, by = "survey_id") |>
  left_join(ind |> select(indicator_id, label, topic, unit, better, note, kmdhs_key), by = "indicator_id") |>
  arrange(match(indicator_id, ind$indicator_id), year, level, area)

# Every indicator must have a national series, and county values must cover all 47 counties
no_nat <- setdiff(ind$indicator_id, trends$indicator_id[trends$level == "national"])
if (length(no_nat)) stop("No national series for: ", paste(no_nat, collapse = ", "))
cov <- trends |> filter(level == "county") |> count(indicator_id, survey_id) |> filter(n != 47)
if (nrow(cov)) { print(cov); stop("County coverage is not 47 for the rows above.") }

saveRDS(trends, "app/data/trends.rds")
message(sprintf("Wrote app/data/trends.rds: %d values, %d indicators, %d surveys.",
                nrow(trends), n_distinct(trends$indicator_id), n_distinct(trends$survey_id)))
