# =============================================================================
# global.R: loaded once per R session, before ui.R and server.R.
# Packages, the prebuilt data (from extract/extract_tables.R), and helpers.
# Everything here works in Shinylive (webR): no 'sf', no compiled spatial code.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)

# An explicit app-wide cache for bindCache(); Shinylive (webR) has no default one
shinyOptions(cache = cachem::cache_mem(max_size = 50 * 1024^2))

kmdhs    <- readRDS("data/kmdhs.rds")
catalog  <- read.csv("data/catalog.csv", check.names = FALSE, encoding = "UTF-8")
counties <- readRDS("data/counties.rds")          # county, lng, lat (NA-separated rings)
COUNTY_NAMES <- counties$county                   # KNBS code order

# ---- Palette, sampled from the report: cover maroon, heading teal-blue, flag colours ----
MAROON    <- "#722525"   # report cover
MAROON_DK <- "#4e1818"
TEAL      <- "#2F9BBF"   # report section headings
KE_RED    <- "#C8202A"   # flag colours on the cover mark (decoration only, never data)
KE_GREEN  <- "#2E8B3A"
SAND      <- "#E8C7B5"   # Kenya map shape on the cover

ACCENT    <- MAROON      # bars and key marks
HIGHLIGHT <- TEAL        # the selected county, a different hue from the ramp
NA_FILL   <- "#d9d4d0"
SEQ_RAMP  <- c("#f7ece5", "#ebc9b8", "#d79b83", "#b9654f", "#8c3530", "#561819")  # sand -> maroon

# ---- Author details for the footer (edit here) ---------------------------------------
AUTHOR <- list(
  name      = "Bonface Osindi",
  title     = "Data Scientist | Data Engineer",
  email     = "bonfaceosindi@gmail.com",
  github    = "https://github.com/bonfaceosindi",
  interests = c("Health Informatics", "Clinical Data Management", "Biostatistics",
                "Public Health Data Science", "One Health Surveillance", "Field Epidemiology",
                "Clinical Trials", "Machine Learning & GenAI for Health", "Data Engineering",
                "Cloud Computing", "Health Dashboards & Data Visualization",
                "Research & Publishing", "Mentoring & Data Science Education")
)

FONT_STACK <-"'Source Sans 3', 'Segoe UI', Roboto, system-ui, sans-serif"

# ---- County-level data: the panel of each table that lists the 47 counties ------
county_panels <- kmdhs |> filter(group == "County") |> distinct(table_id, panel)

county_data <- kmdhs |>
  semi_join(county_panels, by = c("table_id", "panel")) |>
  filter(group %in% c("County", "Total")) |>
  mutate(key = paste(table_id, indicator, sep = " :: "))

national <- county_data |>
  filter(group == "Total") |>
  group_by(key) |> slice(1) |> ungroup() |>
  select(key, national = value)

county_ranked <- county_data |>
  filter(group == "County") |>
  group_by(key) |>
  mutate(rank = rank(-value, ties.method = "min", na.last = "keep"),
         n_ranked = sum(!is.na(value))) |>
  ungroup() |>
  left_join(national, by = "key")

indicators <- county_data |>
  distinct(key, table_id, chapter, topic, title, page, indicator, unit) |>
  left_join(select(catalog, table_id, universe), by = "table_id") |>
  arrange(match(table_id, catalog$table_id))

TOPICS <- unique(indicators$topic)

TOPIC_ICONS <- c(
  "Housing & household population" = "house",
  "Respondents & health insurance" = "person-vcard",
  "Marriage & sexual activity"     = "people",
  "Fertility"                      = "flower2",
  "Fertility preferences"          = "chat-heart",
  "Family planning"                = "capsule",
  "Maternal & newborn health"      = "heart-pulse",
  "Child health"                   = "bandaid",
  "Child nutrition & feeding"      = "cup-straw",
  "Malaria"                        = "bug",
  "Water, sanitation & hygiene"    = "droplet",
  "Gender-based violence"          = "shield-exclamation"
)
topic_icon <- function(tp) unname(TOPIC_ICONS[tp]) %||% "folder2"

topic_summary <- indicators |>
  group_by(topic) |>
  summarise(n_ind = n(), n_tab = n_distinct(table_id), .groups = "drop") |>
  arrange(match(topic, TOPICS))

# Indicator choices for one topic, grouped by report table
indicator_choices <- function(tp) {
  d <- indicators[indicators$topic == tp, ]
  grp <- paste0(d$title, " (Table ", d$table_id, ")")
  split(setNames(d$key, d$indicator), factor(grp, levels = unique(grp)))
}

# Every county indicator, grouped by topic (for the quick search on the Overview)
ALL_IND_CHOICES <- split(
  setNames(indicators$key, paste0(indicators$indicator, " · Table ", indicators$table_id)),
  factor(indicators$topic, levels = TOPICS))

# ---- Formatting helpers --------------------------------------------------------
fmt_value <- function(x, unit) {
  unit <- rep_len(unit, length(x))   # one unit for many values must not shorten the result
  out <- ifelse(unit == "per 1,000", formatC(x, format = "f", digits = 0),
                formatC(x, format = "f", digits = 1))
  out <- ifelse(unit == "percent", paste0(out, "%"), out)
  ifelse(is.na(x), "–", out)
}

unit_label <- function(unit) {
  switch(unit,
         "percent"          = "Percent",
         "per 1,000"        = "Deaths per 1,000 live births",
         "births per woman" = "Births per woman",
         "number"           = "Value",
         unit)
}

FLAG_TEXT <- c(
  "25-49 cases"    = "Based on 25–49 unweighted cases: interpret with caution",
  "suppressed"     = "Suppressed: fewer than 25 unweighted cases",
  "not applicable" = "Not applicable",
  "not calculated" = "Not calculated",
  "less than 0.1"  = "Less than 0.1"
)
flag_text <- function(flag) ifelse(is.na(flag), "", unname(FLAG_TEXT[flag]))

# ---- Headline indicators for the Overview and County profile ---------------------
key_indicators <- tibble::tribble(
  ~key,                                             ~label,                                       ~icon,
  "5.2C :: Total fertility rate",                   "Total fertility rate",                       "people",
  "8.2 :: Under-5 mortality",                       "Under-5 mortality",                          "heart-pulse",
  "7.4.2C :: Any modern method",                    "Modern contraceptive use (married women)",   "capsule",
  "7.14.1C :: Demand satisfied by modern methods",  "Demand for FP satisfied by modern methods",  "check2-circle",
  "9.2C :: 4+ ANC visits",                          "Four or more antenatal care visits",         "clipboard2-pulse",
  "9.7C :: Delivered in a health facility",         "Births in a health facility",                "hospital",
  "5.12C :: Teen ever pregnant",                    "Teenage pregnancy (women 15–19)",            "person",
  "2.9C :: Birth registered",                       "Births registered (children under 5)",       "file-earmark-text",
  "2.11C :: Covered by SHA",                        "Population covered by SHA",                  "card-checklist",
  "12.3C :: Population with access to an ITN",      "Population with access to an ITN",           "shield-check",
  "13.2C :: At least basic drinking water service", "At least basic drinking water",              "droplet",
  "3.5C :: Used internet in past 12 months",        "Internet use in past 12 months (women)",     "wifi"
)

u5 <- kmdhs |> filter(table_id == "8.2", group == "Total", indicator == "Under-5 mortality")

key_indicators <- key_indicators |>
  mutate(county_level = key %in% indicators$key) |>
  left_join(national, by = "key") |>
  mutate(national = if_else(key == "8.2 :: Under-5 mortality", u5$value[1], national),
         unit = coalesce(indicators$unit[match(key, indicators$key)], "per 1,000")) |>
  left_join(
    county_ranked |>
      group_by(key) |>
      summarise(min = min(value, na.rm = TRUE), min_county = category[which.min(value)],
                max = max(value, na.rm = TRUE), max_county = category[which.max(value)],
                .groups = "drop"),
    by = "key")

stopifnot(!anyNA(key_indicators$national))

# ---- Trends: Kenya DHS 1989 to 2025–26 (from extract/fetch_trends.R) ----------------------
trends <- readRDS("data/trends.rds")

TREND_IND <- trends |>
  distinct(indicator_id, label, topic, unit, better, note, kmdhs_key) |>
  mutate(county_years = vapply(indicator_id, function(id)
    length(unique(trends$year_label[trends$indicator_id == id & trends$level == "county"])), 1L))
TREND_TOPICS <- unique(TREND_IND$topic)
YEAR_ORDER   <- trends |> distinct(year, year_label) |> arrange(year)

trend_choices <- function(tp) {
  d <- TREND_IND[TREND_IND$topic == tp, ]
  setNames(d$indicator_id, d$label)
}

# Change wording: percentage points for percentages, plain units otherwise
fmt_change <- function(d, unit) {
  u <- switch(unit, "percent" = " points", "births per woman" = " births per woman",
              "per 1,000" = " per 1,000", "")
  dig <- if (unit == "per 1,000") 0 else 1
  paste0(ifelse(d > 0, "+", ifelse(d < 0, "−", "±")), formatC(abs(d), format = "f", digits = dig), u)
}

# Categorical colours for compared counties (fixed order; Kenya is always the ink colour)
COMPARE_COLS <- c("#8c3530", "#2F9BBF", "#c47f17", "#2E8B3A")

# Headline indicator -> matching trend series (same definition in every survey)
KEY_TREND <- c(
  "5.2C :: Total fertility rate"                   = "FE_FRTR_W_TFR",
  "8.2 :: Under-5 mortality"                       = "CM_ECMR_C_U5M",
  "7.4.2C :: Any modern method"                    = "FP_CUSM_W_MOD",
  "7.14.1C :: Demand satisfied by modern methods"  = "FP_NADM_W_PDM",
  "9.2C :: 4+ ANC visits"                          = "RH_ANCN_W_N4P",
  "9.7C :: Delivered in a health facility"         = "RH_DELP_C_DHF",
  "2.9C :: Birth registered"                       = "CP_BREG_C_REG",
  "12.3C :: Population with access to an ITN"      = "ML_ITNA_P_ACC",
  "13.2C :: At least basic drinking water service" = "WS_SRCE_P_BAS",
  "3.5C :: Used internet in past 12 months"        = "CO_INUS_W_U12"
)
nat_2022 <- trends |> filter(level == "national", survey_id == "KE2022DHS") |>
  select(indicator_id, value_2022 = value)
key_indicators <- key_indicators |>
  mutate(trend_id = unname(KEY_TREND[key])) |>
  left_join(nat_2022, by = c("trend_id" = "indicator_id")) |>
  left_join(select(TREND_IND, indicator_id, better), by = c("trend_id" = "indicator_id"))

# Small multiples on the County profile: series with county values in at least two surveys
PROFILE_TRENDS <- c("FP_CUSM_W_MOD", "RH_DELP_C_DHF", "RH_ANCN_W_N4P", "FE_FRTR_W_TFR",
                    "FE_TNPG_W_CBR", "ML_NETC_C_ITN", "CN_NUTS_C_HA2", "CH_VACC_C_BAS")

# Overview sections
SECTIONS <- c("Fertility and family planning", "Mothers and children", "Living conditions and health cover")
key_indicators$section <- SECTIONS[c(1, 2, 1, 1, 2, 2, 1, 2, 3, 3, 3, 3)]

# Tiles at the top of a county profile
PROFILE_TILES <- c("5.2C :: Total fertility rate", "9.7C :: Delivered in a health facility",
                   "2.11C :: Covered by SHA", "13.2C :: At least basic drinking water service")
