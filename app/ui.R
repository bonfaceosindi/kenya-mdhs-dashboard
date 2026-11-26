# =============================================================================
# ui.R: layout only. All data and helpers come from global.R; logic is in server.R.
# Colours follow the report: cover maroon, teal-blue headings, Kenyan flag stripe.
# =============================================================================

app_theme <- bs_theme(
  version = 5,
  bg = "#fcfaf8", fg = "#2b2222",
  primary = MAROON, info = TEAL, success = KE_GREEN, danger = KE_RED,
  base_font    = font_collection("Source Sans 3", "Segoe UI", "Roboto", "system-ui", "sans-serif"),
  heading_font = font_collection("Archivo", "Segoe UI", "system-ui", "sans-serif"),
  "border-radius" = "0.6rem",
  "font-size-base" = "1rem"
)

# %1$s ... %6$s are colour placeholders (filled below); %% is a literal percent sign
app_css_template <- ("
  :root { --kd-maroon: %1$s; --kd-maroon-dk: %2$s; --kd-teal: %3$s; --kd-sand: %4$s; }
  [data-bs-theme='dark'] { --bs-link-color: #eaa797; --bs-link-hover-color: #f6c7bb;
                           --bs-primary-rgb: 234,167,151; --bs-body-bg: #1d1717; }

  /* Navbar in report maroon with a flag stripe underneath */
  .navbar { background: var(--kd-maroon) !important; }
  .navbar .navbar-brand { font-family: Archivo, system-ui, sans-serif; font-weight: 800; letter-spacing: .01em; }
  .navbar .nav-link { color: rgba(255,255,255,.82) !important; font-weight: 500; }
  .navbar .nav-link.active, .navbar .nav-link:hover { color: #fff !important; }
  .navbar .nav-link.active { box-shadow: inset 0 -3px 0 var(--kd-sand); }
  .flag-stripe { height: 5px; background: linear-gradient(90deg,
      #111 0 30%%, #fff 30%% 32%%, %5$s 32%% 68%%, #fff 68%% 70%%, %6$s 70%% 100%%); }

  h1, h2, h3, h4, .card-header { font-family: Archivo, system-ui, sans-serif; }
  .section-title { font-size: .8rem; font-weight: 700; letter-spacing: .08em; text-transform: uppercase;
                   color: var(--kd-teal); margin: 1.75rem 0 .75rem; }
  .card { border-color: rgba(114,37,37,.12); box-shadow: 0 1px 2px rgba(60,20,20,.05); }
  .card-header { font-weight: 600; background: transparent; }

  /* Overview hero, styled after the report cover */
  .hero { background: linear-gradient(115deg, var(--kd-maroon-dk) 0%%, var(--kd-maroon) 55%%, #8c3530 100%%);
          color: #fff; border-radius: 1rem; padding: 1.5rem 1.75rem .5rem; position: relative; overflow: hidden; }
  .hero > * { position: relative; z-index: 1; }
  .hero-grid { display: grid; grid-template-columns: 1fr auto; gap: 1rem 2.5rem; align-items: end; }
  .hero-search { display: grid; grid-template-columns: 3fr 2fr; gap: 0 1.5rem; margin-top: 1rem; }
  @media (max-width: 900px) { .hero-grid, .hero-search { grid-template-columns: 1fr; } }

  /* Headline indicators: three themes side by side on wide screens, 2 x 2 cards each */
  .kpi-sections { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 0 1.5rem; }
  @media (max-width: 1200px) { .kpi-sections { grid-template-columns: 1fr; } }
  .kpi-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: .75rem; }
  @media (max-width: 1200px) { .kpi-grid { grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); } }
  .kpi { background: var(--bs-body-bg); border: 1px solid rgba(114,37,37,.14); border-top: 3px solid var(--kd-maroon);
         border-radius: .6rem; padding: .75rem .9rem; display: flex; flex-direction: column; gap: .15rem;
         box-shadow: 0 1px 2px rgba(60,20,20,.05); }
  .kpi-title { font-weight: 600; font-size: .9rem; line-height: 1.25; display: flex; gap: .45rem; align-items: flex-start; min-height: 2.3em; }
  .kpi-icon { color: var(--kd-maroon); flex: 0 0 auto; }
  [data-bs-theme='dark'] .kpi-icon { color: #eaa797; }
  .kpi-value { font-family: Archivo, sans-serif; font-weight: 700; font-size: 1.85rem; line-height: 1.1;
               font-variant-numeric: tabular-nums; }
  .kpi-note { font-size: .78rem; color: var(--bs-secondary-color); line-height: 1.3; flex: 1; }
  .kpi-link { font-size: .8rem; font-weight: 600; margin-top: .2rem; }
  .topic-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: .75rem; }
  @media (min-width: 1500px) { .topic-grid { grid-template-columns: repeat(6, minmax(0, 1fr)); } }
  .hero::after { content: ''; position: absolute; right: -60px; top: -60px; width: 260px; height: 260px;
                 border-radius: 50%%; background: rgba(232,199,181,.12); }
  .hero h1 { font-weight: 800; font-size: clamp(1.6rem, 3.2vw, 2.4rem); margin-bottom: .35rem; }
  .hero .kicker { color: var(--kd-sand); font-weight: 600; letter-spacing: .06em; text-transform: uppercase; font-size: .8rem; }
  .hero p.lead { color: rgba(255,255,255,.86); max-width: 62ch; font-size: 1.05rem; }
  .hero .form-label { color: rgba(255,255,255,.9); font-weight: 600; }
  .hero .selectize-input { border: 0; }
  .hero-stats { display: grid; grid-template-columns: repeat(4, auto); gap: .5rem 1.75rem; color: rgba(255,255,255,.85); }
  @media (max-width: 600px) { .hero-stats { grid-template-columns: repeat(2, auto); } }
  .hero-stats strong { display: block; color: #fff; font-size: 1.5rem; font-family: Archivo, sans-serif; }

  /* Headline value boxes */
  .bslib-value-box { border-top: 3px solid var(--kd-maroon) !important; }
  .bslib-value-box .value-box-showcase, .bslib-value-box .value-box-showcase .bi { color: var(--kd-maroon) !important; }
  /* bslib paints showcase icons with a blue gradient (!important); use the report maroon instead */
  html body .bslib-value-box.default .value-box-showcase > svg.bi {
    width: 2rem !important; height: 2rem !important; opacity: .85; fill: currentColor !important; }
  [data-bs-theme='dark'] .bslib-value-box .value-box-showcase,
  [data-bs-theme='dark'] .bslib-value-box .value-box-showcase .bi { color: #eaa797 !important; }
  .bslib-value-box .value-box-value { font-variant-numeric: tabular-nums; font-family: Archivo, sans-serif; font-weight: 700; }
  .bslib-value-box .value-box-title { font-weight: 600; }

  /* Topic cards */
  a.topic-link { display: block; text-decoration: none; color: inherit; height: 100%%; }
  .topic-card { display: flex; gap: .85rem; align-items: center; padding: .9rem 1rem; height: 100%%;
                border: 1px solid rgba(114,37,37,.15); border-radius: .75rem; background: var(--bs-body-bg);
                transition: border-color .15s, transform .15s, box-shadow .15s; }
  a.topic-link:hover .topic-card, a.topic-link:focus .topic-card {
                border-color: var(--kd-maroon); transform: translateY(-2px); box-shadow: 0 4px 14px rgba(114,37,37,.12); }
  .topic-icon { flex: 0 0 2.6rem; height: 2.6rem; border-radius: .6rem; display: grid; place-items: center;
                background: rgba(114,37,37,.09); color: var(--kd-maroon); font-size: 1.25rem; }
  [data-bs-theme='dark'] .topic-icon { background: rgba(234,167,151,.14); color: #eaa797; }
  .topic-name { font-weight: 600; line-height: 1.2; }
  .topic-meta { font-size: .82rem; color: var(--bs-secondary-color); }

  /* Explorer and profile */
  .crumbs { font-size: .82rem; color: var(--bs-secondary-color); font-weight: 400; }
  .crumbs .sep { margin: 0 .35rem; opacity: .6; }
  .ind-national { display: flex; flex-direction: column; margin-bottom: .25rem; }
  .ind-national strong { font-size: 2rem; font-family: Archivo, sans-serif; font-variant-numeric: tabular-nums; line-height: 1.1; color: var(--kd-maroon); }
  [data-bs-theme='dark'] .ind-national strong { color: #eaa797; }
  .universe { font-size: .85rem; color: var(--bs-secondary-color); }
  .source-note { font-size: .8rem; color: var(--bs-secondary-color); }
  .stepper { display: flex; gap: .5rem; }
  .stepper .btn { flex: 1; border-color: var(--bs-border-color); color: var(--bs-body-color); }
  .stepper .btn:hover { border-color: var(--kd-maroon); background: rgba(114,37,37,.08); }
  .leaflet-container { background: var(--bs-body-bg); }
  .info.legend { font-family: 'Source Sans 3', sans-serif; }
  .profile-band { border-left: 5px solid var(--kd-maroon); padding: .25rem 0 .25rem 1rem; margin-bottom: 1rem; }
  .profile-band h2 { margin: 0; font-weight: 800; }
  .county-code { font-size: .8rem; letter-spacing: .08em; text-transform: uppercase; color: var(--kd-teal); font-weight: 700; }
  table.dataTable td { font-variant-numeric: tabular-nums; }
  table.dataTable tbody tr { cursor: pointer; }
  /* Author footer */
  .author-footer { margin-top: 2rem; border-radius: 1rem; overflow: hidden; color: #fff;
                   background: linear-gradient(115deg, var(--kd-maroon-dk), var(--kd-maroon)); }
  .author-footer .flag-stripe { height: 4px; }
  .author-main { display: grid; grid-template-columns: minmax(220px, 1fr) 2.2fr; gap: 1.25rem 2.5rem; padding: 1.5rem 1.75rem 1.25rem; }
  @media (max-width: 768px) { .author-main { grid-template-columns: 1fr; } }
  .author-label { font-size: .72rem; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; color: var(--kd-sand); margin-bottom: .4rem; }
  .author-name { font-family: Archivo, sans-serif; font-weight: 800; font-size: 1.35rem; line-height: 1.2; }
  .author-title { color: rgba(255,255,255,.85); margin-bottom: .6rem; }
  .author-footer a { color: #fff; text-decoration: none; border-bottom: 1px solid rgba(232,199,181,.6); }
  .author-footer a:hover { border-bottom-color: #fff; }
  .chips { display: flex; flex-wrap: wrap; gap: .4rem; }
  .chip { font-size: .8rem; padding: .2rem .65rem; border-radius: 999px; background: rgba(255,255,255,.1);
          border: 1px solid rgba(232,199,181,.35); max-width: 100%%; }
  .author-source { font-size: .78rem; color: rgba(255,255,255,.7); padding: .7rem 1.75rem; display: flex; flex-wrap: wrap;
                   gap: .25rem 1.5rem; justify-content: space-between; background: rgba(0,0,0,.18); }
")
app_css <- local({
  css  <- app_css_template
  cols <- c(MAROON, MAROON_DK, TEAL, SAND, KE_RED, KE_GREEN)
  for (i in seq_along(cols)) css <- gsub(paste0("%", i, "$s"), cols[i], css, fixed = TRUE)
  gsub("%%", "%", css, fixed = TRUE)
})

# Small mark for the navbar: three figures in the flag colours, after the cover logo
brand_mark <- HTML(sprintf(
  '<svg width="26" height="24" viewBox="0 0 26 24" aria-hidden="true" style="margin-right:.5rem;vertical-align:-5px">
     <circle cx="5" cy="4" r="3" fill="#fff"/><rect x="2" y="8" width="6" height="15" rx="3" fill="#fff"/>
     <circle cx="13" cy="4" r="3" fill="%s"/><rect x="10" y="8" width="6" height="15" rx="3" fill="%s"/>
     <circle cx="21" cy="4" r="3" fill="%s"/><rect x="18" y="8" width="6" height="15" rx="3" fill="%s"/>
   </svg>', SAND, SAND, "#7fc98a", "#7fc98a"))

# ---- Overview building blocks (static, built once) ----------------------------------
overview_box <- function(i) {
  k <- key_indicators[i, ]
  div(
    class = "kpi",
    div(class = "kpi-title", span(class = "kpi-icon", bs_icon(k$icon)), k$label),
    div(class = "kpi-value", fmt_value(k$national, k$unit)),
    if (k$county_level) {
      div(class = "kpi-note",
          sprintf("Counties: %s (%s) to %s (%s)",
                  fmt_value(k$min, k$unit), k$min_county, fmt_value(k$max, k$unit), k$max_county))
    } else {
      div(class = "kpi-note", "Per 1,000 live births, 5 years before the survey. National only.")
    },
    if (k$county_level) actionLink(paste0("go_", i), tagList("Map all counties ", bs_icon("arrow-right")),
                                   class = "kpi-link")
  )
}

overview_section <- function(s) {
  idx <- which(key_indicators$section == s)
  div(class = "kpi-section",
      div(class = "section-title", s),
      div(class = "kpi-grid", lapply(idx, overview_box)))
}

topic_card <- function(i) {
  t <- topic_summary[i, ]
  actionLink(
    paste0("topic_", i), class = "topic-link",
    label = div(class = "topic-card",
                div(class = "topic-icon", bs_icon(topic_icon(t$topic))),
                div(div(class = "topic-name", t$topic),
                    div(class = "topic-meta", sprintf("%d indicators · %d tables", t$n_ind, t$n_tab)))))
}

# Footer with the author's contact and interests (details live in AUTHOR, global.R)
author_footer <- function() {
  tags$footer(
    class = "author-footer",
    div(class = "flag-stripe"),
    div(class = "author-main",
        div(div(class = "author-label", "Built by"),
            div(class = "author-name", AUTHOR$name),
            div(class = "author-title", AUTHOR$title),
            div(class = "mb-1", bs_icon("envelope"), " ",
                tags$a(href = paste0("mailto:", AUTHOR$email), AUTHOR$email)),
            div(bs_icon("github"), " ",
                tags$a(href = AUTHOR$github, target = "_blank", rel = "noopener",
                       sub("^https://", "", AUTHOR$github)))),
        div(div(class = "author-label", "Interests"),
            div(class = "chips", lapply(AUTHOR$interests, function(x) span(class = "chip", x))))),
    div(class = "author-source",
        span("Data: KNBS and ICF (2026). 2025–26 Kenya Mini Demographic and Health Survey. CC BY-NC 4.0."),
        span("Not an official KNBS, MoH or ICF product."))
  )
}

# ---- Page -----------------------------------------------------------------------------
page_navbar(
  id = "nav",
  title = tagList(brand_mark, "Kenya MDHS 2025–26"),
  window_title = "Kenya MDHS 2025–26 dashboard",
  theme = app_theme,
  navbar_options = navbar_options(theme = "dark", bg = MAROON),
  fillable = "County explorer",
  header = tagList(
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Archivo:wght@500;600;700;800&family=Source+Sans+3:wght@400;500;600;700&display=swap"),
      tags$style(HTML(app_css))
    ),
    div(class = "flag-stripe")
  ),

  # ---- Overview -------------------------------------------------------------------------
  nav_panel(
    "Overview", icon = bs_icon("grid-1x2"),
    div(
      class = "container-fluid px-3 px-lg-4 py-3",
      div(class = "hero",
          div(class = "hero-grid",
              div(
                div(class = "kicker", "Kenya Mini Demographic and Health Survey · 2025–26"),
                h1("How is Kenya's health changing, county by county?"),
                p(class = "lead mb-0",
                  "The 600-page final report, as an interactive tool. Explore ",
                  format(nrow(indicators)), " county indicators from ", n_distinct(indicators$table_id),
                  " report tables. Every number links back to its table and page.")),
              div(class = "hero-stats",
                  div(strong("47"), "counties"),
                  div(strong(format(nrow(indicators))), "county indicators"),
                  div(strong(nrow(catalog)), "report tables"),
                  div(strong(format(nrow(kmdhs), big.mark = ",")), "values checked"))),
          div(class = "hero-search",
              selectizeInput("jump_ind", "Find an indicator", choices = c("", ALL_IND_CHOICES), width = "100%",
                             options = list(placeholder = "Type, e.g. 'stunting', 'SHA', 'condom'...")),
              selectizeInput("jump_county", "Open a county profile", choices = c("", COUNTY_NAMES), width = "100%",
                             options = list(placeholder = "Type a county name")))
      ),

      div(class = "kpi-sections", lapply(SECTIONS, overview_section)),

      div(class = "section-title", "Browse by topic"),
      div(class = "topic-grid", lapply(seq_len(nrow(topic_summary)), topic_card)),
      p(class = "source-note mt-3",
        "Contraceptive use and demand satisfied refer to currently married women; ",
        "antenatal care and facility births refer to the most recent live birth in the 2 years before the survey."),
      author_footer()
    )
  ),

  # ---- County explorer --------------------------------------------------------------------
  nav_panel(
    "County explorer", icon = bs_icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        selectInput("topic", "Topic", choices = TOPICS, selected = "Family planning"),
        selectizeInput("ind", "Indicator", choices = indicator_choices("Family planning"),
                       options = list(placeholder = "Search indicators")),
        div(class = "stepper",
            actionButton("ind_prev", tagList(bs_icon("chevron-left"), "Previous"), class = "btn-sm btn-outline-secondary"),
            actionButton("ind_next", tagList("Next", bs_icon("chevron-right")), class = "btn-sm btn-outline-secondary")),
        hr(class = "my-2"),
        uiOutput("ind_info"),
        actionLink("to_table", tagList(bs_icon("table"), " View the full report table"), class = "small"),
        hr(class = "my-2"),
        selectizeInput("county", "Highlight a county",
                       choices = c("None" = "", COUNTY_NAMES),
                       options = list(placeholder = "Click the map or pick one")),
        actionButton("to_profile", tagList(bs_icon("geo-alt"), "Open county profile"),
                     class = "btn-primary btn-sm")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          full_screen = TRUE,
          card_header(uiOutput("map_title")),
          card_body(padding = 0, leafletOutput("map", height = "100%")),
          card_footer(uiOutput("source_note"))
        ),
        card(
          full_screen = TRUE,
          card_header("All 47 counties, ranked"),
          card_body(min_height = "560px", plotlyOutput("rank_plot", height = "100%"))
        )
      )
    )
  ),

  # ---- County profile -----------------------------------------------------------------------
  nav_panel(
    "County profile", icon = bs_icon("geo-alt"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("profile_county", "County", choices = COUNTY_NAMES, selected = "Nairobi City"),
        div(class = "stepper",
            actionButton("cty_prev", tagList(bs_icon("chevron-left"), "Previous"), class = "btn-sm btn-outline-secondary"),
            actionButton("cty_next", tagList("Next", bs_icon("chevron-right")), class = "btn-sm btn-outline-secondary")),
        p(class = "source-note mt-1", "Counties follow the official KNBS code order (Mombasa 001 to Nairobi City 047)."),
        selectInput("profile_topic", "Topic for the indicator table", choices = c("All topics", TOPICS)),
        p(class = "source-note",
          "Rank 1 means the highest value of the 47 counties. A high rank is not always good: ",
          "for example, teenage pregnancy or open defecation.")
      ),
      uiOutput("profile_header"),
      uiOutput("profile_tiles"),
      card(
        card_header("Headline indicators: county compared with Kenya"),
        plotlyOutput("profile_plot", height = "460px")
      ),
      card(
        card_header(tagList("All county indicators",
                            span(class = "crumbs ms-2", "Select a row to map that indicator for all counties"))),
        DTOutput("profile_table")
      ),
      author_footer()
    )
  ),

  # ---- Tables ----------------------------------------------------------------------------------
  nav_panel(
    "Tables", icon = bs_icon("table"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Report tables"),
        selectInput("tables_topic", NULL, choices = c("All topics", TOPICS), width = "100%"),
        p(class = "source-note mb-2", "Search by title or table number, then select a row."),
        DTOutput("catalog_table")
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput("table_title")),
        DTOutput("table_view"),
        card_footer(
          div(class = "d-flex flex-wrap gap-2 align-items-center justify-content-between",
              span(class = "source-note", "Values in brackets are based on 25–49 cases; * means suppressed (fewer than 25 cases)."),
              downloadButton("download_table", "Download CSV", class = "btn-sm btn-outline-primary"))
        )
      )
    ),
    author_footer()
  ),

  # ---- About -------------------------------------------------------------------------------------
  nav_panel(
    "About", icon = bs_icon("info-circle"),
    div(class = "container py-4", style = "max-width: 760px;",
        markdown(readLines("about.md", encoding = "UTF-8", warn = FALSE))),
    div(class = "container-xxl pb-3", author_footer())
  ),

  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)
