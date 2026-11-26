# =============================================================================
# server.R: reactive logic only. Data and helpers come from global.R.
# =============================================================================

function(input, output, session) {

  # ---- Shared state ---------------------------------------------------------------------
  selected_county <- reactiveVal(NULL)                 # highlighted county (map, ranking)
  pending_ind     <- reactiveVal(NULL)                 # indicator to select after a topic change
  table_sel       <- reactiveVal(catalog$table_id[1])  # table shown in the Tables tab
  pending_tr      <- reactiveVal(NULL)                 # trend indicator to select after a topic change

  ink <- reactive({
    if (identical(input$dark_mode, "dark")) {
      list(text = "#f1e6e2", muted = "#b7a39d", grid = "#3a2e2c", bar = "#e39a88", hi = "#5cc3e3",
           up = "#5cc3e3", down = "#f08a7e", cmp = c("#e39a88", "#5cc3e3", "#e0a64a", "#6cc47a"))
    } else {
      list(text = "#2b2222", muted = "#7a6662", grid = "#eee4df", bar = ACCENT, hi = HIGHLIGHT,
           up = "#1f7f9e", down = MAROON, cmp = COMPARE_COLS)
    }
  })

  # ---- Navigation helpers (every jump between tabs goes through these) ---------------------------
  set_county <- function(cty) {
    if (is.null(cty) || !cty %in% COUNTY_NAMES) cty <- NULL
    selected_county(cty)
    updateSelectizeInput(session, "county", selected = cty %||% "")
  }

  go_indicator <- function(k, county = NULL) {
    tp <- indicators$topic[match(k, indicators$key)]
    if (identical(tp, input$topic)) {
      updateSelectizeInput(session, "ind", selected = k)
    } else {
      pending_ind(k)
      updateSelectInput(session, "topic", selected = tp)
    }
    if (!is.null(county)) set_county(county)
    nav_select("nav", "County explorer")
  }

  go_trend <- function(id) {
    tp <- TREND_IND$topic[TREND_IND$indicator_id == id]
    if (identical(tp, input$tr_topic)) {
      updateSelectInput(session, "tr_ind", selected = id)
    } else {
      pending_tr(id)
      updateSelectInput(session, "tr_topic", selected = tp)
    }
    nav_select("nav", "Trends")
  }

  go_profile <- function(cty) {
    updateSelectizeInput(session, "profile_county", selected = cty)
    nav_select("nav", "County profile")
  }

  catalog_proxy <- dataTableProxy("catalog_table")
  open_table <- function(id) {
    table_sel(id)
    if (!identical(input$tables_topic, "All topics")) {
      updateSelectInput(session, "tables_topic", selected = "All topics")   # re-render selects it
    } else {
      i <- match(id, catalog$table_id)
      selectRows(catalog_proxy, i)
      selectPage(catalog_proxy, (i - 1) %/% 12 + 1)
    }
    nav_select("nav", "Tables")
  }

  # ---- Shareable links: read the URL once, then keep it in sync ------------------------------
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (identical(q$tab, "explorer") && isTRUE(q$ind %in% indicators$key)) {
      go_indicator(q$ind, q$county)
    } else if (identical(q$tab, "profile") && isTRUE(q$county %in% COUNTY_NAMES)) {
      go_profile(q$county)
    } else if (identical(q$tab, "tables") && isTRUE(q$table %in% catalog$table_id)) {
      open_table(q$table)
    } else if (identical(q$tab, "trends") && isTRUE(q$ind %in% TREND_IND$indicator_id)) {
      go_trend(q$ind)
      cmp <- intersect(strsplit(q$counties %||% "", ",", fixed = TRUE)[[1]], COUNTY_NAMES)
      if (length(cmp)) updateSelectizeInput(session, "tr_counties", selected = head(cmp, 4))
    } else if (identical(q$tab, "about")) {
      nav_select("nav", "About")
    }
  }, once = TRUE, priority = 10)

  observe({
    req(input$nav)
    q <- switch(input$nav,
                "County explorer" = c(tab = "explorer", ind = input$ind, county = selected_county()),
                "Trends"          = c(tab = "trends", ind = input$tr_ind,
                                      counties = if (length(input$tr_counties)) paste(input$tr_counties, collapse = ",")),
                "County profile"  = c(tab = "profile", county = input$profile_county),
                "Tables"          = c(tab = "tables", table = table_sel()),
                "About"           = c(tab = "about"),
                character())
    qs <- if (length(q)) paste0("?", paste(names(q), vapply(q, URLencode, "", reserved = TRUE), sep = "=", collapse = "&")) else "?"
    updateQueryString(qs, mode = "replace")
  })

  # ---- Overview: cards, topic tiles and quick search -------------------------------------------
  lapply(which(key_indicators$county_level), function(i) {
    observeEvent(input[[paste0("go_", i)]], go_indicator(key_indicators$key[i]))
  })

  lapply(seq_len(nrow(topic_summary)), function(i) {
    observeEvent(input[[paste0("topic_", i)]], {
      tp <- topic_summary$topic[i]
      if (!identical(tp, input$topic)) updateSelectInput(session, "topic", selected = tp)
      nav_select("nav", "County explorer")
    })
  })

  observeEvent(input$jump_ind, {
    req(nzchar(input$jump_ind))
    go_indicator(input$jump_ind)
    updateSelectizeInput(session, "jump_ind", selected = "")
  }, ignoreInit = TRUE)

  observeEvent(input$jump_county, {
    req(nzchar(input$jump_county))
    go_profile(input$jump_county)
    updateSelectizeInput(session, "jump_county", selected = "")
  }, ignoreInit = TRUE)

  # ---- Explorer: topic -> indicators, stepping ------------------------------------------------
  observeEvent(input$topic, {
    ch  <- indicator_choices(input$topic)
    sel <- pending_ind()
    if (is.null(sel) || !sel %in% unlist(ch)) sel <- unlist(ch)[[1]]
    updateSelectizeInput(session, "ind", choices = ch, selected = sel)
    pending_ind(NULL)
  }, ignoreInit = TRUE)

  step_indicator <- function(by) {
    keys <- unname(unlist(indicator_choices(input$topic)))
    i <- match(input$ind, keys)
    if (is.na(i)) i <- 1L else i <- (i - 1L + by) %% length(keys) + 1L
    updateSelectizeInput(session, "ind", selected = keys[i])
  }
  observeEvent(input$ind_prev, step_indicator(-1L))
  observeEvent(input$ind_next, step_indicator(1L))

  observeEvent(input$to_table, open_table(meta()$table_id))

  # Link to the multi-year series when this indicator has one
  trend_for <- reactive(TREND_IND$indicator_id[match(meta()$key, TREND_IND$kmdhs_key)])
  output$to_trend_link <- renderUI({
    if (is.na(trend_for())) return(NULL)
    div(actionLink("to_trend", tagList(bs_icon("graph-up"), " See how this has changed over time"),
                   class = "small"))
  })
  observeEvent(input$to_trend, go_trend(trend_for()))
  observeEvent(input$to_profile, go_profile(selected_county() %||% input$profile_county))

  # ---- Current indicator ---------------------------------------------------------------------
  meta    <- reactive({ req(input$ind %in% indicators$key); indicators[match(input$ind, indicators$key), ] })
  current <- reactive(county_data |> filter(key == meta()$key))
  by_cty  <- reactive(current() |> filter(group == "County"))
  nat     <- reactive(current() |> filter(group == "Total") |> slice(1))

  output$map_title <- renderUI({
    m <- meta()
    div(div(class = "crumbs", m$topic, span(class = "sep", "›"), paste("Table", m$table_id),
            span(class = "sep", "›"), m$title),
        div(m$indicator))
  })

  output$ind_info <- renderUI({
    m <- meta(); n <- nat(); d <- by_cty()
    ok <- !is.na(d$value)
    tagList(
      div(class = "ind-national",
          span(class = "small text-body-secondary", "Kenya"),
          strong(if (nrow(n)) fmt_value(n$value, m$unit) else "–")),
      if (any(ok)) p(class = "small mb-1",
                     sprintf("Highest: %s (%s). Lowest: %s (%s).",
                             d$category[ok][which.max(d$value[ok])], fmt_value(max(d$value[ok]), m$unit),
                             d$category[ok][which.min(d$value[ok])], fmt_value(min(d$value[ok]), m$unit))),
      p(class = "universe mb-1", m$universe)
    )
  })

  output$source_note <- renderUI({
    m <- meta()
    span(class = "source-note",
         sprintf("Table %s, report page %s. Grey counties have no estimate; faded bars are based on 25–49 cases.",
                 m$table_id, m$page))
  })

  # ---- Map -----------------------------------------------------------------------------------------
  draw_counties <- function(map, d, m) {
    v  <- d$value[match(COUNTY_NAMES, d$category)]
    fl <- d$flag[match(COUNTY_NAMES, d$category)]
    pal <- if (all(is.na(v))) function(x) rep(NA_FILL, length(x)) else
      colorNumeric(SEQ_RAMP, domain = range(v, na.rm = TRUE), na.color = NA_FILL)
    labels <- sprintf("<strong>%s</strong><br>%s%s", COUNTY_NAMES, fmt_value(v, m$unit),
                      ifelse(is.na(fl), "", paste0("<br><em>", flag_text(fl), "</em>")))
    for (i in seq_along(COUNTY_NAMES)) {
      map <- addPolygons(
        map, lng = counties$lng[[i]], lat = counties$lat[[i]], layerId = COUNTY_NAMES[i],
        fillColor = pal(v[i]), fillOpacity = 0.95, color = "#ffffff", weight = 1, opacity = 1,
        label = HTML(labels[i]),
        # No clipping: the map is often drawn while its tab is hidden (zero size)
        options = pathOptions(noClip = TRUE),
        highlightOptions = highlightOptions(weight = 2.5, color = "#2b2222", bringToFront = TRUE)
      )
    }
    map <- clearControls(map)
    if (!all(is.na(v))) {
      map <- addLegend(map, "bottomleft", pal = pal, values = v, opacity = 0.95,
                       title = unit_label(m$unit),
                       labFormat = labelFormat(suffix = if (m$unit == "percent") "%" else ""))
    }
    map
  }

  draw_highlight <- function(map, county) {
    map <- removeShape(map, "highlight")
    if (length(county) && county %in% COUNTY_NAMES) {
      i <- match(county, COUNTY_NAMES)
      map <- addPolygons(map, lng = counties$lng[[i]], lat = counties$lat[[i]], layerId = "highlight",
                         fill = FALSE, color = HIGHLIGHT, weight = 4, opacity = 1,
                         options = pathOptions(interactive = FALSE, noClip = TRUE))
    }
    map
  }

  output$map <- renderLeaflet({
    base_map() |>
      draw_counties(isolate(by_cty()), isolate(meta())) |>
      draw_highlight(isolate(selected_county()))
  })

  # Blank Kenya map shared by the explorer and the Trends tab
  base_map <- function() {
    leaflet(options = leafletOptions(attributionControl = FALSE, zoomSnap = 0.25)) |>
      fitBounds(33.9, -4.7, 41.9, 5.0) |>
      # The map is often created inside a hidden tab; fit Kenya once it has a real size
      htmlwidgets::onRender("
        function(el, x) {
          var map = this, timer = null;
          var fit = function() {
            if (el.clientWidth < 50 || el.clientHeight < 50) return;
            map.invalidateSize(); map.fitBounds([[-4.7, 33.9], [5.0, 41.9]], { animate: false });
            map.fire('moveend');   // makes the SVG layer resize even when the zoom did not change
          };
          // Refit after the container settles (tab shown, sidebar toggled, full screen)
          new ResizeObserver(function() { clearTimeout(timer); timer = setTimeout(fit, 120); }).observe(el);
          setTimeout(fit, 300);
        }")
  }

  # Recolour with a proxy instead of re-rendering the whole map
  observeEvent(meta(), {
    leafletProxy("map") |> clearShapes() |> draw_counties(by_cty(), meta()) |>
      draw_highlight(selected_county())
  }, ignoreInit = TRUE)

  observeEvent(selected_county(), {
    leafletProxy("map") |> draw_highlight(selected_county())
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$map_shape_click, set_county(input$map_shape_click$id))
  observeEvent(input$county, {
    cty <- if (nzchar(input$county)) input$county else NULL
    if (!identical(cty, selected_county())) selected_county(cty)
  }, ignoreInit = TRUE)

  # ---- Ranked bars ---------------------------------------------------------------------------------
  output$rank_plot <- renderPlotly({
    # Counties without an estimate stay on the axis (at zero, no visible bar)
    d <- by_cty() |> arrange(!is.na(value), value) |> mutate(x = coalesce(value, 0))
    m <- meta(); n <- nat(); k <- ink()
    sel <- selected_county()
    hover <- paste0("<b>", d$category, "</b><br>",
                    ifelse(is.na(d$value), "No estimate", fmt_value(d$value, m$unit)),
                    ifelse(is.na(d$n), "", paste0("<br>n = ", format(d$n, big.mark = ","))),
                    ifelse(is.na(d$flag), "", paste0("<br><i>", flag_text(d$flag), "</i>")))
    shapes <- list(); notes <- list()
    if (nrow(n) && !is.na(n$value)) {
      shapes <- list(list(type = "line", x0 = n$value, x1 = n$value, yref = "paper", y0 = 0, y1 = 1,
                          line = list(color = k$text, dash = "dot", width = 1.5)))
      notes <- list(list(x = n$value, y = 1, yref = "paper", yanchor = "bottom", xanchor = "left",
                         text = paste("Kenya", fmt_value(n$value, m$unit)), showarrow = FALSE,
                         font = list(color = k$text, size = 12)))
    }
    p <- plot_ly(
      d, x = ~x, y = ~factor(category, levels = category), type = "bar", orientation = "h",
      marker = list(color = ifelse(d$category %in% sel, k$hi, k$bar),
                    opacity = ifelse(is.na(d$flag), 1, 0.45)),
      hovertext = hover, hoverinfo = "text", customdata = ~category, source = "rank"
    ) |>
      layout(
        xaxis = list(title = list(text = unit_label(m$unit), font = list(size = 12)), color = k$muted,
                     gridcolor = k$grid, zeroline = FALSE, fixedrange = TRUE),
        yaxis = list(title = "", color = k$text, tickfont = list(size = 11), fixedrange = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 110, r = 16, t = 28, b = 40), bargap = 0.3,
        font = list(family = FONT_STACK),
        hoverlabel = list(font = list(family = FONT_STACK)),
        shapes = shapes, annotations = notes
      ) |>
      config(displayModeBar = FALSE)
    event_register(p, "plotly_click")
  }) |>
    bindCache("rank_plot", input$ind, selected_county() %||% "", input$dark_mode)

  # Listen only on the explorer tab: plotly warns if this runs before the chart is first drawn
  rank_click <- reactive({
    req(identical(input$nav, "County explorer"))
    suppressWarnings(event_data("plotly_click", source = "rank"))
  })
  observeEvent(rank_click(), set_county(rank_click()$customdata[[1]]))

  # ---- County profile ----------------------------------------------------------------------------
  step_county <- function(by) {
    i <- match(input$profile_county, COUNTY_NAMES)
    updateSelectizeInput(session, "profile_county",
                         selected = COUNTY_NAMES[(i - 1L + by) %% length(COUNTY_NAMES) + 1L])
  }
  observeEvent(input$cty_prev, step_county(-1L))
  observeEvent(input$cty_next, step_county(1L))

  profile_rows <- reactive({
    req(input$profile_county %in% COUNTY_NAMES)
    county_ranked |> filter(category == input$profile_county) |>
      left_join(select(indicators, key, universe), by = "key")
  })

  output$profile_header <- renderUI({
    d <- profile_rows()
    nw <- d |> filter(key == "3.1C :: Share of all women")
    div(class = "profile-band",
        div(class = "county-code", sprintf("County %03d of 47", match(input$profile_county, COUNTY_NAMES))),
        h2(input$profile_county),
        p(class = "text-body-secondary mb-0",
          sprintf("%s county-level indicators from %s report tables.",
                  format(sum(!is.na(d$value)), big.mark = ","), n_distinct(d$table_id)),
          if (nrow(nw)) sprintf(" %s of Kenya's women age 15–49 live here.", fmt_value(nw$value, "percent"))))
  })

  output$profile_tiles <- renderUI({
    d <- profile_rows()
    tiles <- lapply(PROFILE_TILES, function(k) {
      r <- d[d$key == k, ]
      lab <- key_indicators$label[match(k, key_indicators$key)]
      if (!nrow(r)) return(NULL)
      value_box(
        title = lab,
        value = fmt_value(r$value, r$unit),
        p(class = "small mb-0",
          sprintf("Kenya %s · rank %s", fmt_value(r$national, r$unit),
                  if (is.na(r$rank)) "–" else paste(r$rank, "of", r$n_ranked)))
      )
    })
    do.call(layout_column_wrap, c(list(width = "200px", class = "mb-3"), tiles))
  })

  output$profile_plot <- renderPlotly({
    k <- ink()
    d <- key_indicators |>
      filter(county_level, unit == "percent") |>
      select(key, label) |>
      inner_join(profile_rows() |> select(key, value, national, flag), by = "key") |>
      mutate(label = factor(label, levels = rev(label)),
             hover_c = paste0("<b>", input$profile_county, "</b>: ", fmt_value(value, "percent"),
                              ifelse(is.na(flag), "", paste0("<br><i>", flag_text(flag), "</i>"))),
             hover_k = paste0("<b>Kenya</b>: ", fmt_value(national, "percent")))
    plot_ly(d) |>
      add_segments(x = ~national, xend = ~value, y = ~label, yend = ~label,
                   line = list(color = k$grid, width = 4), hoverinfo = "none", showlegend = FALSE) |>
      add_markers(x = ~national, y = ~label, name = "Kenya", text = ~hover_k, hoverinfo = "text",
                  marker = list(color = k$muted, size = 10)) |>
      add_markers(x = ~value, y = ~label, name = input$profile_county, text = ~hover_c, hoverinfo = "text",
                  marker = list(color = k$bar, size = 13, line = list(color = "#ffffff", width = 1.5),
                                opacity = ifelse(is.na(d$flag), 1, 0.5))) |>
      layout(
        xaxis = list(title = "Percent", range = c(0, 100), color = k$muted, gridcolor = k$grid,
                     zeroline = FALSE, fixedrange = TRUE),
        yaxis = list(title = "", color = k$text, fixedrange = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.08, font = list(color = k$text)),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 10, r = 16, t = 30, b = 40),
        font = list(family = FONT_STACK)
      ) |>
      config(displayModeBar = FALSE)
  }) |>
    bindCache("profile_plot", input$profile_county, input$dark_mode)

  profile_table_data <- reactive({
    d <- profile_rows()
    if (input$profile_topic != "All topics") d <- filter(d, topic == input$profile_topic)
    arrange(d, match(table_id, catalog$table_id))
  })

  output$profile_table <- renderDT({
    d <- profile_table_data() |>
      transmute(Topic = topic, Table = table_id, Indicator = indicator,
                County = fmt_value(value, unit), Kenya = fmt_value(national, unit),
                Rank = ifelse(is.na(rank), "–", paste0(rank, " of ", n_ranked)),
                Note = flag_text(flag))
    datatable(d, rownames = FALSE, selection = "single",
              options = list(pageLength = 15, dom = "ftip", scrollX = TRUE,
                             columnDefs = list(list(className = "dt-right", targets = 3:5))))
  })

  observeEvent(input$profile_table_rows_selected, {
    k <- profile_table_data()$key[input$profile_table_rows_selected[1]]
    selectRows(dataTableProxy("profile_table"), NULL)
    go_indicator(k, input$profile_county)
  })

  # ---- Tables ----------------------------------------------------------------------------------------
  catalog_view <- reactive({
    if (identical(input$tables_topic, "All topics")) catalog else catalog[catalog$topic == input$tables_topic, ]
  })

  # ---- Trends --------------------------------------------------------------------------------------------
  observeEvent(input$tr_topic, {
    ch  <- trend_choices(input$tr_topic)
    sel <- pending_tr()
    if (is.null(sel) || !sel %in% ch) sel <- ch[[1]]
    updateSelectInput(session, "tr_ind", choices = ch, selected = sel)
    pending_tr(NULL)
  }, ignoreInit = TRUE)

  tr_meta   <- reactive({ req(input$tr_ind %in% TREND_IND$indicator_id)
                          TREND_IND[TREND_IND$indicator_id == input$tr_ind, ] })
  tr_data   <- reactive(trends |> filter(indicator_id == tr_meta()$indicator_id))
  tr_nat    <- reactive(tr_data() |> filter(level == "national") |> arrange(year))
  tr_cty    <- reactive(tr_data() |> filter(level == "county"))
  tr_cyears <- reactive(YEAR_ORDER$year_label[YEAR_ORDER$year_label %in% tr_cty()$year_label])

  # Map view ("y:<year>" or "c:<from>|<to>"), checked against this indicator's county years
  # (the radio input can still hold the previous indicator's choice for a moment)
  tr_view <- reactive({
    v <- input$tr_view %||% ""; yrs <- tr_cyears()
    yrs_in <- strsplit(sub("^[yc]:", "", v), "|", fixed = TRUE)[[1]]
    if (grepl("^[yc]:", v) && length(yrs_in) && all(yrs_in %in% yrs)) v
    else if (length(yrs)) paste0("y:", yrs[length(yrs)]) else ""
  })

  # The two surveys being compared: from the map selector, else first and last county years
  tr_pair <- reactive({
    v <- tr_view()
    yrs <- tr_cyears()
    if (startsWith(v, "c:")) strsplit(sub("^c:", "", v), "|", fixed = TRUE)[[1]]
    else if (length(yrs) >= 2) c(yrs[1], yrs[length(yrs)])
    else NULL
  })

  output$tr_title <- renderUI({
    m <- tr_meta(); n <- tr_nat()
    div(div(class = "crumbs", m$topic, span(class = "sep", "›"),
            sprintf("Kenya, %s to %s", n$year_label[1], n$year_label[nrow(n)])),
        div(m$label))
  })

  output$tr_info <- renderUI({
    m <- tr_meta(); n <- tr_nat()
    first <- n[1, ]; last <- n[nrow(n), ]
    d <- last$value - first$value
    tagList(
      div(class = "ind-national",
          span(class = "small text-body-secondary", paste0("Kenya, ", last$year_label)),
          strong(fmt_value(last$value, m$unit))),
      if (nrow(n) > 1) p(class = "small mb-1",
                         sprintf("%s from %s in %s (%s).", if (d >= 0) "Up" else "Down",
                                 fmt_value(first$value, m$unit), first$year_label, fmt_change(d, m$unit))),
      p(class = "universe mb-1",
        if (m$better == "higher") "Higher is better." else "Lower is better.",
        if (!is.na(m$note)) paste0(" ", m$note, "."))
    )
  })

  output$tr_line <- renderPlotly({
    m <- tr_meta(); k <- ink(); n <- tr_nat()
    sel <- input$tr_counties
    cd  <- tr_cty() |> filter(area %in% sel)
    yrs <- YEAR_ORDER |> filter(year_label %in% tr_data()$year_label)
    p <- plot_ly() |>
      add_trace(data = n, x = ~year, y = ~value, type = "scatter", mode = "lines+markers", name = "Kenya",
                line = list(color = k$text, width = 3), marker = list(color = k$text, size = 9),
                text = ~paste0("<b>Kenya, ", year_label, "</b><br>", fmt_value(value, m$unit)),
                hoverinfo = "text")
    ends <- n |> slice_tail(n = 1) |> mutate(lab = "Kenya")
    for (i in seq_along(sel)) {
      d <- cd |> filter(area == sel[i]) |> arrange(year)
      if (!nrow(d)) next
      p <- p |> add_trace(data = d, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                          name = sel[i], line = list(color = k$cmp[i], width = 2.5),
                          marker = list(color = k$cmp[i], size = 8),
                          text = ~paste0("<b>", area, ", ", year_label, "</b><br>", fmt_value(value, m$unit)),
                          hoverinfo = "text")
      ends <- bind_rows(ends, d |> slice_tail(n = 1) |> mutate(lab = sel[i]))
    }
    notes <- lapply(seq_len(nrow(ends)), function(j)
      list(x = ends$year[j], y = ends$value[j], text = paste0("  ", ends$lab[j]), xanchor = "left",
           showarrow = FALSE, font = list(color = k$text, size = 12)))
    p |>
      layout(
        xaxis = list(title = "", tickmode = "array", tickvals = yrs$year, ticktext = yrs$year_label,
                     range = c(min(yrs$year) - 1, max(yrs$year) + 4), color = k$muted,
                     gridcolor = k$grid, zeroline = FALSE, fixedrange = TRUE),
        yaxis = list(title = list(text = unit_label(m$unit), font = list(size = 12)), rangemode = "tozero",
                     color = k$muted, gridcolor = k$grid, zeroline = FALSE, fixedrange = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.12, font = list(color = k$text)),
        annotations = notes,
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 50, r = 10, t = 30, b = 30), font = list(family = FONT_STACK),
        hoverlabel = list(font = list(family = FONT_STACK))
      ) |>
      config(displayModeBar = FALSE)
  }) |>
    bindCache("tr_line", input$tr_ind, paste(input$tr_counties, collapse = "|"), input$dark_mode)

  output$tr_county_section <- renderUI({
    yrs <- tr_cyears()
    if (!length(yrs)) {
      return(card(card_body(p(class = "mb-0 text-body-secondary", bs_icon("info-circle"),
        " The surveys report this indicator for Kenya as a whole only, so there is no county map."))))
    }
    views <- setNames(paste0("y:", yrs), yrs)
    if (length(yrs) >= 2) {
      pairs <- list(c(yrs[1], yrs[length(yrs)]))
      if (length(yrs) == 3) pairs <- c(pairs, list(yrs[2:3]))
      views <- c(views, setNames(vapply(pairs, function(p) paste0("c:", p[1], "|", p[2]), ""),
                                 vapply(pairs, function(p) paste0("Change ", p[1], " → ", p[2]), "")))
    }
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header(div(class = "d-flex flex-wrap gap-2 align-items-center justify-content-between",
                        span("County map"),
                        radioButtons("tr_view", NULL, choices = views, inline = TRUE,
                                     selected = paste0("y:", yrs[length(yrs)])))),
        card_body(padding = 0, leafletOutput("tr_map", height = "540px")),
        card_footer(span(class = "source-note",
                         "Click a county to add it to the comparison. Change shades are relative to the largest change on the map."))
      ),
      if (length(yrs) >= 2) {
        card(
          full_screen = TRUE,
          card_header(uiOutput("tr_change_title")),
          card_body(plotlyOutput("tr_change", height = "560px")),
          card_footer(span(class = "source-note",
                           "Hollow dot: earlier survey. Filled dot: later survey, blue if better, maroon if worse. ",
                           "Best improvement at the top."))
        )
      } else {
        card(card_body(p(class = "text-body-secondary mb-0", bs_icon("info-circle"),
          sprintf(" County values exist for %s only, so change by county cannot be shown. The national trend is above.", yrs))))
      }
    )
  })

  draw_compare <- function(map, sel) {
    for (i in 1:4) map <- removeShape(map, paste0("cmp_", i))
    for (i in seq_along(sel)) {
      j <- match(sel[i], COUNTY_NAMES)
      map <- addPolygons(map, lng = counties$lng[[j]], lat = counties$lat[[j]], layerId = paste0("cmp_", i),
                         fill = FALSE, color = ink()$cmp[i], weight = 4, opacity = 1,
                         options = pathOptions(interactive = FALSE, noClip = TRUE))
    }
    map
  }

  output$tr_map <- renderLeaflet({
    v <- tr_view()
    req(nzchar(v))
    m <- tr_meta(); d <- tr_cty()
    if (startsWith(v, "y:")) {
      yr  <- sub("^y:", "", v)
      dd  <- d[d$year_label == yr, ]
      val <- dd$value[match(COUNTY_NAMES, dd$area)]
      pal <- colorNumeric(SEQ_RAMP, domain = range(val, na.rm = TRUE), na.color = NA_FILL)
      fill <- pal(val)
      lab <- sprintf("<strong>%s, %s</strong><br>%s", COUNTY_NAMES, yr, fmt_value(val, m$unit))
      add_legend <- function(map) addLegend(map, "bottomleft", pal = pal, values = val, opacity = 0.95,
                                            title = paste(unit_label(m$unit), yr),
                                            labFormat = labelFormat(suffix = if (m$unit == "percent") "%" else ""))
    } else {
      pr <- strsplit(sub("^c:", "", v), "|", fixed = TRUE)[[1]]
      a  <- d[d$year_label == pr[1], ]; b <- d[d$year_label == pr[2], ]
      va <- a$value[match(COUNTY_NAMES, a$area)]; vb <- b$value[match(COUNTY_NAMES, b$area)]
      ch <- vb - va
      imp <- if (m$better == "higher") ch else -ch
      t <- suppressWarnings(max(abs(imp), na.rm = TRUE)); if (!is.finite(t) || t == 0) t <- 1
      cols <- c(MAROON, "#d39a8f", "#e6e1de", "#8fcbe0", "#1f7f9e")   # worse ... better, grey midpoint
      bin  <- cut(imp, c(-Inf, -t / 2, -t / 6, t / 6, t / 2, Inf), labels = FALSE)
      fill <- ifelse(is.na(bin), NA_FILL, cols[bin])
      lab <- sprintf("<strong>%s</strong><br>%s: %s<br>%s: %s<br>Change: %s", COUNTY_NAMES,
                     pr[1], fmt_value(va, m$unit), pr[2], fmt_value(vb, m$unit),
                     ifelse(is.na(ch), "–", fmt_change(ch, m$unit)))
      add_legend <- function(map) addLegend(map, "bottomleft", colors = rev(cols), opacity = 0.95,
                                            labels = c("Much better", "Better", "About the same", "Worse", "Much worse"),
                                            title = paste0("Change, ", pr[1], " to ", pr[2]))
    }
    map <- base_map()
    for (i in seq_along(COUNTY_NAMES)) {
      map <- addPolygons(map, lng = counties$lng[[i]], lat = counties$lat[[i]], layerId = COUNTY_NAMES[i],
                         fillColor = fill[i], fillOpacity = 0.95, color = "#ffffff", weight = 1, opacity = 1,
                         label = HTML(lab[i]), options = pathOptions(noClip = TRUE),
                         highlightOptions = highlightOptions(weight = 2.5, color = "#2b2222", bringToFront = TRUE))
    }
    map |> add_legend() |> draw_compare(isolate(input$tr_counties))
  })

  observeEvent(input$tr_counties, {
    leafletProxy("tr_map") |> draw_compare(input$tr_counties)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$tr_map_shape_click, {
    id  <- input$tr_map_shape_click$id
    req(id %in% COUNTY_NAMES)
    cur <- input$tr_counties
    new <- if (id %in% cur) setdiff(cur, id) else tail(c(cur, id), 4)
    updateSelectizeInput(session, "tr_counties", selected = new)
  })

  output$tr_change_title <- renderUI({
    pr <- tr_pair(); req(length(pr) == 2)
    m <- tr_meta(); n <- tr_nat()
    a <- n$value[n$year_label == pr[1]]; b <- n$value[n$year_label == pr[2]]
    div(if (length(a) && length(b))
          div(class = "crumbs", sprintf("Kenya: %s → %s (%s)", fmt_value(a, m$unit), fmt_value(b, m$unit),
                                        fmt_change(b - a, m$unit))),
        div(sprintf("Change by county, %s to %s", pr[1], pr[2])))
  })

  output$tr_change <- renderPlotly({
    pr <- tr_pair(); req(length(pr) == 2)
    m <- tr_meta(); k <- ink(); sel <- input$tr_counties
    d <- tr_cty() |> filter(year_label %in% pr) |> select(area, year_label, value) |>
      pivot_wider(names_from = year_label, values_from = value)
    d <- tibble(area = d$area, a = d[[pr[1]]], b = d[[pr[2]]]) |>
      filter(!is.na(a), !is.na(b)) |>
      mutate(ch = b - a, imp = if (m$better == "higher") ch else -ch) |>
      arrange(imp)
    col   <- ifelse(d$imp > 0, k$up, ifelse(d$imp < 0, k$down, k$muted))
    ticks <- ifelse(d$area %in% sel, paste0("<b>", d$area, "</b>"), d$area)
    hover <- sprintf("<b>%s</b><br>%s: %s<br>%s: %s<br>Change: %s", d$area, pr[1], fmt_value(d$a, m$unit),
                     pr[2], fmt_value(d$b, m$unit), fmt_change(d$ch, m$unit))
    plot_ly(d) |>
      add_segments(x = ~a, xend = ~b, y = ~area, yend = ~area, line = list(color = k$grid, width = 3),
                   hoverinfo = "none", showlegend = FALSE) |>
      add_markers(x = ~a, y = ~area, text = hover, hoverinfo = "text", showlegend = FALSE,
                  marker = list(color = "rgba(0,0,0,0)", size = 8, line = list(color = k$muted, width = 1.5))) |>
      add_markers(x = ~b, y = ~area, text = hover, hoverinfo = "text", showlegend = FALSE,
                  marker = list(color = col, size = 9)) |>
      layout(
        xaxis = list(title = list(text = unit_label(m$unit), font = list(size = 12)), color = k$muted,
                     gridcolor = k$grid, zeroline = FALSE, fixedrange = TRUE),
        yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = d$area,
                     tickmode = "array", tickvals = d$area, ticktext = ticks, color = k$text,
                     tickfont = list(size = 10), fixedrange = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 110, r = 16, t = 10, b = 40), font = list(family = FONT_STACK),
        hoverlabel = list(font = list(family = FONT_STACK))
      ) |>
      config(displayModeBar = FALSE)
  }) |>
    bindCache("tr_change", input$tr_ind, paste(tr_pair(), collapse = "|"), paste(input$tr_counties, collapse = "|"),
              input$dark_mode)

  output$tr_table <- renderDT({
    m <- tr_meta()
    w <- tr_data() |>
      filter(level %in% c("national", "county")) |>
      mutate(v = fmt_value(value, m$unit)) |>
      select(area, year_label, v) |>
      pivot_wider(names_from = year_label, values_from = v)
    w <- w[order(match(w$area, c("Kenya", COUNTY_NAMES))),
           c("area", intersect(YEAR_ORDER$year_label, names(w)))]
    names(w)[1] <- "Area"
    datatable(w, rownames = FALSE, selection = "none",
              options = list(pageLength = 12, dom = "ftip", scrollX = TRUE,
                             columnDefs = list(list(className = "dt-right", targets = seq_len(ncol(w) - 1)))))
  })

  output$tr_download <- downloadHandler(
    filename = function() paste0("kenya_dhs_trend_", tr_meta()$indicator_id, ".csv"),
    content  = function(file) {
      write.csv(tr_data() |> select(indicator_id, label, unit, survey_id, year_label, level, area,
                                    value, n_unweighted, source),
                file, row.names = FALSE, na = "")
    }
  )

  # County profile: how the county has changed (small multiples)
  lapply(seq_along(PROFILE_TRENDS), function(i) {
    output[[paste0("pt_", i)]] <- renderPlotly({
      req(input$profile_county %in% COUNTY_NAMES)
      id <- PROFILE_TRENDS[i]; k <- ink()
      m  <- TREND_IND[TREND_IND$indicator_id == id, ]
      d  <- trends |> filter(indicator_id == id, year >= 2014,
                             level == "national" | (level == "county" & area == input$profile_county))
      kn <- d |> filter(level == "national") |> arrange(year)
      cd <- d |> filter(level == "county") |> arrange(year)
      yrs <- YEAR_ORDER |> filter(year >= 2014)
      plot_ly() |>
        add_trace(data = kn, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                  line = list(color = k$muted, width = 2), marker = list(color = k$muted, size = 6),
                  text = ~paste0("Kenya, ", year_label, ": ", fmt_value(value, m$unit)), hoverinfo = "text") |>
        add_trace(data = cd, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                  line = list(color = k$bar, width = 2.5), marker = list(color = k$bar, size = 7),
                  text = ~paste0(area, ", ", year_label, ": ", fmt_value(value, m$unit)), hoverinfo = "text") |>
        layout(
          showlegend = FALSE,
          xaxis = list(title = "", tickmode = "array", tickvals = yrs$year, ticktext = yrs$year_label,
                       range = c(2012.5, 2027),
                       color = k$muted, gridcolor = k$grid, zeroline = FALSE, fixedrange = TRUE,
                       tickfont = list(size = 10)),
          yaxis = list(title = "", rangemode = "tozero", color = k$muted, gridcolor = k$grid, zeroline = FALSE,
                       fixedrange = TRUE, tickfont = list(size = 10),
                       ticksuffix = if (m$unit == "percent") "%" else ""),
          margin = list(l = 40, r = 8, t = 6, b = 24),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(family = FONT_STACK)
        ) |>
        config(displayModeBar = FALSE)
    }) |>
      # The indicator must be in the key: these 8 outputs share code, so they share cache entries
      bindCache("profile_trend", PROFILE_TRENDS[i], input$profile_county, input$dark_mode)
  })


  # ---- Tables: catalog and table view (catalog_view is defined above) ----------------------------
  output$catalog_table <- renderDT({
    v <- catalog_view()
    sel <- match(isolate(table_sel()), v$table_id)
    datatable(
      v |> transmute(Table = table_id, Title = title, Page = page),
      rownames = FALSE,
      selection = list(mode = "single", selected = if (is.na(sel)) NULL else sel),
      options = list(pageLength = 12, dom = "ftip", scrollX = TRUE,
                     displayStart = if (is.na(sel)) 0 else (sel - 1) %/% 12 * 12)
    )
  })

  observeEvent(input$catalog_table_rows_selected, {
    table_sel(catalog_view()$table_id[input$catalog_table_rows_selected[1]])
  })

  table_rows <- reactive(kmdhs |> filter(table_id == !!table_sel()))

  output$table_title <- renderUI({
    r <- catalog[catalog$table_id == table_sel(), ]
    div(div(class = "crumbs", r$topic, span(class = "sep", "›"), paste("Report page", r$page)),
        div(strong(paste0("Table ", r$table_id, ": ", r$title))),
        div(class = "source-note fw-normal", r$universe))
  })

  output$table_view <- renderDT({
    d <- table_rows() |>
      mutate(num = ifelse(unit == "per 1,000", formatC(value, format = "f", digits = 0),
                          formatC(value, format = "f", digits = 1)),
             cell = case_when(flag %in% "suppressed"   ~ "*",
                              is.na(value)             ~ "–",
                              flag %in% "25-49 cases"  ~ paste0("(", num, ")"),
                              TRUE                     ~ num)) |>
      select(Panel = panel, Group = group, Category = category, indicator, cell) |>
      pivot_wider(names_from = indicator, values_from = cell)
    if (all(is.na(d$Panel))) d$Panel <- NULL
    datatable(d, rownames = FALSE, selection = "none", extensions = "FixedColumns",
              options = list(pageLength = 50, dom = "ftip", scrollX = TRUE,
                             fixedColumns = list(leftColumns = if ("Panel" %in% names(d)) 3 else 2)))
  })

  output$download_table <- downloadHandler(
    filename = function() paste0("kmdhs_2025-26_table_", table_sel(), ".csv"),
    content  = function(file) write.csv(table_rows(), file, row.names = FALSE, na = "")
  )
}
