# =============================================================================
# server.R: reactive logic only. Data and helpers come from global.R.
# =============================================================================

function(input, output, session) {

  # ---- Shared state ---------------------------------------------------------------------
  selected_county <- reactiveVal(NULL)                 # highlighted county (map, ranking)
  pending_ind     <- reactiveVal(NULL)                 # indicator to select after a topic change
  table_sel       <- reactiveVal(catalog$table_id[1])  # table shown in the Tables tab

  ink <- reactive({
    if (identical(input$dark_mode, "dark")) {
      list(text = "#f1e6e2", muted = "#b7a39d", grid = "#3a2e2c", bar = "#e39a88", hi = "#5cc3e3")
    } else {
      list(text = "#2b2222", muted = "#7a6662", grid = "#eee4df", bar = ACCENT, hi = HIGHLIGHT)
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
    } else if (identical(q$tab, "about")) {
      nav_select("nav", "About")
    }
  }, once = TRUE, priority = 10)

  observe({
    req(input$nav)
    q <- switch(input$nav,
                "County explorer" = c(tab = "explorer", ind = input$ind, county = selected_county()),
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
    leaflet(options = leafletOptions(attributionControl = FALSE, zoomSnap = 0.25)) |>
      fitBounds(33.9, -4.7, 41.9, 5.0) |>
      draw_counties(isolate(by_cty()), isolate(meta())) |>
      draw_highlight(isolate(selected_county())) |>
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
  })

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
    bindCache(input$ind, selected_county() %||% "", input$dark_mode)

  observeEvent(event_data("plotly_click", source = "rank"), {
    set_county(event_data("plotly_click", source = "rank")$customdata[[1]])
  })

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
    bindCache(input$profile_county, input$dark_mode)

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
