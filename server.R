# =============================================================================
# server.R — Resident Education Dashboard
# =============================================================================

server <- function(input, output, session) {

  # ── Date range auto-updates when academic year changes ──────────────────────
  observeEvent(input$year_val, {
    yrs  <- as.integer(str_split_i(input$year_val, "\u2013", 1))
    from <- max(as.Date(paste0(yrs,     "-07-01")), date_min)
    to   <- min(as.Date(paste0(yrs + 1, "-06-30")), date_max)
    updateDateRangeInput(session, "date_range",
                         start = from, end = to,
                         min = date_min, max = date_max)
  }, ignoreInit = FALSE)

  # ── Reset all filters ───────────────────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateSelectInput(session,        "year_val", selected = all_acad_years[1])
    updateCheckboxGroupInput(session, "pgy",      selected = all_pgy)
    updateCheckboxGroupInput(session, "program",  selected = all_programs)
    updateCheckboxGroupInput(session, "dis_team", selected = all_dis_teams)
  })

  # ── Core filtered datasets ──────────────────────────────────────────────────
  filtered_procs <- reactive({
    req(input$date_range, length(input$pgy) > 0, length(input$program) > 0)
    filter_df(procedures,
              input$date_range[1], input$date_range[2],
              input$pgy, input$program,
              dis_sel = input$dis_team)
  })

  filtered_visits <- reactive({
    req(input$date_range, length(input$pgy) > 0, length(input$program) > 0)
    filter_df(visits,
              input$date_range[1], input$date_range[2],
              input$pgy, input$program,
              dis_sel = input$dis_team)
  })

  # ── Resident and program aggregates ────────────────────────────────────────
  res_agg <- reactive({
    ev <- filtered_procs()
    req(nrow(ev) > 0)
    make_res_agg(ev)
  })

  prog_proc_agg <- reactive({
    ev <- filtered_procs()
    ra <- res_agg()
    req(nrow(ev) > 0, nrow(ra) > 0)
    make_prog_proc_agg(ev, ra)
  })

  # ── Sidebar description ─────────────────────────────────────────────────────
  output$filter_desc_ui <- renderUI({
    req(input$date_range)
    tags$small(class = "text-muted fst-italic",
      describe_filters(input$date_range[1], input$date_range[2],
                       input$pgy, input$program, all_pgy, all_programs)
    )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # Tab: Procedure Summary
  # ═══════════════════════════════════════════════════════════════════════════

  output$vbox_procs <- renderUI({
    ep <- tryCatch(filtered_procs(), error = function(e) tibble())
    value_box("Total Procedures", format(nrow(ep), big.mark = ","),
              theme = "primary", showcase = bs_icon("clipboard2-check"))
  })

  output$vbox_residents <- renderUI({
    ep <- tryCatch(filtered_procs(), error = function(e) tibble())
    value_box("Residents with Records", n_distinct(ep$resident_id),
              theme = value_box_theme(bg = "#E9A835", fg = "white"),
              showcase = bs_icon("people"))
  })

  output$vbox_procs_per_res <- renderUI({
    ep    <- tryCatch(filtered_procs(), error = function(e) tibble())
    n_res <- n_distinct(ep$resident_id)
    value_box("Procedures / Resident",
              if (n_res > 0) round(nrow(ep) / n_res, 1) else "\u2014",
              theme    = value_box_theme(bg = "#6C757D", fg = "white"),
              showcase = bs_icon("person-check"),
              footer   = "Among residents with \u2265 1 procedure")
  })

  output$prog_proc_table <- renderDT({
    ppa <- tryCatch(prog_proc_agg(), error = function(e) tibble())
    if (nrow(ppa) == 0)
      return(datatable(tibble(Message = "No records match filters."), rownames = FALSE))

    out <- ppa |>
      group_by(procedure_type) |>
      summarise(proc_count = sum(proc_count), .groups = "drop") |>
      mutate(pct_of_total = proc_count / pmax(sum(proc_count), 1) * 100) |>
      transmute(
        `Procedure Type`   = procedure_type,
        `Procedure Count`  = proc_count,
        `% of Total Procs` = paste0(round(pct_of_total), "%")
      ) |>
      arrange(`Procedure Type`)

    datatable(out, rownames = FALSE, filter = "top",
              extensions = "Buttons",
              options = list(
                dom       = "Bfrtip",
                buttons   = list(list(
                  extend = "excel",
                  title  = paste("Procedure Summary", input$date_range[1],
                                 "to", input$date_range[2])
                )),
                pageLength = 20
              ))
  })

  # ── Excel export ────────────────────────────────────────────────────────────
  output$dl_excel <- downloadHandler(
    filename = function() paste0("resident_education_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      ep <- tryCatch(filtered_procs(),  error = function(e) tibble())
      ev <- tryCatch(filtered_visits(), error = function(e) tibble())
      writexl::write_xlsx(list(Procedures = ep, Visits = ev), path = file)
    }
  )

  # ═══════════════════════════════════════════════════════════════════════════
  # Tab: Resident
  # ═══════════════════════════════════════════════════════════════════════════

  # Update resident dropdown when filters change
  observe({
    ep <- tryCatch(filtered_procs(), error = function(e) tibble())
    if (nrow(ep) == 0) {
      updateSelectizeInput(session, "selected_resident",
                           choices = character(0), server = FALSE)
      return()
    }
    choices <- sort(unique(ep$name_pk))
    current <- isolate(input$selected_resident)
    new_sel <- if (!is.null(current) && current %in% choices) current else character(0)
    updateSelectizeInput(session, "selected_resident",
                         choices = choices, selected = new_sel, server = FALSE)
  })

  sel_res <- reactive({
    req(input$selected_resident, nchar(input$selected_resident) > 0)
    res_agg() |> filter(resident_id == input$selected_resident)
  })

  output$resident_info_ui <- renderUI({
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) == 0)
      return(tags$small(class = "text-muted", "No resident selected."))
    tags$div(class = "small",
      tags$b(sr$resident_name[1]), tags$br(),
      sr$program[1], " \u00b7 ", as.character(sr$PGY[1])
    )
  })

  output$resident_validation_ui <- renderUI({
    req(input$selected_resident, nchar(input$selected_resident) > 0)
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) == 0) {
      div(class = "alert alert-warning mt-1",
          bs_icon("exclamation-triangle"), " ",
          "No records for this resident under current filters.")
    } else if (nrow(sr) > 1) {
      div(class = "alert alert-warning mt-1",
          bs_icon("exclamation-triangle"), " ",
          "Resident appears in multiple program/PGY combinations. ",
          "Apply a more specific filter.")
    } else NULL
  })

  bench <- reactive({
    req(input$selected_resident, nchar(input$selected_resident) > 0)
    ra <- tryCatch(res_agg(), error = function(e) tibble())
    req(nrow(ra) > 0)
    make_benchmark(ra, input$selected_resident, input$bench_type)
  })

  # ── Resident metric boxes ───────────────────────────────────────────────────
  output$resident_metrics_ui <- renderUI({
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) != 1) return(NULL)

    bc        <- tryCatch(bench(), error = function(e) tibble())
    peer_tots <- bc$total_procs
    peer_med  <- median(peer_tots)
    diff_med  <- sr$total_procs - peer_med
    pct_rnk   <- pct_rank_fn(peer_tots, sr$total_procs)
    n_peers   <- n_distinct(bc$resident_id)

    pct_lbl <- if (is.na(pct_rnk)) paste0("< ", MIN_PEERS, " peers")
               else paste0(pct_rnk, "th pct")

    layout_column_wrap(
      width = 1 / 5, fill = FALSE,
      value_box("This Resident", sr$total_procs,
                theme = value_box_theme(bg = RESIDENT_HIGHLIGHT, fg = "white"),
                showcase = bs_icon("person-fill")),
      value_box("Peer Median", round(peer_med, 1),
                theme = "secondary", showcase = bs_icon("people")),
      value_box("vs. Peer Median",
                paste0(if (diff_med >= 0) "+" else "", round(diff_med, 1)),
                theme = if (diff_med >= 0) value_box_theme(bg = "#57A773", fg = "white")
                        else "warning",
                showcase = bs_icon("arrow-left-right")),
      value_box("Percentile", pct_lbl,
                theme = "light", showcase = bs_icon("bar-chart-steps"),
                footer = paste0("Mid-rank \u00b7 n = ", n_peers, " peers")),
      value_box("Distinct Types", sr$distinct_types,
                theme = "light", showcase = bs_icon("list-check"))
    )
  })

  # ── Distribution boxplot ────────────────────────────────────────────────────
  output$distribution_plot <- renderPlot({
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) != 1) return(empty_plot("Select a resident to view comparison."))
    bc <- tryCatch(bench(), error = function(e) tibble())
    if (nrow(bc) == 0) return(empty_plot("No peer group available."))

    bc    <- bc |> mutate(is_sel = resident_id == input$selected_resident)
    sel_pt <- bc |> filter(is_sel)

    ggplot(bc, aes(x = total_procs, y = "")) +
      geom_boxplot(fill = PEER_COLOR, color = "#78909C",
                   outlier.shape = NA, width = 0.5, linewidth = 0.9) +
      geom_jitter(aes(color = is_sel, size = is_sel),
                  position = position_jitter(height = 0.15, seed = 7),
                  alpha = 0.75) +
      {if (nrow(sel_pt) > 0)
        geom_label(data = sel_pt, aes(label = resident_name),
                   nudge_y = 0.33, size = 3.8, color = RESIDENT_HIGHLIGHT,
                   fontface = "bold", label.padding = unit(0.2, "lines"),
                   label.size = 0.25, fill = "white")} +
      scale_color_manual(values = c("FALSE" = PEER_COLOR, "TRUE" = RESIDENT_HIGHLIGHT),
                         guide = "none") +
      scale_size_manual(values  = c("FALSE" = 2.5, "TRUE" = 5.5), guide = "none") +
      labs(x        = "Total Documented Procedures",
           y        = NULL,
           title    = "Peer Volume Distribution",
           subtitle = paste0(input$bench_type, " \u00b7 n = ",
                             n_distinct(bc$resident_id), " residents")) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y        = element_blank(),
            panel.grid.major.y = element_blank(),
            plot.title         = element_text(face = "bold", size = 12))
  })

  # ── Procedure-specific comparison ───────────────────────────────────────────
  output$proc_specific_plot <- renderPlot({
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) != 1) return(empty_plot("Select a resident to view comparison."))
    bc <- tryCatch(bench(), error = function(e) tibble())
    ep <- tryCatch(filtered_procs(), error = function(e) tibble())
    if (nrow(bc) == 0 || nrow(ep) == 0) return(empty_plot("No peer data."))

    peer_ids <- unique(bc$resident_id)
    types    <- sort(unique(ep$procedure_type))
    n_peers  <- n_distinct(bc$resident_id)

    res_proc <- ep |>
      filter(resident_id == input$selected_resident) |>
      count(procedure_type, name = "res_count")

    peer_stats <- expand_grid(resident_id = peer_ids, procedure_type = types) |>
      left_join(
        ep |> filter(resident_id %in% peer_ids) |>
          count(resident_id, procedure_type, name = "n"),
        by = c("resident_id", "procedure_type")
      ) |>
      mutate(n = replace_na(n, 0L)) |>
      group_by(procedure_type) |>
      summarise(peer_med = median(n), peer_p25 = quantile(n, 0.25),
                peer_p75 = quantile(n, 0.75), .groups = "drop")

    # Wrap names first, then reorder by resident count (low → high)
    plot_df <- tibble(procedure_type = types) |>
      left_join(res_proc,   by = "procedure_type") |>
      left_join(peer_stats, by = "procedure_type") |>
      mutate(
        res_count  = replace_na(res_count, 0L),
        across(c(peer_med, peer_p25, peer_p75), ~replace_na(., 0)),
        proc_label = fct_reorder(str_wrap(procedure_type, width = 20), res_count)
      )

    ggplot(plot_df, aes(y = proc_label)) +
      geom_segment(aes(x = peer_p25, xend = peer_p75, yend = proc_label),
                   color = PEER_COLOR, linewidth = 5, alpha = 0.85, lineend = "round") +
      geom_point(aes(x = peer_med),  shape = 18, size = 6,   color = "#607D8B") +
      geom_point(aes(x = res_count), shape = 19, size = 5.5, color = RESIDENT_HIGHLIGHT) +
      geom_text(aes(x = res_count, label = res_count),
                hjust = -0.55, size = 7.7, color = RESIDENT_HIGHLIGHT, fontface = "bold") +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.22)),
                         breaks = scales::breaks_pretty(n = 5)) +
      labs(x        = "Procedure Count",
           y        = NULL,
           title    = "Procedure-Specific Comparison",
           subtitle = paste0(
             input$bench_type, " \u00b7 n = ", n_peers, " peers\n",
             "\u25cf = this resident    \u25c6 = peer median    \u2015\u2015 = peer IQR"
           )) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_line(color = "#EEEEEE", linewidth = 0.5),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_line(color = "#E0E0E0", linewidth = 0.4),
            axis.text.y        = element_text(size = 11, lineheight = 0.9,
                                              margin = margin(r = 6)),
            axis.text.x        = element_text(size = 10),
            plot.title         = element_text(face = "bold", size = 13),
            plot.subtitle      = element_text(size = 9.5, color = "#555555",
                                              lineheight = 1.4),
            plot.margin        = margin(8, 16, 8, 8))
  })

  # ── Resident detail sub-tab ─────────────────────────────────────────────────
  output$resident_detail_header <- renderUI({
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) == 0)
      return(div(class = "alert alert-info mt-1",
                 bs_icon("info-circle"), " Select a resident on the Comparison tab."))
    tags$p(class = "text-muted small mb-2",
      bs_icon("person-fill"), " ",
      tags$b(sr$resident_name[1]), " \u00b7 ",
      sr$program[1], " \u00b7 ", as.character(sr$PGY[1]),
      " \u00b7 Benchmark: ", tags$em(input$bench_type)
    )
  })

  output$resident_detail_table <- renderDT({
    sr <- tryCatch(sel_res(), error = function(e) tibble())
    if (nrow(sr) != 1)
      return(datatable(tibble(Message = "Select a resident to view detail."),
                       rownames = FALSE, options = list(dom = "t")))

    bc       <- tryCatch(bench(), error = function(e) tibble())
    ep       <- tryCatch(filtered_procs(), error = function(e) tibble())
    peer_ids <- unique(bc$resident_id)
    types    <- sort(unique(ep$procedure_type))

    res_proc <- ep |>
      filter(resident_id == input$selected_resident) |>
      count(procedure_type, name = "res_count")

    peer_detail <- expand_grid(resident_id = peer_ids, procedure_type = types) |>
      left_join(
        ep |> filter(resident_id %in% peer_ids) |>
          count(resident_id, procedure_type, name = "n"),
        by = c("resident_id", "procedure_type")
      ) |>
      mutate(n = replace_na(n, 0L)) |>
      group_by(procedure_type) |>
      summarise(peer_med  = round(median(n), 1),
                peer_p25  = round(quantile(n, 0.25), 1),
                peer_p75  = round(quantile(n, 0.75), 1),
                peer_vals = list(n), .groups = "drop")

    tibble(procedure_type = types) |>
      left_join(res_proc,    by = "procedure_type") |>
      left_join(peer_detail, by = "procedure_type") |>
      mutate(
        res_count = replace_na(res_count, 0L),
        diff_med  = res_count - peer_med,
        pct_rnk   = map2_dbl(peer_vals, res_count,
                              ~pct_rank_fn(.x, as.numeric(.y)))
      ) |>
      transmute(
        `Procedure Type`   = procedure_type,
        `Resident Count`   = res_count,
        `Peer Median`      = peer_med,
        `Peer 25th`        = peer_p25,
        `Peer 75th`        = peer_p75,
        `Diff from Median` = round(diff_med, 1),
        `Percentile`       = if_else(is.na(pct_rnk), "\u2014",
                                     paste0(round(pct_rnk, 1), "th"))
      ) |>
      arrange(desc(`Resident Count`)) |>
      datatable(rownames = FALSE,
                options = list(dom = "ft", pageLength = 25, scrollX = TRUE))
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # Tab: Diagnoses
  # ═══════════════════════════════════════════════════════════════════════════

  output$vbox_dx_groups <- renderUI({
    ev <- tryCatch(filtered_visits(), error = function(e) tibble())
    value_box("Distinct Diagnosis Groups",
              n_distinct(ev$icd10_major_group),
              theme = value_box_theme(bg = "#57A773", fg = "white"),
              showcase = bs_icon("journals"))
  })

  output$vbox_top_dx <- renderUI({
    ev  <- tryCatch(filtered_visits(), error = function(e) tibble())
    top <- ev |>
      distinct(icd10_major_group, csn) |>
      count(icd10_major_group, sort = TRUE) |>
      slice_head(n = 1) |>
      pull(icd10_major_group)
    value_box("Most Common Diagnosis Group",
              if (length(top) > 0) top else "\u2014",
              theme    = value_box_theme(bg = "#E9A835", fg = "white"),
              showcase = bs_icon("award"))
  })

  output$dx_resident_dist_plot <- renderPlotly({
    ev <- tryCatch(filtered_visits(), error = function(e) tibble())
    if (nrow(ev) == 0)
      return(plotly_empty() |> layout(title = "No visits match the selected filters."))

    # Count distinct CSNs per program × diagnosis group
    visit_counts <- ev |>
      distinct(program, icd10_major_group, csn) |>
      count(program, icd10_major_group, name = "n")

    # Order diagnosis groups and programs by total volume
    dx_order <- visit_counts |>
      count(icd10_major_group, wt = n, name = "total") |>
      arrange(desc(total)) |>
      pull(icd10_major_group)

    prog_order <- visit_counts |>
      count(program, wt = n, name = "total") |>
      arrange(total) |>
      pull(program)

    prog_dx <- visit_counts |>
      mutate(
        program           = factor(program, levels = prog_order),
        icd10_major_group = factor(icd10_major_group, levels = dx_order),
        tooltip = paste0("Program: ", program,
                         "\nDiagnosis: ", icd10_major_group,
                         "\nVisits: ", n)
      )

    p <- ggplot(prog_dx, aes(x = n, y = program, fill = icd10_major_group)) +
      geom_col(aes(text = tooltip), position = "stack", width = 0.7) +
      scale_x_continuous(labels = label_comma()) +
      scale_fill_viridis_d(option = "D", name = "Diagnosis Group") +
      labs(title    = "Diagnosis Groups by Program",
           subtitle = "Distinct visits (CSNs) per diagnosis group",
           x = "Visit Count", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(),
            plot.title         = element_text(face = "bold", size = 12),
            axis.text.y        = element_text(color = "#111111"),
            plot.margin        = margin(l = 10))

    ggplotly(p, tooltip = "text")
  })

}
