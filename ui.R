# =============================================================================
# ui.R — Resident Education Dashboard
# =============================================================================

ui <- page_navbar(
  title = "Resident Education Dashboard",
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#2E86AB",
    base_font  = font_google("Inter")
  ),

  # ── Compact value-box + compact sidebar CSS ────────────────────────────────
  header = tags$head(tags$style(HTML("
    .bslib-value-box                     { min-height: 72px !important; }
    .bslib-value-box .value-box-title    { font-size: 0.70rem !important; line-height: 1.2; }
    .bslib-value-box .value-box-value    { font-size: 1.20rem !important; line-height: 1.3; }
    .bslib-value-box .value-box-showcase { padding: 0.4rem !important; }
    .bslib-value-box .value-box-footer   { font-size: 0.65rem !important; }
    /* Compact sidebar font */
    .bslib-sidebar-layout > .sidebar     { font-size: 0.78rem !important; }
    .bslib-sidebar-layout > .sidebar .form-label,
    .bslib-sidebar-layout > .sidebar label { font-size: 0.75rem !important; }
    .bslib-sidebar-layout > .sidebar .form-control,
    .bslib-sidebar-layout > .sidebar .form-select,
    .bslib-sidebar-layout > .sidebar .selectize-input { font-size: 0.75rem !important; }
    .bslib-sidebar-layout > .sidebar .form-check-label { font-size: 0.74rem !important; }
    .bslib-sidebar-layout > .sidebar hr { margin: 0.3rem 0 !important; }
    .bslib-sidebar-layout > .sidebar .btn { font-size: 0.74rem !important; }
  "))),

  # ── Persistent sidebar ─────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 240,

    # Academic year
    selectInput("year_val", "Academic Year",
      choices  = all_acad_years,
      selected = all_acad_years[1]
    ),

    # Date range (defaults to full selected year; can be narrowed)
    dateRangeInput("date_range", "Date Range",
      start = date_min, end = date_max,
      min   = date_min, max = date_max,
      format = "M d, yyyy"
    ),

    hr(style = "margin: 0.5rem 0;"),

    tags$label("PGY Level", class = "form-label"),
    checkboxGroupInput("pgy", NULL,
      choices  = all_pgy,
      selected = all_pgy
    ),

    tags$label("Program", class = "form-label"),
    checkboxGroupInput("program", NULL,
      choices  = all_programs,
      selected = all_programs
    ),

    hr(style = "margin: 0.5rem 0;"),

    checkboxGroupInput("dis_team", "Discharge Team",
      choices  = all_dis_teams,
      selected = all_dis_teams
    ),

    hr(style = "margin: 0.5rem 0;"),

    actionButton("reset_filters", "Reset Filters",
      class = "btn-outline-secondary btn-sm w-100",
      icon  = icon("rotate-left")
    ),

    hr(style = "margin: 0.5rem 0;"),

    uiOutput("filter_desc_ui"),

    hr(style = "margin: 0.5rem 0;"),

    tags$small(class = "text-muted fst-italic",
      tags$b("Note:"), " All metrics count only the top 8 procedure types by volume. ",
      "Miscellaneous or low-frequency procedures are excluded."
    )
  ),

  # ── Tab 1: Resident ────────────────────────────────────────────────────────
  nav_panel(
    "Resident",
    icon = bs_icon("person-lines-fill"),

    # Resident selector — visible on both sub-tabs
    card(
      card_header("Select Resident"),
      layout_columns(
        col_widths = c(5, 4, 3),
        selectizeInput("selected_resident", "Resident",
          choices = NULL,
          options = list(placeholder = "Search for a resident\u2026")
        ),
        selectInput("bench_type", "Benchmark Group",
          choices = c(
            "Same program & PGY"    = "Same program & PGY",
            "Same program, all PGY" = "Same program, all PGY"
          ),
          selected = "Same program & PGY"
        ),
        div(style = "padding-top: 1.75rem;", uiOutput("resident_info_ui"))
      )
    ),

    uiOutput("resident_validation_ui"),

    navset_card_underline(

      # ── Comparison sub-tab ────────────────────────────────────────────────
      nav_panel("Comparison",

        uiOutput("resident_metrics_ui"),

        layout_columns(
          col_widths = c(5, 7),

          card(
            full_screen = TRUE,
            card_header("Total Volume Distribution in Peer Group"),
            plotOutput("distribution_plot", height = "300px"),
            card_footer(tags$small(class = "text-muted",
              "Each point = one peer resident. Box = middle 50% (IQR). ",
              tags$b("Red"), " = selected resident. ",
              tags$b("Percentile"), " = share of peers with fewer procedures ",
              "(mid-rank method; requires \u2265 5 peers)."
            ))
          ),

          card(
            full_screen = TRUE,
            card_header("Procedure-Specific Comparison"),
            plotOutput("proc_specific_plot", height = "420px"),
            card_footer(tags$small(class = "text-muted",
              tags$b("Red dot"), " = this resident. \u2003",
              tags$b("Gray \u25c6"), " = peer median. \u2003",
              tags$b("Gray bar"), " = peer IQR (25th\u201375th pct). \u2003",
              "Zeros included for peers with no instances of a type."
            ))
          )
        )
      ),

      # ── Detail sub-tab ────────────────────────────────────────────────────
      nav_panel("Detail",
        uiOutput("resident_detail_header"),
        card(
          card_header("Procedure Detail by Type"),
          DTOutput("resident_detail_table")
        )
      )
    )
  ),

  # ── Tab 2: Procedure Summary ───────────────────────────────────────────────
  nav_panel(
    "Procedure Summary",
    icon = bs_icon("table"),

    # Value boxes
    layout_column_wrap(
      width = 1 / 3,
      fill  = FALSE,
      uiOutput("vbox_procs"),
      uiOutput("vbox_residents"),
      uiOutput("vbox_procs_per_res")
    ),

    card(
      full_screen = TRUE,
      card_header("Procedure Summary"),
      DTOutput("prog_proc_table")
    )
  ),

  # ── Tab 3: Diagnoses ───────────────────────────────────────────────────────
  nav_panel(
    "Diagnoses",
    icon = bs_icon("clipboard2-pulse"),

    # Value boxes
    layout_column_wrap(
      width = 1 / 2,
      fill  = FALSE,
      uiOutput("vbox_dx_groups"),
      uiOutput("vbox_top_dx")
    ),

    layout_columns(
      col_widths = 12,

      card(
        full_screen = TRUE,
        card_header("Resident Visit Distribution by Diagnosis Group"),
        card_footer(tags$small(class = "text-muted",
          "Each point = one resident's visit count. Line = group median. Top 10 groups."
        )),
        plotlyOutput("dx_resident_dist_plot", height = "420px")
      )
    )
  ),

  # ── Tab 4: Export ─────────────────────────────────────────────────────────
  nav_panel(
    "Export",
    icon = bs_icon("download"),

    card(
      card_header("Export Filtered Data"),
      tags$p(class = "text-muted mb-3",
        "Downloads an Excel workbook with Procedures and Visits as separate sheets, ",
        "reflecting the currently active sidebar filters."
      ),
      downloadButton("dl_excel", "Download Excel Workbook",
        class = "btn-primary btn-sm",
        icon  = icon("file-excel")
      )
    )
  )
)
