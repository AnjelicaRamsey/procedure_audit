# =============================================================================
# global.R — Resident Education Dashboard
# Loaded once at startup; shared by ui.R and server.R
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(plotly)
library(writexl)

# ── Paths ─────────────────────────────────────────────────────────────────────
PROC_PATH  <- "data/procedures_joined_stub.csv"
VISIT_PATH <- "data/visits_joined_stub.csv"

# Minimum peer-group size for percentile calculation
MIN_PEERS <- 5

RESIDENT_HIGHLIGHT <- "#C0392B"
PEER_COLOR         <- "#B0BEC5"

# ── Dynamic palette builder (for proc types, dx groups, programs, teams) ──────
make_palette <- function(vals) {
  vals <- sort(unique(na.omit(as.character(vals))))
  setNames(hue_pal()(length(vals)), vals)
}

# Build program colors dynamically from whatever programs exist in the data
get_program_colors <- function(programs) {
  all_cols <- make_palette(programs)
  all_cols[programs]
}

# ── Data preparation ──────────────────────────────────────────────────────────

prep_procedures <- function(path = PROC_PATH) {
  read_csv(path, show_col_types = FALSE) |>
    rename(
      program  = `Program/Type`,
      rotation = `UC/ED Rotation`
    ) |>
    mutate(
      # ── RESIDENT IDENTIFIER — change here to swap to employee ID
      resident_id  = name_pk,

      service_date = as.Date(service_date),

      # Academic year: July 1 starts the next AY
      acad_year = paste0(
        if_else(month(service_date) >= 7, year(service_date),     year(service_date) - 1L),
        "\u2013",
        if_else(month(service_date) >= 7, year(service_date) + 1L, year(service_date))
      ),

      # PGY as ordered factor (input is numeric 1-4)
      PGY = factor(paste0("PGY", PGY), levels = paste0("PGY", 1:4), ordered = TRUE)
    ) |>
    # Deduplicate on procedure event key
    distinct(note_id, .keep_all = TRUE) |>
    # Drop PHI
    select(-mrn, -dob, -note_text, -Email,
           -Last_Name, -First_Name, -resident_middle)
}

prep_visits <- function(path = VISIT_PATH) {
  read_csv(path, show_col_types = FALSE) |>
    rename(
      program  = `Program/Type`,
      rotation = `UC/ED Rotation`
    ) |>
    mutate(
      resident_id  = name_pk,
      service_date = as.Date(service_date),
      acad_year = paste0(
        if_else(month(service_date) >= 7, year(service_date),     year(service_date) - 1L),
        "\u2013",
        if_else(month(service_date) >= 7, year(service_date) + 1L, year(service_date))
      ),
      PGY = factor(paste0("PGY", PGY), levels = paste0("PGY", 1:4), ordered = TRUE)
    ) |>
    distinct(csn, .keep_all = TRUE) |>
    select(-mrn, -dob, -note_text, -Email,
           -Last_Name, -First_Name, -resident_middle)
}

# ── Shared filter function (works on procedures or visits) ────────────────────
filter_df <- function(df, date_from, date_to, pgy_sel, prog_sel,
                      arr_sel = NULL, dis_sel = NULL) {
  out <- df |>
    filter(
      service_date >= date_from,
      service_date <= date_to,
      as.character(PGY) %in% pgy_sel,
      program %in% prog_sel
    )
  if (!is.null(arr_sel) && length(arr_sel) > 0 && !setequal(arr_sel, all_arr_teams))
    out <- out |> filter(arrival_team %in% arr_sel)
  if (!is.null(dis_sel) && length(dis_sel) > 0 && !setequal(dis_sel, all_dis_teams))
    out <- out |> filter(discharge_team %in% dis_sel)
  out
}

# ── Resident-level aggregate ──────────────────────────────────────────────────
make_res_agg <- function(events) {
  events |>
    group_by(resident_id, resident_name, program, PGY) |>
    summarise(
      total_procs    = n(),
      distinct_types = n_distinct(procedure_type),
      .groups        = "drop"
    )
}

# ── Program-level aggregate ───────────────────────────────────────────────────
make_prog_agg <- function(res_agg) {
  res_agg |>
    group_by(resident_id, program) |>
    summarise(total_procs = sum(total_procs),
              distinct_types = max(distinct_types), .groups = "drop") |>
    group_by(program) |>
    summarise(
      total_procs        = sum(total_procs),
      n_residents        = n_distinct(resident_id),
      procs_per_resident = total_procs / n_residents,
      median_procs       = median(total_procs),
      p25_procs          = quantile(total_procs, 0.25),
      p75_procs          = quantile(total_procs, 0.75),
      avg_distinct_types = mean(distinct_types),
      .groups            = "drop"
    )
}

# ── Program × procedure-type aggregate (zeros included) ──────────────────────
make_prog_proc_agg <- function(events, res_agg) {
  types  <- sort(unique(events$procedure_type))
  grid   <- res_agg |>
    group_by(resident_id, program) |>
    summarise(.groups = "drop") |>
    crossing(procedure_type = types)

  res_proc <- events |>
    count(resident_id, program, procedure_type, name = "n")

  grid |>
    left_join(res_proc, by = c("resident_id", "program", "procedure_type")) |>
    mutate(n = replace_na(n, 0L)) |>
    group_by(program, procedure_type) |>
    summarise(
      proc_count     = sum(n),
      n_residents    = n_distinct(resident_id),
      median_per_res = median(n),
      p25_per_res    = quantile(n, 0.25),
      p75_per_res    = quantile(n, 0.75),
      .groups        = "drop"
    ) |>
    group_by(program) |>
    mutate(
      pct_of_program     = proc_count / pmax(sum(proc_count), 1) * 100,
      procs_per_resident = proc_count / n_residents
    ) |>
    ungroup()
}

# ── Resident visits aggregate ─────────────────────────────────────────────────
make_res_visit_agg <- function(visits) {
  visits |>
    group_by(resident_id, resident_name, program, PGY) |>
    summarise(total_visits = n(), .groups = "drop")
}

# ── Benchmark cohort ──────────────────────────────────────────────────────────
make_benchmark <- function(res_agg, selected_name, bench_type) {
  sel <- res_agg |> filter(resident_name == selected_name)
  if (nrow(sel) == 0) return(tibble())

  sel_program <- sel$program[1]
  sel_pgy     <- sel$PGY[1]

  cohort <- switch(bench_type,
    "Same program & PGY"    = res_agg |> filter(program == sel_program, PGY == sel_pgy),
    "Same program, all PGY" = res_agg |> filter(program == sel_program),
    res_agg |> filter(program == sel_program, PGY == sel_pgy)
  )

  cohort |>
    group_by(resident_id, resident_name) |>
    summarise(
      program        = first(program),
      PGY            = first(PGY),
      total_procs    = sum(total_procs),
      distinct_types = max(distinct_types),
      .groups        = "drop"
    )
}

# ── Percentile rank (mid-rank method for ties) ────────────────────────────────
pct_rank_fn <- function(peer_vals, resident_val) {
  if (length(peer_vals) < MIN_PEERS) return(NA_real_)
  below <- sum(peer_vals < resident_val,  na.rm = TRUE)
  equal <- sum(peer_vals == resident_val, na.rm = TRUE)
  round((below + 0.5 * equal) / length(peer_vals) * 100, 1)
}

# ── Filter description ────────────────────────────────────────────────────────
describe_filters <- function(date_from, date_to, pgy_sel, prog_sel,
                             all_pgy, all_programs) {
  pgy_str  <- if (setequal(pgy_sel, all_pgy)) "all PGY levels"
              else paste(sort(pgy_sel), collapse = ", ")
  prog_str <- if (setequal(prog_sel, all_programs)) "all programs"
              else paste(prog_sel, collapse = " & ")
  paste0("Showing ", pgy_str, " in ", prog_str,
         " \u00b7 ", format(date_from, "%b %d, %Y"),
         " \u2013 ", format(date_to,   "%b %d, %Y"))
}

# ── Empty-plot placeholder ────────────────────────────────────────────────────
empty_plot <- function(msg = "No data available.") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg,
             size = 4.5, color = "#9E9E9E", hjust = 0.5) +
    theme_void()
}

# ── Load data at startup ──────────────────────────────────────────────────────
procedures <- prep_procedures()
visits     <- prep_visits()

# ── Dynamic choice vectors ────────────────────────────────────────────────────
all_acad_years <- sort(unique(procedures$acad_year), decreasing = TRUE)
all_pgy        <- levels(procedures$PGY) |> intersect(as.character(procedures$PGY))
all_programs   <- sort(unique(procedures$program))
all_proc_types <- sort(unique(procedures$procedure_type))
all_dx_groups  <- sort(unique(visits$icd10_major_group))
all_arr_teams  <- sort(unique(na.omit(procedures$arrival_team)))
all_dis_teams  <- sort(unique(na.omit(procedures$discharge_team)))
date_min       <- min(procedures$service_date, na.rm = TRUE)
date_max       <- max(procedures$service_date, na.rm = TRUE)

# ── Palettes built from data ──────────────────────────────────────────────────
program_colors   <- get_program_colors(all_programs)
proc_type_colors <- make_palette(all_proc_types)
dx_group_colors  <- make_palette(all_dx_groups)
