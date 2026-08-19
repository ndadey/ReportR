# ==============================================================================
# Line 1 — General Trend Analysis: Achievement Level Transitions
#
# Functions:
#   compute_transitions()            — cohort-matched year-to-year transition tables
#   plot_transition_heatmaps()       — average transition heatmap per subject
#   plot_transition_alluvial()       — alluvial flow per subject/grade (ggalluvial)
#   plot_transition_rates()          — dot chart of total/net rates across cells
#   flag_transitions()               — normative z-score flagging
#   compute_transition_comparisons() — link to score and proficiency trends
#   summarize_transition_findings()  — plain-language narrative
#   run_achievement_transitions()    — orchestrator
# ==============================================================================


#' Compute year-to-year achievement level transition tables
#'
#' Matches students cohort-style: Grade G in year T is matched to Grade G+1
#' in year T+1 for span-tested subjects, and on STUDENT_ID + SUBJECT only for
#' EOC subjects. Produces long-format transition counts/proportions, a summary
#' table of total and net transition rates, and average matrices pooled across
#' year-pairs.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Must contain `STUDENT_ID`, `YEAR`, `SUBJECT`,
#'   `GRADE`, and `ACHIEVEMENT_LEVEL`.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Cells with fewer students have `prop` set to `NA`.
#'   Default `10`.
#'
#' @return A named list:
#'   * `$long_table`    — one row per SUBJECT × GRADE × year-pair × from_level × to_level
#'   * `$summary_table` — one row per SUBJECT × GRADE × year-pair with
#'     `n_total`, `total_transition_rate`, `net_upward_rate`
#'   * `$avg_matrices`  — average transition proportions pooled across year-pairs
#' @export
compute_transitions <- function(student_results_long,
                                assessment_spec,
                                min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "SUBJECT", "GRADE", "ACHIEVEMENT_LEVEL") %in%
          names(student_results_long))
  )

  labels <- assessment_spec$achievement_levels$labels

  base <- student_results_long |>
    dplyr::filter(
      !is.na(.data[["ACHIEVEMENT_LEVEL"]]),
      !is.na(.data[["STUDENT_ID"]])
    ) |>
    dplyr::mutate(
      ACHIEVEMENT_LEVEL = factor(.data[["ACHIEVEMENT_LEVEL"]],
                                 levels = labels, ordered = TRUE)
    )

  yrs <- sort(unique(base[["YEAR"]]))
  if (length(yrs) < 2L) {
    message("Achievement transitions require at least two years of data.")
    return(list(long_table = NULL, summary_table = NULL, avg_matrices = NULL))
  }

  # Cohort-matched pairs across all consecutive year-pairs
  pairs_list <- lapply(seq_len(length(yrs) - 1L), function(i) {
    yr1 <- yrs[i]
    yr2 <- yrs[i + 1L]

    d1 <- base |>
      dplyr::filter(.data[["YEAR"]] == yr1) |>
      dplyr::select("STUDENT_ID", "SUBJECT", GRADE_1 = "GRADE",
                    from_level = "ACHIEVEMENT_LEVEL")

    d2 <- base |>
      dplyr::filter(.data[["YEAR"]] == yr2) |>
      dplyr::select("STUDENT_ID", "SUBJECT", GRADE_2 = "GRADE",
                    to_level = "ACHIEVEMENT_LEVEL")

    d1 |>
      dplyr::mutate(
        GRADE_2 = dplyr::if_else(
          is.na(.data[["GRADE_1"]]),
          NA_integer_,
          as.integer(.data[["GRADE_1"]]) + 1L
        )
      ) |>
      dplyr::inner_join(d2, by = c("STUDENT_ID", "SUBJECT", "GRADE_2")) |>
      dplyr::transmute(
        SUBJECT,
        GRADE  = .data[["GRADE_1"]],
        YEAR_1 = yr1,
        YEAR_2 = yr2,
        from_level,
        to_level
      )
  })

  matched <- dplyr::bind_rows(pairs_list)

  if (nrow(matched) == 0L) {
    message("No matched student records found for transition analysis.")
    return(list(long_table = NULL, summary_table = NULL, avg_matrices = NULL))
  }

  # Long table: n and proportion per from_level × to_level cell
  long_table <- matched |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]],
                    .data[["YEAR_1"]], .data[["YEAR_2"]],
                    .data[["from_level"]], .data[["to_level"]]) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]],
                    .data[["YEAR_1"]], .data[["YEAR_2"]]) |>
    dplyr::mutate(
      n_total = sum(.data[["n"]]),
      prop    = dplyr::if_else(
        .data[["n_total"]] >= min_n,
        .data[["n"]] / .data[["n_total"]],
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  # Summary: total and net rates per cell
  summary_table <- long_table |>
    dplyr::mutate(
      move_dir = dplyr::case_when(
        as.integer(.data[["from_level"]]) == as.integer(.data[["to_level"]]) ~ "stable",
        as.integer(.data[["to_level"]])   >  as.integer(.data[["from_level"]]) ~ "up",
        TRUE ~ "down"
      )
    ) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]],
                    .data[["YEAR_1"]], .data[["YEAR_2"]],
                    .data[["n_total"]], .data[["move_dir"]]) |>
    dplyr::summarize(pct = sum(.data[["prop"]], na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from  = "move_dir", values_from = "pct",
                       values_fill = 0) |>
    dplyr::mutate(
      total_transition_rate = 1 - dplyr::coalesce(.data[["stable"]], 0),
      net_upward_rate       = dplyr::coalesce(.data[["up"]], 0) -
                              dplyr::coalesce(.data[["down"]], 0)
    )

  # Individual level-to-level transition proportions joined to summary_table.
  # Column names: L<from_index>_L<to_index> e.g. L1_L2 = from level 1 to level 2.
  level_props <- long_table |>
    dplyr::mutate(
      col_name = paste0("L", as.integer(.data[["from_level"]]),
                        "_L", as.integer(.data[["to_level"]]))
    ) |>
    dplyr::select("SUBJECT", "GRADE", "YEAR_1", "YEAR_2",
                  "col_name", "prop") |>
    tidyr::pivot_wider(names_from  = "col_name",
                       values_from = "prop",
                       values_fill = NA_real_)

  summary_table <- summary_table |>
    dplyr::left_join(level_props,
                     by = c("SUBJECT", "GRADE", "YEAR_1", "YEAR_2"))

  # Average matrices pooled across year-pairs
  avg_matrices <- long_table |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]],
                    .data[["from_level"]], .data[["to_level"]]) |>
    dplyr::summarize(
      avg_prop = mean(.data[["prop"]], na.rm = TRUE),
      avg_n    = round(mean(.data[["n"]], na.rm = TRUE)),
      .groups  = "drop"
    )

  list(
    long_table    = long_table,
    summary_table = summary_table,
    avg_matrices  = avg_matrices
  )
}


#' Plot average transition heatmaps by subject
#'
#' @param transition_data Named list returned by [compute_transitions()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects (one per subject), each faceted by grade.
#' @export
plot_transition_heatmaps <- function(transition_data, assessment_spec) {
  stopifnot(
    is.list(transition_data),
    !is.null(transition_data$avg_matrices),
    is.list(assessment_spec)
  )

  labels <- assessment_spec$achievement_levels$labels

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  dat <- transition_data$avg_matrices |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      from_level = factor(.data[["from_level"]], levels = labels, ordered = TRUE),
      to_level   = factor(.data[["to_level"]],   levels = labels, ordered = TRUE),
      grade_lbl  = dplyr::if_else(
        is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
      )
    )

  subjects <- names(assessment_spec$subjects)

  out <- setNames(lapply(subjects, function(s) {
    d <- dat |> dplyr::filter(.data[["SUBJECT"]] == s)
    if (nrow(d) == 0L) return(NULL)
    lbl <- unique(d[["subj_label"]])[1L]

    ggplot2::ggplot(
      d,
      ggplot2::aes(x = .data[["to_level"]], y = .data[["from_level"]],
                   fill = .data[["avg_prop"]])
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(round(.data[["avg_prop"]] * 100, 0), "%"),
          color = .data[["avg_prop"]] > 0.5
        ),
        size = 2.5, show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey20")) +
      ggplot2::facet_wrap(~ grade_lbl, ncol = 2) +
      ggplot2::scale_fill_gradient(
        low = "#f7fbff", high = "#08519c",
        limits    = c(0, 1), na.value = "grey85",
        name      = "Avg. Transition\nProportion",
        labels    = function(x) paste0(round(x * 100), "%")
      ) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(
        title = lbl,
        x     = "Achievement Level in Year T+1",
        y     = "Achievement Level in Year T"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x      = ggplot2::element_text(angle = 30, hjust = 0, size = 7),
        axis.text.y      = ggplot2::element_text(size = 7),
        strip.background = ggplot2::element_rect(fill = "grey92")
      )
  }), subjects)

  Filter(Negate(is.null), out)
}


#' Plot alluvial (Sankey) flow diagrams for achievement level transitions
#'
#' Requires the `ggalluvial` package. If not installed, returns `NULL` with a
#' message.
#'
#' @param transition_data Named list returned by [compute_transitions()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by `"SUBJECT_Grade_G"`, or `NULL`
#'   if `ggalluvial` is unavailable.
#' @export
plot_transition_alluvial <- function(transition_data, assessment_spec) {
  stopifnot(
    is.list(transition_data),
    !is.null(transition_data$avg_matrices),
    is.list(assessment_spec)
  )

  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    message("install.packages('ggalluvial') to produce alluvial transition plots.")
    return(NULL)
  }

  labels <- assessment_spec$achievement_levels$labels

  dat <- transition_data$avg_matrices |>
    dplyr::filter(!is.na(.data[["avg_prop"]])) |>
    dplyr::mutate(
      from_level = factor(.data[["from_level"]], levels = labels, ordered = TRUE),
      to_level   = factor(.data[["to_level"]],   levels = labels, ordered = TRUE)
    )

  keys <- dat |>
    dplyr::distinct(.data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]])

  out <- vector("list", nrow(keys))
  plot_names <- character(nrow(keys))

  for (k in seq_len(nrow(keys))) {
    s <- keys[["SUBJECT"]][k]
    g <- keys[["GRADE"]][k]

    d <- dat |>
      dplyr::filter(
        .data[["SUBJECT"]] == s,
        if (is.na(g)) is.na(.data[["GRADE"]]) else !is.na(.data[["GRADE"]]) & .data[["GRADE"]] == g
      )

    if (nrow(d) == 0L || sum(d[["avg_prop"]], na.rm = TRUE) == 0) {
      next
    }

    subj_label_val <- assessment_spec$subjects[[s]]$label
    subj_label <- if (!is.null(subj_label_val)) subj_label_val else s
    grade_str  <- if (is.na(g)) "EOC" else paste0("Grade ", g)
    title_str  <- paste0(subj_label, " — ", grade_str)
    key_str    <- paste0(s, "_", if (is.na(g)) "EOC" else g)

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(
        y     = .data[["avg_n"]],
        axis1 = .data[["from_level"]],
        axis2 = .data[["to_level"]]
      )
    ) +
      ggalluvial::geom_alluvium(
        ggplot2::aes(fill = .data[["from_level"]]),
        width = 0.3, alpha = 0.75, knot.pos = 0.4
      ) +
      ggalluvial::geom_stratum(width = 0.3, fill = "grey85", color = "grey40") +
      ggplot2::geom_text(
        stat  = ggalluvial::StatStratum,
        ggplot2::aes(label = ggplot2::after_stat(stratum)),
        size  = 2.8
      ) +
      ggplot2::scale_x_discrete(
        limits = c("Year T", "Year T+1"),
        expand = c(0.1, 0.1)
      ) +
      ggplot2::scale_fill_brewer(palette = "RdYlGn", direction = 1,
                                  name = "Starting Level") +
      ggplot2::labs(
        title = title_str,
        y     = "Average Number of Students"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")

    out[[k]]       <- p
    plot_names[k]  <- key_str
  }

  names(out) <- plot_names
  Filter(Negate(is.null), out)
}


#' Plot total and net transition rates across all grade/subject/year cells
#'
#' @param transition_data Named list returned by [compute_transitions()].
#' @param assessment_spec Named list describing the assessment program.
#' @param flag_table Data frame of flagged cells (from [flag_transitions()]),
#'   or `NULL` to omit highlighting.
#'
#' @return A ggplot object.
#' @export
plot_transition_rates <- function(transition_data, assessment_spec,
                                  flag_table = NULL) {
  stopifnot(
    is.list(transition_data),
    !is.null(transition_data$summary_table),
    is.list(assessment_spec)
  )

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  dat <- transition_data$summary_table |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label  = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl   = dplyr::if_else(
        is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
      ),
      year_pair   = paste0(.data[["YEAR_1"]], "→", .data[["YEAR_2"]]),
      cell_id     = paste0(.data[["subj_label"]], " ", .data[["grade_lbl"]])
    )

  if (!is.null(flag_table) && nrow(flag_table) > 0L) {
    flag_keys <- paste0(flag_table[["SUBJECT"]], "_",
                        flag_table[["YEAR_1"]], "_",
                        flag_table[["YEAR_2"]], "_",
                        flag_table[["GRADE"]])
    dat_keys  <- paste0(dat[["SUBJECT"]], "_",
                        dat[["YEAR_1"]], "_",
                        dat[["YEAR_2"]], "_",
                        dat[["GRADE"]])
    dat[["flagged"]] <- dat_keys %in% flag_keys
  } else {
    dat[["flagged"]] <- FALSE
  }

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x     = .data[["total_transition_rate"]],
      y     = stats::reorder(.data[["cell_id"]], .data[["total_transition_rate"]]),
      color = .data[["flagged"]],
      shape = .data[["flagged"]]
    )
  ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ year_pair, nrow = 1) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#cc0000", "FALSE" = "steelblue"),
      labels = c("TRUE" = "Flagged", "FALSE" = "Normal"),
      name   = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 17L, "FALSE" = 16L),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(x * 100), "%"),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      x     = "Total Transition Rate",
      y     = NULL,
      title = "Total Transition Rates by Grade, Subject, and Year Pair"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}


#' Flag normatively large achievement level transitions
#'
#' Computes z-scores for `total_transition_rate` and `net_upward_rate` across
#' all SUBJECT × GRADE × year-pair cells and flags those beyond `flag_sd`
#' standard deviations. Generates a plain-language annotation for each flag.
#'
#' @param transition_data Named list returned by [compute_transitions()].
#' @param assessment_spec Named list describing the assessment program.
#' @param flag_sd Numeric. Standard deviation threshold for flagging. Default
#'   `1.5`.
#'
#' @return A named list:
#'   * `$table`     — flagged cells with z-scores and annotations
#'   * `$narrative` — markdown summary of flags
#' @export
flag_transitions <- function(transition_data, assessment_spec, flag_sd = 1.5) {
  stopifnot(
    is.list(transition_data),
    !is.null(transition_data$summary_table),
    is.list(assessment_spec)
  )

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  tbl <- transition_data$summary_table |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl  = dplyr::if_else(
        is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
      ),
      pct_up     = dplyr::coalesce(.data[["up"]],     0),
      pct_stable = dplyr::coalesce(.data[["stable"]], 0),
      pct_down   = dplyr::coalesce(.data[["down"]],   0)
    )

  # Z-score each direction rate across all cells
  z_flag <- function(x, sd_floor = 1) {
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    (x - m) / if (s > 0) s else sd_floor
  }

  tbl <- tbl |>
    dplyr::mutate(
      z_up     = z_flag(.data[["pct_up"]]),
      z_stable = z_flag(.data[["pct_stable"]]),
      z_down   = z_flag(.data[["pct_down"]]),
      flag_type = dplyr::case_when(
        .data[["z_up"]]     >=  flag_sd ~ "unusually high upward rate",
        .data[["z_up"]]     <= -flag_sd ~ "unusually low upward rate",
        .data[["z_down"]]   >=  flag_sd ~ "unusually high downward rate",
        .data[["z_down"]]   <= -flag_sd ~ "unusually low downward rate",
        .data[["z_stable"]] <= -flag_sd ~ "unusually low stability rate",
        .data[["z_stable"]] >=  flag_sd ~ "unusually high stability rate",
        TRUE ~ NA_character_
      )
    )

  flagged_summary <- tbl |>
    dplyr::filter(!is.na(.data[["flag_type"]])) |>
    dplyr::mutate(
      annotation = sprintf(
        "%s %s: %s in %s–%s (↑ %.0f%%  → %.0f%%  ↓ %.0f%%).",
        .data[["subj_label"]], .data[["grade_lbl"]], .data[["flag_type"]],
        .data[["YEAR_1"]], .data[["YEAR_2"]],
        .data[["pct_up"]] * 100, .data[["pct_stable"]] * 100,
        .data[["pct_down"]] * 100
      )
    ) |>
    dplyr::select("SUBJECT", "GRADE", "subj_label", "grade_lbl",
                  "YEAR_1", "YEAR_2", "n_total",
                  "pct_up", "pct_stable", "pct_down",
                  "z_up", "z_stable", "z_down",
                  "flag_type", "annotation") |>
    dplyr::arrange(dplyr::desc(
      pmax(abs(.data[["z_up"]]), abs(.data[["z_stable"]]),
           abs(.data[["z_down"]]), na.rm = TRUE)
    ))

  # --- Individual cell-level flags (from_level × to_level proportions) ------
  cell_flags <- data.frame(stringsAsFactors = FALSE)
  if (!is.null(transition_data$long_table) &&
      nrow(transition_data$long_table) > 0L) {

    lt <- transition_data$long_table |>
      dplyr::filter(!is.na(.data[["prop"]])) |>
      dplyr::left_join(subj_lookup, by = "SUBJECT") |>
      dplyr::mutate(
        subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
        grade_lbl  = dplyr::if_else(
          is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
        )
      )

    # Normalize within each from_level × to_level transition type
    cell_flags <- lt |>
      dplyr::group_by(.data[["from_level"]], .data[["to_level"]]) |>
      dplyr::mutate(
        mean_prop = mean(.data[["prop"]], na.rm = TRUE),
        sd_prop   = stats::sd(.data[["prop"]], na.rm = TRUE),
        z_cell    = (.data[["prop"]] - .data[["mean_prop"]]) /
                      dplyr::if_else(!is.na(.data[["sd_prop"]]) &
                                       .data[["sd_prop"]] > 0,
                                     .data[["sd_prop"]], 1)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(abs(.data[["z_cell"]]) >= flag_sd) |>
      dplyr::mutate(
        flag_dir   = dplyr::if_else(.data[["z_cell"]] > 0,
                                     "unusually high", "unusually low"),
        annotation = sprintf(
          "%s %s: %s→%s transition was %s in %s–%s (%.0f%%; typical: %.0f%%).",
          .data[["subj_label"]], .data[["grade_lbl"]],
          as.character(.data[["from_level"]]),
          as.character(.data[["to_level"]]),
          .data[["flag_dir"]],
          .data[["YEAR_1"]], .data[["YEAR_2"]],
          .data[["prop"]] * 100,
          .data[["mean_prop"]] * 100
        )
      ) |>
      dplyr::select("SUBJECT", "GRADE", "subj_label", "grade_lbl",
                    "YEAR_1", "YEAR_2", "from_level", "to_level",
                    "prop", "mean_prop", "z_cell", "flag_dir", "annotation") |>
      dplyr::arrange(dplyr::desc(abs(.data[["z_cell"]])))
  }

  # --- Narrative ------------------------------------------------------------
  has_summary_flags <- nrow(flagged_summary) > 0L
  has_cell_flags    <- nrow(cell_flags) > 0L

  if (!has_summary_flags && !has_cell_flags) {
    return(list(
      table         = flagged_summary,
      table_cells   = cell_flags,
      narrative     = "No achievement transition cells were flagged as normatively unusual."
    ))
  }

  narrative_parts <- character(0)

  if (has_summary_flags) {
    bullets <- paste0("- ", flagged_summary[["annotation"]], collapse = "\n")
    narrative_parts <- c(narrative_parts, paste0(
      "**Direction rate flags** (>±", flag_sd, " SD from typical):\n\n", bullets
    ))
  }

  if (has_cell_flags) {
    bullets <- paste0("- ", cell_flags[["annotation"]], collapse = "\n")
    narrative_parts <- c(narrative_parts, paste0(
      "**Individual transition cell flags** (specific from→to level, >±",
      flag_sd, " SD from typical for that transition):\n\n", bullets
    ))
  }

  list(
    table       = flagged_summary,
    table_cells = cell_flags,
    narrative   = paste(narrative_parts, collapse = "\n\n")
  )
}


#' Compare transition patterns to mean score and proficiency trends
#'
#' Joins transition summary statistics to already-computed score and proficiency
#' trends, computes correlations, and flags divergent cases where transition
#' direction contradicts scale score or proficiency direction.
#'
#' @param transition_data Named list returned by [compute_transitions()].
#' @param score_trends Named list returned by [run_score_trends()].
#' @param proficiency Named list returned by [run_proficiency()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list:
#'   * `$correlations` — Pearson correlations between net_upward_rate and
#'     delta_mean / delta_pct_proficient by SUBJECT
#'   * `$divergences`  — flagged divergent cases with annotations
#'   * `$joined`       — full joined table for downstream plotting
#' @export
compute_transition_comparisons <- function(transition_data,
                                           score_trends,
                                           proficiency,
                                           assessment_spec) {
  stopifnot(
    is.list(transition_data),
    !is.null(transition_data$summary_table),
    is.list(score_trends),
    is.list(proficiency),
    is.list(assessment_spec)
  )

  summary_tbl <- transition_data$summary_table

  # --- Year-over-year delta_mean from the score_trends wide table ---
  score_wide <- score_trends$table
  yrs <- sort(as.integer(sub("_Mean$", "", grep("_Mean$", names(score_wide),
                                                value = TRUE))))
  delta_mean_rows <- lapply(seq_len(length(yrs) - 1L), function(i) {
    y1 <- yrs[i]; y2 <- yrs[i + 1L]
    cols_needed <- c("SUBJECT", "GRADE",
                     paste0(y1, "_Mean"), paste0(y2, "_Mean"))
    if (!all(cols_needed %in% names(score_wide))) return(NULL)
    score_wide[, cols_needed, drop = FALSE] |>
      dplyr::rename(
        mean_y1 = dplyr::all_of(paste0(y1, "_Mean")),
        mean_y2 = dplyr::all_of(paste0(y2, "_Mean"))
      ) |>
      dplyr::mutate(
        YEAR_1     = y1,
        YEAR_2     = y2,
        delta_mean = .data[["mean_y2"]] - .data[["mean_y1"]]
      ) |>
      dplyr::select("SUBJECT", "GRADE", "YEAR_1", "YEAR_2", "delta_mean")
  })
  delta_mean_tbl <- dplyr::bind_rows(delta_mean_rows)

  # --- Year-over-year delta_pct_proficient from proficiency long table ---
  prof_tbl <- proficiency$table
  delta_prof_tbl <- prof_tbl |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]]) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::mutate(
      YEAR_1          = dplyr::lag(.data[["YEAR"]]),
      YEAR_2          = .data[["YEAR"]],
      delta_pct_prof  = .data[["pct_proficient"]] - dplyr::lag(.data[["pct_proficient"]])
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data[["YEAR_1"]])) |>
    dplyr::select("SUBJECT", "GRADE", "YEAR_1", "YEAR_2", "delta_pct_prof")

  # --- Join to transition summary ---
  joined <- summary_tbl |>
    dplyr::left_join(delta_mean_tbl, by = c("SUBJECT", "GRADE", "YEAR_1", "YEAR_2")) |>
    dplyr::left_join(delta_prof_tbl, by = c("SUBJECT", "GRADE", "YEAR_1", "YEAR_2"))

  # --- Correlations by SUBJECT ---
  subjects <- unique(joined[["SUBJECT"]])
  corr_rows <- lapply(subjects, function(s) {
    d <- joined |>
      dplyr::filter(.data[["SUBJECT"]] == s,
                    !is.na(.data[["net_upward_rate"]]))
    n_mean <- sum(!is.na(d[["delta_mean"]]) & !is.na(d[["net_upward_rate"]]))
    n_prof <- sum(!is.na(d[["delta_pct_prof"]]) & !is.na(d[["net_upward_rate"]]))
    data.frame(
      SUBJECT      = s,
      corr_mean    = if (n_mean >= 3L)
                       stats::cor(d[["net_upward_rate"]], d[["delta_mean"]],
                                  use = "complete.obs") else NA_real_,
      corr_prof    = if (n_prof >= 3L)
                       stats::cor(d[["net_upward_rate"]], d[["delta_pct_prof"]],
                                  use = "complete.obs") else NA_real_,
      n_mean       = n_mean,
      n_prof       = n_prof,
      stringsAsFactors = FALSE
    )
  })
  correlations <- dplyr::bind_rows(corr_rows)

  # --- Divergent cases: transition and scale/proficiency direction disagree ---
  divergences <- joined |>
    dplyr::filter(!is.na(.data[["net_upward_rate"]])) |>
    dplyr::mutate(
      dir_net    = dplyr::case_when(
        .data[["net_upward_rate"]] > 0.05  ~ "up",
        .data[["net_upward_rate"]] < -0.05 ~ "down",
        TRUE ~ "flat"
      ),
      dir_mean   = dplyr::case_when(
        dplyr::coalesce(.data[["delta_mean"]], 0) > 1   ~ "up",
        dplyr::coalesce(.data[["delta_mean"]], 0) < -1  ~ "down",
        TRUE ~ "flat"
      ),
      dir_prof   = dplyr::case_when(
        dplyr::coalesce(.data[["delta_pct_prof"]], 0) > 1   ~ "up",
        dplyr::coalesce(.data[["delta_pct_prof"]], 0) < -1  ~ "down",
        TRUE ~ "flat"
      ),
      divergence_flag = dplyr::case_when(
        .data[["dir_net"]] != "flat" & .data[["dir_mean"]] != "flat" &
          .data[["dir_net"]] != .data[["dir_mean"]] ~ "mean_diverges",
        .data[["dir_net"]] != "flat" & .data[["dir_prof"]] != "flat" &
          .data[["dir_net"]] != .data[["dir_prof"]] ~ "prof_diverges",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data[["divergence_flag"]])) |>
    dplyr::mutate(
      grade_lbl  = dplyr::if_else(
        is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
      ),
      subj_label = vapply(.data[["SUBJECT"]], function(s) {
        lbl <- assessment_spec$subjects[[s]]$label
        if (!is.null(lbl)) lbl else s
      }, character(1)),
      annotation = dplyr::case_when(
        .data[["divergence_flag"]] == "mean_diverges" ~ sprintf(
          "Mean scale score %s in %s %s %s–%s, but net transition rate was %s.",
          .data[["dir_mean"]], .data[["subj_label"]], .data[["grade_lbl"]],
          .data[["YEAR_1"]], .data[["YEAR_2"]], .data[["dir_net"]]
        ),
        TRUE ~ sprintf(
          "Percent proficient %s in %s %s %s–%s, but net transition rate was %s.",
          .data[["dir_prof"]], .data[["subj_label"]], .data[["grade_lbl"]],
          .data[["YEAR_1"]], .data[["YEAR_2"]], .data[["dir_net"]]
        )
      )
    ) |>
    dplyr::select("SUBJECT", "GRADE", "subj_label", "grade_lbl",
                  "YEAR_1", "YEAR_2",
                  "net_upward_rate", "delta_mean", "delta_pct_prof",
                  "dir_net", "dir_mean", "dir_prof",
                  "divergence_flag", "annotation")

  list(correlations = correlations, divergences = divergences, joined = joined)
}


#' Plot scatterplots comparing transition rates to score/proficiency changes
#'
#' @param comparisons Named list returned by [compute_transition_comparisons()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list with `$scatter_mean` and `$scatter_prof` ggplot objects.
#' @export
plot_transition_comparisons <- function(comparisons, assessment_spec) {
  stopifnot(is.list(comparisons), !is.null(comparisons$joined))

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  # Pre-compute divergence keys outside mutate — %in% returns a vector but
  # && requires length-1 scalars, causing an error inside dplyr::mutate()
  div_keys <- if (!is.null(comparisons$divergences) &&
                  nrow(comparisons$divergences) > 0L) {
    paste0(comparisons$divergences[["SUBJECT"]],
           comparisons$divergences[["GRADE"]],
           comparisons$divergences[["YEAR_1"]],
           comparisons$divergences[["YEAR_2"]])
  } else {
    character(0)
  }

  dat <- comparisons$joined |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label  = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl   = dplyr::if_else(
        is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
      ),
      pt_label    = paste0(.data[["subj_label"]], "\n", .data[["grade_lbl"]]),
      diverged    = paste0(.data[["SUBJECT"]], .data[["GRADE"]],
                           .data[["YEAR_1"]], .data[["YEAR_2"]]) %in% div_keys
    )

  make_scatter <- function(dat, x_col, x_lab) {
    d <- dat |> dplyr::filter(!is.na(.data[[x_col]]),
                               !is.na(.data[["net_upward_rate"]]))
    if (nrow(d) == 0L) return(NULL)
    ggplot2::ggplot(
      d,
      ggplot2::aes(x = .data[["net_upward_rate"]], y = .data[[x_col]],
                   color = .data[["subj_label"]], label = .data[["grade_lbl"]])
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::geom_point(size = 2.5, alpha = 0.8) +
      ggplot2::geom_text(size = 2.2, vjust = -0.6, show.legend = FALSE) +
      ggplot2::facet_wrap(~ year_pair, nrow = 1) +
      ggplot2::scale_x_continuous(
        labels = function(x) paste0(round(x * 100), "pp")
      ) +
      ggplot2::labs(
        x     = "Net Upward Transition Rate",
        y     = x_lab,
        color = "Subject"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
  }

  dat <- dat |>
    dplyr::mutate(
      year_pair = paste0(.data[["YEAR_1"]], "→", .data[["YEAR_2"]])
    )

  # Change in % Proficient vs. Change in Mean Scale Score (no transition axis)
  scatter_prof_vs_mean <- {
    d <- dat |>
      dplyr::filter(!is.na(.data[["delta_mean"]]),
                    !is.na(.data[["delta_pct_prof"]]))
    if (nrow(d) == 0L) {
      NULL
    } else {
      ggplot2::ggplot(
        d,
        ggplot2::aes(x = .data[["delta_mean"]], y = .data[["delta_pct_prof"]],
                     color = .data[["subj_label"]], label = .data[["grade_lbl"]])
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        ggplot2::geom_point(size = 2.5, alpha = 0.8) +
        ggplot2::geom_text(size = 2.2, vjust = -0.6, show.legend = FALSE) +
        ggplot2::facet_wrap(~ year_pair, nrow = 1) +
        ggplot2::labs(
          x     = "Year-over-Year Change in Mean Scale Score",
          y     = "Year-over-Year Change in % Proficient",
          color = "Subject"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
    }
  }

  list(
    scatter_mean         = make_scatter(dat, "delta_mean",     "Year-over-Year Change in Mean Scale Score"),
    scatter_prof         = make_scatter(dat, "delta_pct_prof", "Year-over-Year Change in % Proficient"),
    scatter_prof_vs_mean = scatter_prof_vs_mean
  )
}


#' Summarize achievement transition findings in plain text
#'
#' @param transition_data Named list returned by [compute_transitions()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list with `$narrative` (markdown character string).
#' @export
summarize_transition_findings <- function(transition_data, assessment_spec,
                                           flags = NULL) {
  stopifnot(
    is.list(transition_data),
    is.list(assessment_spec)
  )

  if (is.null(transition_data$summary_table) ||
      nrow(transition_data$summary_table) == 0L) {
    return(list(narrative = "No transition data available to summarize."))
  }

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  # Build a lookup of flagged summary cells for ⚠ indicators
  flag_keys <- character(0)
  if (!is.null(flags) && !is.null(flags$table) && nrow(flags$table) > 0L) {
    flag_keys <- paste0(flags$table[["SUBJECT"]], "_",
                        flags$table[["GRADE"]], "_",
                        flags$table[["YEAR_1"]], "_",
                        flags$table[["YEAR_2"]])
  }

  tbl <- transition_data$summary_table |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl  = dplyr::if_else(
        is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
      ),
      pct_up     = dplyr::coalesce(.data[["up"]],     0),
      pct_stable = dplyr::coalesce(.data[["stable"]], 0),
      pct_down   = dplyr::coalesce(.data[["down"]],   0)
    )

  subjects <- unique(tbl[["SUBJECT"]])
  by_subject <- vapply(subjects, function(s) {
    d_s <- tbl |> dplyr::filter(.data[["SUBJECT"]] == s)
    lbl <- unique(d_s[["subj_label"]])[1L]

    grade_bullets <- vapply(sort(unique(d_s[["GRADE"]])), function(g) {
      d_g <- d_s |>
        dplyr::filter(
          if (is.na(g)) is.na(.data[["GRADE"]]) else .data[["GRADE"]] == g
        )
      g_lbl <- if (is.na(g)) "EOC" else paste0("Grade ", g)

      yr_lines <- vapply(seq_len(nrow(d_g)), function(i) {
        r      <- d_g[i, ]
        cell_k <- paste0(r[["SUBJECT"]], "_", r[["GRADE"]], "_",
                         r[["YEAR_1"]], "_", r[["YEAR_2"]])
        flag_indicator <- if (cell_k %in% flag_keys) " (flagged)" else ""
        sprintf(
          "%s to %s%s: Up %.0f%%, Stable %.0f%%, Down %.0f%%",
          r[["YEAR_1"]], r[["YEAR_2"]], flag_indicator,
          r[["pct_up"]] * 100, r[["pct_stable"]] * 100,
          r[["pct_down"]] * 100
        )
      }, character(1))

      paste0("* ", g_lbl, ": ", paste(yr_lines, collapse = "; "))
    }, character(1))

    paste0("**", lbl, "**\n\n", paste(grade_bullets, collapse = "\n"))
  }, character(1))

  narrative <- paste0(
    "Achievement level transition summary ",
    "(Up = moved to a higher level, Stable = stayed at same level, ",
    "Down = moved to a lower level; flagged = normatively unusual):\n\n",
    paste(by_subject, collapse = "\n\n")
  )

  list(narrative = narrative)
}


#' Run the Line 1 achievement transitions sub-analysis
#'
#' Computes transition matrices, flags normatively large transitions, compares
#' results to scale score and proficiency trends, and produces plots and
#' narrative text.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param score_trends Named list returned by [run_score_trends()].
#' @param proficiency Named list returned by [run_proficiency()].
#' @param min_n Integer. Minimum cell size for suppression. Default `10`.
#' @param flag_sd Numeric. SD threshold for normative flagging. Default `1.5`.
#'
#' @return A named list:
#'   * `$matrices`     — transition data and plots
#'   * `$flags`        — normative flag table, plot, and narrative
#'   * `$comparisons`  — correlations, divergence flags, and scatter plots
#'   * `$text`         — overall narrative
#' @export
run_achievement_transitions <- function(student_results_long,
                                        assessment_spec,
                                        score_trends,
                                        proficiency,
                                        min_n   = 10,
                                        flag_sd = 1.5) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    is.list(score_trends),
    is.list(proficiency)
  )

  trans_data <- compute_transitions(student_results_long, assessment_spec,
                                     min_n = min_n)

  if (is.null(trans_data$summary_table)) {
    no_data_msg <- paste(
      "Achievement transition analysis could not be completed.",
      "This sub-analysis requires at least two years of data with",
      "matched student records (same STUDENT_ID at consecutive grades).",
      "Only one effective year of data was found after filtering."
    )
    return(list(
      matrices = list(
        long_table    = NULL,
        summary_table = NULL,
        avg_matrices  = NULL,
        plot          = list(heatmap = NULL, alluvial = NULL, rates = NULL)
      ),
      flags = list(
        table       = NULL,
        table_cells = NULL,
        narrative   = no_data_msg,
        plot        = NULL
      ),
      comparisons = list(
        correlations = NULL,
        divergences  = NULL,
        joined       = NULL,
        plot         = list(scatter_mean = NULL, scatter_prof = NULL,
                            scatter_prof_vs_mean = NULL)
      ),
      text = list(narrative = no_data_msg)
    ))
  }

  flags        <- flag_transitions(trans_data, assessment_spec,
                                    flag_sd = flag_sd)
  comparisons  <- compute_transition_comparisons(trans_data, score_trends,
                                                  proficiency, assessment_spec)
  heatmaps     <- plot_transition_heatmaps(trans_data, assessment_spec)
  alluvials    <- plot_transition_alluvial(trans_data, assessment_spec)
  rates_plot   <- plot_transition_rates(trans_data, assessment_spec,
                                         flag_table = flags$table)
  scatter_plots <- plot_transition_comparisons(comparisons, assessment_spec)
  text          <- summarize_transition_findings(trans_data, assessment_spec,
                                                    flags = flags)

  list(
    matrices = list(
      long_table    = trans_data$long_table,
      summary_table = trans_data$summary_table,
      avg_matrices  = trans_data$avg_matrices,
      plot = list(
        heatmap  = heatmaps,
        alluvial = alluvials,
        rates    = rates_plot
      )
    ),
    flags = list(
      table       = flags$table,
      table_cells = flags$table_cells,
      narrative   = flags$narrative,
      plot        = rates_plot
    ),
    comparisons = list(
      correlations = comparisons$correlations,
      divergences  = comparisons$divergences,
      joined       = comparisons$joined,
      plot = list(
        scatter_mean         = scatter_plots$scatter_mean,
        scatter_prof         = scatter_plots$scatter_prof,
        scatter_prof_vs_mean = scatter_plots$scatter_prof_vs_mean
      )
    ),
    text = text
  )
}
