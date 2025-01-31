rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
source('source/export/survey_experts/00_definitions.R')
#
#
#
# --- prepare response data ---------------
#
# response data
load(paste0(response_data_path, "results_combined.rda"))
#
# add variables
res_comb_upd <- res_comb |>
  dplyr::mutate(
    question_text_short = dplyr::case_match(
      question_id,
      "A1" ~ "Hoeveel introductieplaatsen?",
      "A2" ~ "Introductieplaatsen toegankelijk?",
      "A4" ~ "Bijzondere introductieplaatsen?",
      "A5" ~ "Kans op introductie?",
      "A6" ~ "Kans op vestiging?",
      "B1" ~ "Verspreiding gekend?",
      "B2" ~ "Verspreidingspatroon?",
      "B3" ~ "Welke populatiedichtheid?",
      "B4" ~ "Verandering verspreidingsgebieden?",
      "B5" ~ "Verspreidingsgebieden toegankelijk?",
      "B7" ~ "Bijzondere verspreidingsgebieden?",
      "C1" ~ "Impact biodiversiteit?",
      "C2" ~ "Impact biodiversiteit natuurgebieden?",
      "C3" ~ "Impact andere sectoren?",
      "D1" ~ "Welke bemonsteringsmethoden?",
      "D2" ~ "Meest relevante methode?",
      "D3" ~ "Sensitiviteit methode?",
      "D4" ~ "Specificiteit methode?",
      "D5" ~ "Kosten methode?",
      "D6" ~ "Scope methode?",
      "D7" ~ "Veldprotocol beschikbaar?",
      "D8" ~ "Relevante meetnetten?",
      "D9" ~ "Door meetnetten opgepikt?",
      "D10" ~ "Losse waarnemingen representatief?",
      "E1" ~ "Soort beheerd?",
      "E2" ~ "Informatie beheersevaluatie",
    ),
    question_text_trunc = stringr::str_trunc(string = question_text, width = 40),
    .after = question_text
  )
# dplyr::rowwise() |>
# dplyr::mutate(
#  question_text_wrap = question_text |>
#    stringr::str_wrap(width = 40) |>
#    paste(x = _, NULL, collapse = "\n")
#)


#
# define subsets of response data
res_scored <- res_comb_upd |>
  dplyr::filter(question_scored |> as.logical(), !section_skipped)
res_open <- res_comb_upd |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped)
#
#
#
# --- summarize responses (functions) ---------------
#
#
# mean function
sum_mean <- function(
    res_scored,
    crit,
    group_by_what = "species"
){
  res_mean <- res_scored |>
    dplyr::filter(
      grepl(crit, score_crit)
    ) |>
    # mean per group
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_by_what))) |>
    dplyr::mutate(m = mean(response_score, na.rm = TRUE)) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    # mean of means
    dplyr::mutate(m = mean(m), .by = species) |>
    dplyr::distinct(species, .keep_all = TRUE) |>
    #
    dplyr::select(tidyselect::starts_with(c("species","stadium","m","on_union", "ven"))) |>
    dplyr::arrange(dplyr::desc(m)) |>
    dplyr::rename_with(~ paste0("m_", crit), "m")
}
#
# function to factorize variables
factorize <- function(
    res,
    varnames,
    varlevels
) {
  for (i in seq_along(varnames)) {
    res  <- res  |>
      dplyr::mutate(
        !!varnames[i] := factor(get(varnames[i]), levels = varlevels[[i]])
      )
  }
  return(res)
}
#
#
#
# --- plot ranking according to mean scores ---------------
#
# grand means urgency & feasibility
res_m_feas <- sum_mean(res_scored, "feas")
res_m_urge <- sum_mean(res_scored, "urge")
# grand mean all questions
res_m_feasurge <- sum_mean(res_scored, "feas|urge")
# section group means urgency & feasibility
res_gm_feas <- sum_mean(res_scored, "feas", group_by_what = c("species", "section_no"))
res_gm_urge <- sum_mean(res_scored, "urge", group_by_what = c("species", "section_no"))

# merge data
res_m <- dplyr::full_join(
  x = res_m_feas,
  y = res_m_urge
  ) |>
  dplyr::full_join(
    x = _,
    y = res_m_feasurge |> dplyr::rename(m_feasurge = "m_feas|urge")
    ) |>
  dplyr::full_join(
    x = _,
    y = res_gm_feas |> dplyr::rename(gm_feas = "m_feas")
  ) |>
  dplyr::full_join(
    x = _,
    y = res_gm_urge |> dplyr::rename(gm_urge = "m_urge")
  ) |>
  dplyr::mutate(
    gm_mfeas_murge = mean(c(m_feas, m_urge)),
    .by = species
  ) |>
  dplyr::mutate(
    gm_gmfeas_gmurge = mean(c(gm_feas, gm_urge)),
    .by = species
  ) |>
  dplyr::arrange(dplyr::desc(m_feasurge))
#
# correlations of different means
cor(res_m |> dplyr::select(tidyselect::contains(c("m_"))))
#
#
# convert to plot data
res_plot_tmp <- res_m |>
  dplyr::mutate(
    m_max = dplyr::case_when(
      m_feas > m_urge ~ m_feas,
      TRUE ~ m_urge
  ),
  on_unionlist_upd = dplyr::case_when(
    on_unionlist ~ "on unionlist",
    !on_unionlist ~ "not on unionlist"
  )
  )
res_plot <- factorize(
  res_plot_tmp,
  c("species", "ven_name_eng", "ven_name_nld", "on_unionlist_upd"),
  list(
    res_m$species |> unique() |> rev(),
    res_m$ven_name_eng |> unique() |> rev(),
    res_m$ven_name_nld |> unique() |> rev(),
    c("on unionlist", "not on unionlist")
  )
)
#
# plot
ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = m_feasurge, y = ven_name_nld, color = on_unionlist_upd)) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = 0, xmax = m_max), linetype = "dashed") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = 0, xmax = m_feasurge)) +
  ggplot2::geom_point(ggplot2::aes(x = m_feas), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = m_feas), shape = "F") +
  ggplot2::geom_point(ggplot2::aes(x = m_urge), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = m_urge), shape = "U") +
  ggplot2::geom_point() +
  ggplot2::coord_cartesian(xlim = c(
    res_scored$response_score |> na.omit() |> min(),
    res_scored$response_score |> na.omit() |> max()
    )) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::labs(
    x = "mean of feasibilty and urgency scores (grand mean)",
    y = "species (vernacular name nld)"
    )
# closer to F if species is present as F then has more questions
# closer to U if species is absent as U then has more questions
#
#
#
# --- plot score categories 'unknown' vs. rest ---------------
#
# convert to plot data
res_plot_tmp <- res_scored |>
  dplyr::mutate(
    score_category = dplyr::case_when(
      !grepl("ongekend|ik weet het niet", response_text) ~ "rest",
      TRUE ~ response_text |> gsub(" ", "", x = _)
    )
  ) |>
  # score category proportions per species
  dplyr::mutate(
    n_scores_category = dplyr::n(),
    .by = c(species, score_category)
  ) |>
  dplyr::mutate(
    n_scores_tot = dplyr::n(),
    .by = species
  ) |>
  dplyr::mutate(prop_scores = n_scores_category / n_scores_tot) |>
  tidyr::pivot_wider(
    data = _,
    names_from = score_category,
    values_from = prop_scores,
    names_prefix = "prop_scores_"
  ) |>
  dplyr::group_by(species) |>
  tidyr::fill(tidyselect::contains("prop_scores"), .direction = "downup") |>
  dplyr::ungroup() |>
  dplyr::mutate(dplyr::across(tidyselect::contains("prop_scores"), \(x) tidyr::replace_na(x, 0)))

# check whether proportions sum to 1
test <- res_plot_tmp |>
  dplyr::rowwise() |>
  dplyr::mutate(prop_check = rowSums(dplyr::across(tidyselect::starts_with("prop_scores"))))
assertthat::are_equal(sum(test$prop_check), nrow(test))
#
# convert character vars to factors for ordering
res_plot <- factorize(
  res_plot_tmp,
  c("question_text", "question_text_trunc", "section_title", "species", "ven_name_nld"),
  list(
    res_plot_tmp$question_text |> unique(),
    res_plot_tmp$question_text_trunc |> unique(),
    res_plot_tmp$section_title |> unique() |> _[match(
        seq(min(res_plot_tmp$section_no), max(res_plot_tmp$section_no)),
        res_plot_tmp$section_no |> unique()
      )],
    res_m$species |> unique() |> rev(),
    res_m$ven_name_nld |> unique() |> rev()
  )
)
#
# plot
ggplot2::ggplot(res_plot, ggplot2::aes(y = ven_name_nld)) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin = 0,
      xmax = prop_scores_ongekend + prop_scores_ikweethetniet,
      color = "ongekend / ik weet het niet"
      ),
    linewidth = 2
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin = prop_scores_ongekend + prop_scores_ikweethetniet,
      xmax = prop_scores_ongekend + prop_scores_ikweethetniet + prop_scores_rest,
      color = "rest"
      ),
    linewidth = 2
  ) +
  ggplot2::scale_color_manual(values = c("#EA5F94", "lightgrey")) +
  ggplot2::coord_cartesian(xlim = c(0,1)) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    x = "proportion responses",
    y = "species (vernacular name nld)",
    color = "category response"
  )
#
#
res_plot_upd <- res_plot |>
  dplyr::mutate(
    score_category = dplyr::case_when(
      !grepl("ongekend|ik weet het niet", response_text) ~ "rest",
      TRUE ~ "ongekend / ik weet het niet"
      )
  )

ggplot2::ggplot(
  res_plot_upd,
  ggplot2::aes(
    x = question_text_short, y = ven_name_nld,
    fill = score_category, color = score_category
  )
) +
  ggplot2::geom_point(size = 3, shape = 21) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(section_title),
    scales = "free",
    space = "free"
  ) +
  ggplot2::scale_color_manual(values = c("#EA5F94", "lightgrey")) +
  ggplot2::scale_fill_manual(values = c("#EA5F94", "lightgrey")) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "proportion responses",
    y = "species (vernacular name nld)",
    color = "category response",
    fill = "category response"
  )
#
#
#
# --- plot reported vs. prius stadium ---------------
#
res_plot_tmp <- res_comb_upd |>
  dplyr::distinct(species, .keep_all = TRUE) |>
  dplyr::mutate(
    prius_stadium_upd = dplyr::case_when(
      grepl("IRR", prius_stadium) ~ "irrelevant",
      grepl("AFW", prius_stadium) ~ "afwezig",
      grepl("SPO", prius_stadium) ~ "sporadisch aanwezig",
      grepl("BEP", prius_stadium) ~ "beperkt gevestigd",
      grepl("VER", prius_stadium) ~ "wijdverspreid"
    ),
    on_unionlist_upd = dplyr::case_when(
      on_unionlist ~ "on unionlist",
      !on_unionlist ~ "not on unionlist"
    ),
    # stadium labels
    label_helper = paste(prius_stadium_upd, stadium, sep ="_")
  ) |>
  dplyr::group_by(label_helper) |>
  dplyr::mutate(
    stadium_label = dplyr::case_when(
      (prius_stadium_upd != stadium & dplyr::row_number() == 1) ~ paste(ven_name_nld, collapse = "\n"),
      TRUE ~ NA_character_
    )) |>
  dplyr::ungroup()
varlevels = c("irrelevant", "afwezig", "sporadisch aanwezig", "beperkt gevestigd", "wijdverspreid")
res_plot <- factorize(
  res_plot_tmp,
  c("stadium", "prius_stadium_upd", "on_unionlist_upd"),
  list(
    varlevels,
    varlevels,
    c("on unionlist", "not on unionlist")
  )
)
#
# plot
ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = prius_stadium_upd, y = stadium)) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "lightgrey", linewidth = 2) +
  #ggplot2::geom_point(size = 12, shape = 21, fill = "white", color = "lightgrey") +
  ggplot2::geom_point(
    size = 3,
    position = ggplot2::position_jitter(width = 0.1, height = 0.1),
    alpha = .4
    ) +
  ggforce::geom_mark_circle(
    ggplot2::aes(
    label = stadium_label,
    filter = !is.na(stadium_label)),
    color = "#EA5F94",
    label.fontface = "plain", label.fontsize = 10, label.colour = "#EA5F94",
    con.type = "straight", con.colour = "#EA5F94"
    ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(on_unionlist_upd),
    scales = "free",
    space = "free"
  ) +
  ggplot2::scale_x_discrete(drop = FALSE) +
  ggplot2::scale_y_discrete(drop = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "prius stadium",
    y = "reported stadium"
  )
#
#
#
# --- plot scoring questions ---------------
#
#
#
# --- plot score distributions over closed questions ---------------
#
# convert to plot data
res_plot_tmp <- res_scored |>
  dplyr::mutate(
    question_no = dplyr::row_number(),
    .by = c(species, section_no)
  )
#
# convert character vars to factors for ordering
res_plot <- factorize(
  res_plot_tmp,
  c("question_text", "question_text_short", "section_title"),
  list(
    res_plot_tmp$question_text |> unique(),
    res_plot_tmp$question_text_short |> unique(),
    res_plot_tmp$section_title |> unique() |> _[match(
      seq(min(res_plot_tmp$section_no), max(res_plot_tmp$section_no)),
      res_plot_tmp$section_no |> unique()
    )]
  )
)
#
# plot
ggplot2::ggplot(res_plot, ggplot2::aes(response_score)) +
  ggplot2::geom_bar(ggplot2::aes(fill = score_crit)) +
  ggtext::geom_textbox(
    ggplot2::aes(x = 0.5, y = Inf, label = question_text_short),
    hjust = 0, vjust = -0.2, size = 2.5,
    box.padding = grid::unit(c(0, 0, 0, 0), "pt"),
    box.colour = "transparent", fill = "transparent",
    width = grid::unit(100, "pt")
    ) +
  ggh4x::facet_grid2(
    rows = ggplot2::vars(section_title),
    cols = ggplot2::vars(question_no),
    scales = "free",
    independent = "x",
    render_empty = FALSE,
    switch = "y"
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text.x.top = ggplot2::element_blank(),
    strip.placement = "outside",
    panel.spacing.y = grid::unit(1.5, "lines"),
    legend.position = "bottom",
    legend.title = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "response score",
    y = "frequency"
  )
#
#
#
# --- plot word clouds over open questions ---------------
#
