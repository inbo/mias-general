# --- load-data-and-functions ---------------

# rm(list = ls())

# language
if (!exists("lang")) {
  lang <- "EN"
}
if (!exists("functions_path")) {
  functions_path <- "source/functions/"
}
if (!exists("response_data_path")) {
  response_data_path <- "data/survey_experts/"
}
if (!exists("questions_path")) {
  questions_path <- paste0("source/export/survey_experts/", "questions_", lang, "/")
}


list.files(functions_path, full.names = TRUE) |>
  lapply(source) |>
  invisible()


# # response data
# current version: 17-02-2025
load(paste0(response_data_path, "results_combined_upd.rda"))


# --- prepare response data --------------------------------------------
#
#
# define subsets of response data

res_scored_allspec <- res_comb_upd |>
  dplyr::filter(question_scored |> as.logical(), !section_skipped) #|>
  #dplyr::filter(!grepl("IRR", prius_stadium))
res_scored <- res_scored_allspec |>
  dplyr::filter(on_unionlist)
res_open <- res_comb_upd |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
  dplyr::filter(on_unionlist, !grepl("IRR", prius_stadium))

# --- setup functions --------------------------------------------------
#
summarize <- function(
    res,
    crit,
    group_by_what = "species",
    fun = "mean"
){
  res_mean <- res |>
    dplyr::filter(
      grepl(crit, score_crit)
    ) |>
    # fun output per group
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_by_what))) |>
    dplyr::mutate(m = do.call(fun, list(response_score, na.rm = TRUE))) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    # fun output of fun output
    dplyr::mutate(m = do.call(fun, list(m)), .by = species) |>
    dplyr::distinct(species, .keep_all = TRUE) |>
    #
    dplyr::select(tidyselect::starts_with(c("species","stadium","m","on_union", "vern"))) |>
    dplyr::arrange(dplyr::desc(m)) |>
    dplyr::rename_with(~ paste0("m_", crit), "m")
}
#
#
# function to highlight labels
highlight_labs <- function(
    labs,
    pattern,
    color = "black"
){
  tmpdata <- data.frame(labs = labs)|>
    dplyr::mutate(
      labs_hl := dplyr::case_when(
        grepl(pattern, labs) ~ glue::glue(
          "<span style = 'color:{color}'>**{labs}**</span>"),
        TRUE ~ labs
      )
    ) |>
    dplyr::pull(labs_hl)
}
#
#
#
# --- calculate mean scores and rank -----------------------------------
#
# grand means urgency & feasibility
res_m_feas <- summarize(res_scored_allspec, "feas")
res_m_urge <- summarize(res_scored_allspec, "urge")
# grand mean all questions
res_m_feasurge <- summarize(res_scored_allspec, "feas|urge")
# section group means urgency & feasibility
res_gm_feas <- summarize(res_scored_allspec, "feas", group_by_what = c("species", "section_no"))
res_gm_urge <- summarize(res_scored_allspec, "urge", group_by_what = c("species", "section_no"))
# grand median all questions
res_med_feasurge <- summarize(res_scored_allspec, "feas|urge", fun = "median")

# merge data
res_m_allspec <- dplyr::full_join(
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
  # group mean m_feas m_urge
  dplyr::mutate(
    gm_mfeas_murge = mean(c(m_feas, m_urge)),
    .by = species
  ) |>
  # group mean gm_feas gm_urge
  dplyr::mutate(
    gm_gmfeas_gmurge = mean(c(gm_feas, gm_urge)),
    .by = species
  ) |>
  dplyr::arrange(dplyr::desc(m_feas), dplyr::desc(m_urge))#gm_mfeas_murge))
#
# correlations of different means
cor(res_m_allspec |> dplyr::select(tidyselect::starts_with(c("m_", "gm_"))))
#
# keep only species on unionlist
res_m <- res_m_allspec |>
  dplyr::filter(on_unionlist)
#
#
#
# --- define common plot parameters ------------------------------------
#
#
# order of factor levels
levels_species_allspec <- res_m_allspec$species |> unique() |> rev()
levels_species <- res_m$species |> unique() |> rev()
levels_vern_name_eng_allspec <- res_m_allspec$vern_name_eng |> unique() |> rev()
levels_vern_name_eng <- res_m$vern_name_eng |> unique() |> rev()
levels_vern_name_nld_allspec <- res_m_allspec$vern_name_nld |> unique() |> rev()
levels_vern_name_nld <- res_m$vern_name_nld |> unique() |> rev()
levels_stadium <- c(
  "irrelevant", "absent", "sporadically present",
  "established to limited extend", "widespread"
)
levels_milieu <- c(
  "freshwater",
  "freshwater, brackishwater",
  "freshwater, brackishwater, marine",
  "freshwater, terrestrial",
  "terrestrial",
  "terrestrial, brackishwater",
  "terrestrial, freshwater, brackishwater",
  "marine",
  "brackishwater, marine"
)
#
# colors stadium (cols4all - palette: met.lakota)
cols_stadium <- data.frame(
  cols = c(
    INBOtheme::inbo_groen,
    INBOtheme::inbo_lichtgrijs,
    INBOtheme::inbo_oranje,
    INBOtheme::inbo_hoofd,
    INBOtheme::inbo_steun_donkerroos
    ),
  #cols = c("#247D3F", "#04A3BD", "#F0BE3D", "#DA7901", "#931E18"),
  stadium = levels_stadium
)
#
# colors feasibility urgency
cols_feasurge <- c("#04A3BD", "#DA7901")
#
# color to highlight data
col_hl <- INBOtheme::inbo_hoofd
#
# color for background data
col_bg <- "#E5E4E2"
#
# colors method categories (cols4all - 10 cols - palette: met.redon)
#cols_meth <- c("#5B859E","#1E395F","#75884B","#1E5A46","#DF8D71","#AF4F2F","#D48F90","#732F30","#AB84A5","#59385C")
cols_meth <- INBOtheme::inbo_palette()
#
# color to highlight axis labels
col_labs <- "#71797E" #"#EA5F94"
#
#
# black
col_black <- "#000000"
#
#
#
# --- plotting - species ranking ---------------------------------------------------

# convert to plot data
res_plot_tmp <- res_m_allspec |>
  dplyr::mutate(
    m_max = dplyr::case_when(
      m_feas > m_urge ~ m_feas,
      TRUE ~ m_urge
    ),
    m_min = dplyr::case_when(
      m_feas < m_urge ~ m_feas,
      TRUE ~ m_urge
    ),
    on_unionlist_upd = dplyr::case_when(
      on_unionlist ~ "species on unionlist",
      !on_unionlist ~ "species not on unionlist"
    ),
    part = dplyr::case_when(
      dplyr::row_number() < (dplyr::n()/2) ~ "part 1",
      TRUE ~ "part 2"
    ),
    y_axis_num_helper = dplyr::n() - dplyr::row_number() + 1
  ) |>
  tidyr::pivot_longer(
    cols = tidyselect::all_of(c("m_urge", "m_feas")),
    names_to = "indicator",
    values_to = "indicator_m"
  ) |>
  dplyr::mutate(
    indicator = dplyr::case_match(
      indicator,
      "m_urge" ~ "urgency",
      "m_feas" ~ "feasibility"
    ),
    indicator_cutoff = median(indicator_m),
    .by = indicator,
  ) |>
  dplyr::mutate(
    above_indicator_cutoff = dplyr::case_when(
      sum(indicator_m > indicator_cutoff) == 2 ~ TRUE,
      sum(indicator_m > indicator_cutoff) < 2 ~ FALSE
    ),
    .by = species
  )

res_plot <- factorize(
  dataframe = res_plot_tmp,
  varnames = c("species", "vern_name_eng", "vern_name_nld",
               "on_unionlist_upd", "stadium"),
  varlevels = list(
    levels_species_allspec,
    levels_vern_name_eng_allspec |> rev(),
    levels_vern_name_nld_allspec,
    c("species on unionlist", "species not on unionlist"),
    levels_stadium
  )
)

cutoff_feas <- res_plot_tmp |>
  dplyr::filter(grepl("feas", indicator))|>
  dplyr::pull(indicator_cutoff) |>
  unique()
cutoff_urge <- res_plot_tmp |>
  dplyr::filter(grepl("urge", indicator))|>
  dplyr::pull(indicator_cutoff) |>
  unique()
#
# plot
plot_ranking <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = indicator_m, y = y_axis_num_helper, color = stadium, shape = interaction(indicator, on_unionlist_upd))) +
  ggplot2::geom_vline(data = res_plot |> dplyr::filter(indicator == "urgency"),
                      ggplot2::aes(xintercept = indicator_cutoff),
                      linetype = "longdash",
                      color = col_black) +
  ggplot2::geom_vline(data = res_plot |> dplyr::filter(indicator == "feasibility"),
                      ggplot2::aes(xintercept = indicator_cutoff),
                      linetype = "solid",
                      color = col_black) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = m_max, xmax = 5), linetype = "dotted") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = m_min, xmax = m_max), linewidth = 0.7) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = m_min, xmax = m_max, linetype = above_indicator_cutoff), linewidth = 0.7, color = "white") +
  ggplot2::geom_point(size = 2.5, fill = "white") +
  # annotation
  ggplot2::annotate(
    "text", x = cutoff_feas + 0.1, y = 0, hjust = 0, size = 3, angle = 90,
    label = "critical value feasibility" |> stringr::str_wrap(width = 20),
    color = col_black
    ) +
  ggplot2::annotate(
    "text", x = cutoff_urge + 0.1, y = 0, hjust = 0, size = 3, angle = 90,
    label = "critical value urgency" |> stringr::str_wrap(width = 20),
    color = col_black
  ) +
  # scales
  ggplot2::scale_shape_manual(
    values = c(17, 16, 24, 21),
    labels = function(x) gsub("\\.", ", ", x) |> stringr::str_wrap(string = _, width = 20))+
  ggplot2::scale_color_manual(
    values = cols_stadium |>
      dplyr::filter(stadium %in% (res_plot$stadium |> unique())) |>
      dplyr::pull(cols),
    labels = function(x) stringr::str_wrap(x, width = 20)
  ) +
  ggplot2::scale_linetype_manual(values = c("dotted", NA), guide = "none") +
  ggplot2::scale_y_continuous(
    position = "right",
    breaks = res_plot$y_axis_num_helper |> unique(),
    labels = res_plot$vern_name_eng |> levels(),
    sec.axis = ggplot2::dup_axis(name = "Species (English vernular name)", labels = NULL, breaks = NULL),
    expand = c(0, 1)
    ) +
  ggplot2::coord_cartesian(
    xlim = c(
    res_scored_allspec$response_score |> na.omit() |> min(),
    res_scored_allspec$response_score |> na.omit() |> max()
   ),
   ylim = c(
     res_plot$y_axis_num_helper |> min(),
     res_plot$y_axis_num_helper |> max()
   )
   ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "inside",
    legend.justification = c(0, 1),
    legend.key = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(margin = ggplot2::margin(b = 5, unit = "pt")),
    axis.text.y = ggtext::element_markdown(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "Overall feasibilty/urgency scores",
    y = NULL
  )#
#plot_ranking
#plot_ranking |> plotly::ggplotly()

# --- plotting - invasion stadium vs. prius stadium ---------------------------------------------------

#
res_plot_tmp <- res_comb_upd |>
  dplyr::distinct(species, .keep_all = TRUE) |>
  dplyr::mutate(
    prius_stadium_upd = dplyr::case_when(
      grepl("IRR", prius_stadium) ~ "irrelevant",
      grepl("AFW", prius_stadium) ~ "absent",
      grepl("SPO", prius_stadium) ~ "sporadically present",
      grepl("BEP", prius_stadium) ~ "established to limited extend",
      grepl("VER", prius_stadium) ~ "widespread"
    ),
    on_unionlist_upd = dplyr::case_when(
      on_unionlist ~ "on unionlist",
      !on_unionlist ~ "not on unionlist"
    ),
    # stadium labels
    label_helper = paste(prius_stadium_upd, stadium, sep ="_")
  ) |>
  dplyr::group_by(label_helper, on_unionlist_upd) |>
  dplyr::mutate(
    stadium_label = dplyr::case_when(
      (prius_stadium_upd != stadium & dplyr::row_number() == 1) ~ paste(vern_name_eng, collapse = "\n"),
      TRUE ~ NA_character_
    )) |>
  dplyr::ungroup()
res_plot <- factorize(
  res_plot_tmp,
  c("stadium", "prius_stadium_upd", "on_unionlist_upd"),
  list(
    levels_stadium,
    levels_stadium,
    c("on unionlist", "not on unionlist")
  )
)
#
# plot
plot_stadium <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = prius_stadium_upd, y = stadium)) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = col_bg, linewidth = 2) +
  #ggplot2::geom_point(size = 12, shape = 21, fill = "white", color = col_bg) +
  ggplot2::geom_point(
    size = 3,
    position = ggplot2::position_jitter(width = 0.1, height = 0.1),
    alpha = .4,
    color = col_black
  ) +
  ggforce::geom_mark_circle(
    ggplot2::aes(
      label = stadium_label,
      filter = !is.na(stadium_label)),
    color = col_hl,
    label.fontface = "plain", label.fontsize = 10, label.colour = col_hl,
    con.type = "straight", con.colour = col_hl
  ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(on_unionlist_upd),
    scales = "free",
    space = "free"
  ) +
  ggplot2::scale_x_discrete(drop = FALSE) +
  ggplot2::scale_y_discrete(drop = FALSE, labels = levels(res_plot$stadium) |> gsub("irrelevant", "", x = _)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "Invasion stage assigned in PrIUS",
    y = "Invasion stage reported by expert"
  )

#plot_stadium
#plot_stadium |> plotly::ggplotly()
#
#
#
# --- plotting - invasion stadium vs. species distribution ---------------------------------------------------
#
res_plot_tmp1 <- res_scored_allspec |>
  dplyr::filter(grepl("B1", question_id)) |>
  # adapt response text
  dplyr::mutate(
    response_text_recoded = dplyr::case_when(
      grepl("I do not know", response_text) ~ "do not know",
      grepl("distribution is not sufficiently known", response_text) ~ "unknown",
      grepl("distribution map is not representative", response_text) ~ "known, but map not representative",
      grepl("distribution map is representative", response_text) ~ "known and map representative"
    )
  ) |>
  dplyr::arrange(response_score |> dplyr::desc())
res_plot_tmp2  <- factorize(
  res_plot_tmp1,
  c("stadium", "response_text_recoded"),
  list(
    levels_stadium,
    res_plot_tmp1$response_text_recoded |> unique() |> rev()
  )
) |>
  dplyr::arrange(response_text_recoded)
res_plot <- res_plot_tmp2 |>
  factorize("vern_name_eng", list(res_plot_tmp2$vern_name_eng |> unique() |> rev()))
#
# plot
plot_stadium_dist <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = response_text_recoded, y = vern_name_eng)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(stadium),
    scales = "free",
    space = "free",
    labeller = ggplot2::label_wrap_gen(width = 10)
  ) +
  ggplot2::scale_x_discrete(labels = ggplot2::label_wrap_gen(width = 20)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank(),
    strip.text.y.right = ggplot2::element_text(angle = 0)
  ) +
  ggplot2::labs(
    x = "distribution of species",
    y = "species (vernacular name eng)"
  )
#
#
#
# --- plotting - Score proportions "unknown" vs. rest per species ---------------------------------------------------
#
#
# convert to plot data
res_plot_tmp <- res_scored_allspec |>
  dplyr::mutate(
    score_category = dplyr::case_when(
      !grepl("unknown|do not know", response_text) ~ "rest",
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
  c("question_text", "section_title", "species", "vern_name_eng"),
  list(
    res_plot_tmp$question_text |> unique(),
    res_plot_tmp$section_title |> unique() |> _[match(
      seq(min(res_plot_tmp$section_no), max(res_plot_tmp$section_no)),
      res_plot_tmp$section_no |> unique()
    )],
    levels_species,
    levels_vern_name_eng_allspec
  )
)
#
# plot
plot_unknown_prop_species <- ggplot2::ggplot(res_plot, ggplot2::aes(y = vern_name_eng)) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin = 0,
      xmax = prop_scores_unknown + prop_scores_Idonotknow,
      color = "unknown / I do not know"
    ),
    linewidth = 2
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin = prop_scores_unknown + prop_scores_Idonotknow,
      xmax = prop_scores_unknown + prop_scores_Idonotknow + prop_scores_rest,
      color = "rest"
    ),
    linewidth = 2
  ) +
  ggplot2::scale_color_manual(values = c(col_bg, col_hl)) +
  ggplot2::coord_cartesian(xlim = c(0,1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  ggplot2::labs(
    x = "Proportion responses",
    y = "Species (English vernacular name)",
    color = "Category response"
  )
# plot_unknown_prop_species
#
#
#
# --- plotting - Score proportions "unknown" vs. rest per question ---------------------------------------------------
#
#
# convert to plot data
res_plot_tmp <- res_scored_allspec |>
  dplyr::mutate(
    score_category = dplyr::case_when(
      !grepl("unknown|do not know", response_text) ~ "rest",
      TRUE ~ response_text |> gsub(" ", "", x = _)
    )
  ) |>
  # score category proportions per question
  dplyr::mutate(
    n_scores_category = dplyr::n(),
    .by = c(question_text, score_category)
  ) |>
  dplyr::mutate(
    n_scores_tot = dplyr::n(),
    .by = question_text
  ) |>
  dplyr::mutate(prop_scores = n_scores_category / n_scores_tot) |>
  tidyr::pivot_wider(
    data = _,
    names_from = score_category,
    values_from = prop_scores,
    names_prefix = "prop_scores_"
  ) |>
  dplyr::group_by(question_text) |>
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
  c("question_text_short", "section_title", "species", "vern_name_eng"),
  list(
    res_plot_tmp$question_text_short |> unique(),
    res_plot_tmp$section_title |> unique() |> _[match(
      seq(min(res_plot_tmp$section_no), max(res_plot_tmp$section_no)),
      res_plot_tmp$section_no |> unique()
    )],
    levels_species,
    levels_vern_name_eng_allspec
  )
)
#
# plot
plot_unknown_prop_question <- ggplot2::ggplot(res_plot, ggplot2::aes(x = question_text_short)) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      ymin = 0,
      ymax = prop_scores_unknown + prop_scores_Idonotknow,
      color = "unknown / I do not know"
    ),
    linewidth = 2
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      ymin = prop_scores_unknown + prop_scores_Idonotknow,
      ymax = prop_scores_unknown + prop_scores_Idonotknow + prop_scores_rest,
      color = "rest"
    ),
    linewidth = 2
  ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(section_title),
    scales = "free",
    space = "free"
  ) +
  ggplot2::scale_color_manual(values = c(col_bg, col_hl)) +
  #ggplot2::scale_y_discrete(labels = labs_hl) +
  ggplot2::coord_cartesian(ylim = c(0,1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = ggplot2::margin(5,5,5,30),
    strip.text.x.top = ggplot2::element_text(angle = 50, hjust = 0, vjust = 0)
  ) +
  ggplot2::labs(
    x = "Question",
    y = "Proportion responses",
    color = "Category response"
  )
# plot_unknown_prop_question
#
#
#
# --- plotting - Score patterns "unknown" vs. rest per species and question ---------------------------------------------------
#
res_plot_upd <- res_plot |>
  dplyr::mutate(
    score_category = dplyr::case_when(
      !grepl("unknown|do not know", response_text) ~ "rest",
      TRUE ~ "unknown / I do not know"
    )
  )

plot_unknown_pattern <- ggplot2::ggplot(
  res_plot_upd,
  ggplot2::aes(
    x = question_text_short, y = vern_name_eng,
    fill = score_category, color = score_category
  )
) +
  ggplot2::geom_point(size = 3, shape = 21) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(section_title),
    scales = "free",
    space = "free"
  ) +
  ggplot2::scale_color_manual(values = c(col_bg, col_hl)) +
  ggplot2::scale_fill_manual(values = c(col_bg, col_hl)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = ggplot2::margin(5,30,5,5),
    strip.text.x.top = ggplot2::element_text(angle = 50, hjust = 0, vjust = 0)
  ) +
  ggplot2::labs(
    x = "Question",
    y = "Species (English vernacular name)",
    color = "Category response",
    fill = "Category response"
  )

#plot_unknown_pattern
#
#
#
# --- plotting - Frequency of methods ---------------------------------------------------
#
# get recoded and processed data
res_meth_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda")))
res_meth_options <- get(load((paste0(response_data_path, "recoded_processed/", "results_methods_options.rda"))))
#
# frequency plot also for best methods
#
# get response options per species in long format
res_plot_tmp1 <- res_meth_recoded |>
  dplyr::filter(grepl("D1$", question_id), !grepl("followup", question_text)) |>
  tidyr::crossing(res_meth_options) |>
  dplyr::relocate(tidyselect::any_of(tidyselect::starts_with("response_options")),
                  .after = response_text_final) |>
  dplyr::group_by(species) |>
  dplyr::rowwise() |>
  dplyr::filter(grepl(pattern = response_options, x = response_text_final)) |>
  dplyr::ungroup()
#
#
# count response option across species
res_plot_tmp2 <- res_plot_tmp1 |>
  dplyr::mutate(
    response_count = dplyr::n(),
    .by = response_options,
    .after = response_options
  ) |>
  dplyr::distinct(
    response_options,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(response_count)
#
res_plot <- factorize(
  res_plot_tmp2,
  c("response_options", "response_options_cat"),
  list(res_plot_tmp2$response_options,
                   res_meth_options$response_options_cat |> unique()
  )
)
#
# plot
plot_meth_freq <- ggplot2::ggplot(res_plot) +
  ggplot2::geom_linerange(
    ggplot2::aes(xmin = 0, xmax = response_count,
                 y = response_options,
                 color = response_options_cat),
    linewidth = 2
  ) +
  ggplot2::scale_color_manual(values = c(cols_meth)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "Frequency",
    y = "Method",
    color = "Method category"
  )
# plot_meth_freq
#
#
#
# --- plotting - Methods combinations ---------------------------------------------------
#
### plot combination of method within species (network graph)
#
# get maximum number of methods selected
methods_n_max <- res_plot_tmp1 |>
  dplyr::count(species) |>
  dplyr::pull(n) |>
  max()
#
# get number of unique pairwise methods combinations (ignoring order)
methods_pw_max <- methods_n_max * (methods_n_max - 1) / 2
#
# prepare plot data
res_plot_tmp <- res_plot_tmp1 |>
  dplyr::mutate(
    tmp = dplyr::row_number(),
    .by = species
  ) |>
  dplyr::select(-response_options_cat) |>
  tidyr::pivot_wider(
    names_from = tmp,
    names_prefix = "method_",
    values_from = response_options
  ) |>
  # get all unique pairwise combinations of method columns
  dplyr::rowwise() |>
  dplyr::mutate(
    method_pw = combn(
      x = dplyr::across(tidyselect::contains("method")), # & where !is.na not working
      m = 2,
      FUN = paste,
      collapse = ";"
    ) |> paste(x = _, collapse = ",")
  ) |>
  # separate pairwise combinations into columns
  tidyr::separate_wider_delim(
    cols = method_pw,
    delim = ",",
    names = paste0("method_pw_", 1:methods_pw_max),
    too_few = "align_start"
  ) |>
  # transform to long format
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("method_pw"),
    names_to = "method_pw_name",
    values_to = "method_pw_value"
  ) |>
  # remove combinations containing NA
  dplyr::filter(
    !grepl("NA\\;|\\;NA", method_pw_value)
  ) |>
  # separate methods pw values into from and to columns
  tidyr::separate_wider_delim(
    cols = method_pw_value,
    delim = ";",
    names = c("from", "to")
  ) |>
  # turn into network data
  dplyr::select(c("from", "to")) |>
  tidygraph::as_tbl_graph(x = _, directed = FALSE) |>
  tidygraph::activate(edges) |>
  dplyr::group_by(from, to) |>
  dplyr::mutate(n_links = dplyr::n()) |>
  dplyr::ungroup() |>
  tidygraph::activate(nodes) |>
  dplyr::arrange(match(name, res_plot_tmp2$response_options))
#
plot_meth_netw <- ggraph::ggraph(graph = res_plot_tmp, layout = 'linear', circular = FALSE) +
  ggraph::geom_edge_arc(ggplot2::aes(edge_width = n_links/2.5, label = n_links, alpha = (n_links + 10)/100),
                        #label_dodge = grid::unit(3, 'mm'),
                        angle_calc = 'along',
                        label_size = 4,
                        colour = col_hl,
                        lineend = "butt"
  ) +
  ggraph::geom_node_text(ggplot2::aes(label = name), nudge_y = -0.1, hjust = 1, color = col_black) +
  ggplot2::coord_flip(ylim = c(-2,9)) +
  ggplot2::theme_void()

#plot_meth_netw
#
#
#
# --- plotting - Characteristics best method ---------------------------------------------------
#
if (FALSE) {

# issue with missing response text recoded
# data best methods (reply "geen" turns NA)
res_plot_best <- res_meth_recoded |>
  dplyr::filter(grepl("D2$", question_id)) |>
  tidyr::crossing(res_meth_options) |>
  dplyr::relocate(tidyselect::any_of(tidyselect::starts_with("response_options")),
                  .after = response_text_final) |>
  dplyr::group_by(species) |>
  dplyr::rowwise() |>
  dplyr::filter(grepl(pattern = response_options, x = response_text_final)) |>
  dplyr::ungroup() |>
  factorize(
    dataframe = _,
    c("vern_name_eng", "response_options", "response_options_cat", "prius_milieu"),
    list(levels_vern_name_eng_allspec,
                     res_meth_options$response_options,
                     res_meth_options$response_options_cat |> unique(),
                     levels_milieu
    )
  )
#
res_plot_tmp <- res_plot_best |>
  dplyr::distinct(species, .keep_all = TRUE) |>
  dplyr::rename(best_method = response_text_final) |>
  dplyr::select(tidyselect::contains(c("species", "best_method"))) |>
  dplyr::full_join(
    x = _,
    y = res_scored |>
      dplyr::filter(grepl("D3|D4|D5", question_id)) |>
      dplyr::filter(!grepl("followup", question_text))
  ) |>
  # adapt response text
  dplyr::mutate(
    response_text_recoded = dplyr::case_when(
      grepl("unknown|do not know", response_text) ~ "unknown / I do not know",
      grepl("low", response_text) ~ "low",
      grepl("medium", response_text) ~ "medium",
      grepl("high", response_text) ~ "high"
    )
  ) |>
  # sort according to response score and frequency best method
  dplyr::mutate(
    n_rows = dplyr::n(),
    .by = best_method
  ) |>
  dplyr::arrange(
    n_rows |> dplyr::desc(),
    response_score
  )
#
res_plot <- res_plot_tmp |>
  factorize(
    dataframe = _,
    c("best_method", "question_text_short", "response_text_recoded"),
    list(res_plot_tmp$best_method |> unique(),
                     res_plot_tmp$question_text_short |> unique() |> rev(),
                     res_plot_tmp$response_text_recoded |> unique()
    )
  )
#
plot_bestmeth <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = response_text_recoded)
) +
  ggplot2::geom_bar() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(best_method),
    cols = ggplot2::vars(question_text_short),
    labeller = ggplot2::label_wrap_gen(width = 20),
    switch = "y",
    scales = "free"
  ) +
  ggplot2::scale_y_continuous(breaks = \(x) seq(floor(min(x)), ceiling(max(x)), by = ceiling(max(x)/4))) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text.y.left = ggplot2::element_text(angle = 0),
    strip.placement = "outside",
    axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1)
  ) +
  ggplot2::labs(
    x = "response score",
    y = "frequency"
  )
plot_bestmeth

}
#
#
#
# --- plotting - Existing monitoring efforts ---------------------------------------------------
#
#
# get recoded and processed data
res_moni_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda")))
res_moni_options <- get(load((paste0(response_data_path, "recoded_processed/", "results_monitoring_options.rda"))))

### Monitoring schemes across species

# get response options per species in long format
res_plot_tmp <- res_moni_recoded |>
  dplyr::filter(grepl("D8", question_id)) |>
  tidyr::crossing(response_options = res_moni_options) |>
  dplyr::relocate(tidyselect::any_of(tidyselect::starts_with("response_options")),
                  .after = response_text_final) |>
  dplyr::group_by(species) |>
  dplyr::rowwise() |>
  dplyr::filter(
    grepl(
      pattern = response_options |> gsub("\\(", "\\\\(", x = _) |> gsub("\\)", "\\\\)", x = _),
      x = response_text_final
      )
    ) |>
  dplyr::ungroup() |>
  # sort schemes according to number of species covered
  dplyr::mutate(
    n_species = dplyr::n(),
    .by = response_options
  ) |>
  dplyr::arrange(n_species |> dplyr::desc()) |>
  # separate informative answers from uninformative ones
  dplyr::mutate(
    response_category =   dplyr::case_when(
      grepl("none|do not know|^other$", response_options) ~ "Monitoring absent or unknown",
      TRUE ~ "Monitoring reported"
    )
  )
#
# convert to plot data
res_plot <- factorize(
  res_plot_tmp,
  c("vern_name_eng", "response_options", "prius_milieu"),
  list(levels_vern_name_eng_allspec,
                   res_plot_tmp$response_options |> unique(),
                   levels_milieu
  )
)
#
# plot
plot_moni_patt <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = response_options, y = vern_name_eng)
) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(kingdom),
    cols = ggplot2::vars(response_category),
    scales = "free",
    space = "free",
    labeller = ggplot2::label_wrap_gen(width = 15)
  ) +
  ggplot2::geom_point(size = 3, color = col_black) +
  ggplot2::scale_x_discrete(labels = ggplot2::label_wrap_gen(width = 30)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text.x.top = ggplot2::element_text(angle = 0, hjust = 0, vjust = 0),
    axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1),
    axis.text.y = ggtext::element_markdown(),
    strip.text.y.right = ggplot2::element_text(angle = 0)
  ) +
  ggplot2::labs(
    x = "Methods",
    y = "Species (vernacular name eng)"
  )
# plot_moni_patt


