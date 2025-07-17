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
  dplyr::arrange(dplyr::desc(gm_mfeas_murge))
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
cols_meth <- c("#5B859E","#1E395F","#75884B","#1E5A46","#DF8D71","#AF4F2F","#D48F90","#732F30","#AB84A5","#59385C")
#
# color to highlight axis labels
col_labs <- "#71797E" #"#EA5F94"
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
    .by = indicator
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
  ggplot2::geom_vline(ggplot2::aes(linetype = indicator, xintercept = indicator_cutoff)) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = gm_mfeas_murge, xmax = 5), linetype = "dotted") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = m_min, xmax = m_max), linewidth = 0.7) +
  ggplot2::geom_point(size = 2.5, fill = "white") +
  # annotation
  ggplot2::annotate(
    "text", x = cutoff_feas + 0.1, y = 0, hjust = 0, size = 3, angle = 90,
    label = "critical value feasibility" |> stringr::str_wrap(width = 20)
    ) +
  ggplot2::annotate(
    "text", x = cutoff_urge + 0.1, y = 0, hjust = 0, size = 3, angle = 90,
    label = "critical value urgency" |> stringr::str_wrap(width = 20)
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
  ggplot2::scale_linetype_manual(values = c("solid", "longdash"), guide = "none") +
  ggplot2::scale_y_continuous(
    position = "right",
    breaks = res_plot$y_axis_num_helper |> unique(),
    labels = res_plot$vern_name_eng |> levels(),
    sec.axis = ggplot2::dup_axis(name = "Species (vernular name eng)", labels = NULL, breaks = NULL),
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
plot_ranking


# --- plotting - invasion stadium vs. prius stadium ---------------------------------------------------

if (FALSE){
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
  dplyr::group_by(label_helper) |>
  dplyr::mutate(
    stadium_label = dplyr::case_when(
      (prius_stadium_upd != stadium & dplyr::row_number() == 1) ~ paste(vern_name_nld, collapse = "\n"),
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
    alpha = .4
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
  ggplot2::scale_y_discrete(drop = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "stadium in prius",
    y = "stadium reported by expert"
  )

plot_stadium
#
#
#
# --- plotting - invasion stadium vs. species distribution ---------------------------------------------------
#
res_plot_tmp1 <- res_scored |>
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
  factorize("vern_name_nld", list(res_plot_tmp2$vern_name_nld |> unique() |> rev()))
#
# plot
plot_stadium_dist <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = response_text_recoded, y = vern_name_nld)) +
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
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
    strip.background = ggplot2::element_blank(),
    strip.text.y.right = ggplot2::element_text(angle = 0)
  ) +
  ggplot2::labs(
    x = "distribution of species",
    y = "species (vernacular name nld)"
  )
#
#
# --- plotting - score distributions per question ---------------------------------------------------
#
if (exists("mode_source") && grepl("presentation", mode_source)) {

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
plot_scoredist <- ggplot2::ggplot(res_plot, ggplot2::aes(response_score)) +
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
    labeller = ggplot2::label_wrap_gen(width = 20)
  ) +
  ggplot2::scale_fill_manual(values = cols_feasurge) +
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

}
}

