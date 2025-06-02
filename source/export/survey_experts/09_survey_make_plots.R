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
  dplyr::filter(question_scored |> as.logical(), !section_skipped) |>
  dplyr::filter(!grepl("IRR", prius_stadium))
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
# # grand means urgency & feasibility
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
#cor(res_m_allspec |> dplyr::select(tidyselect::contains(c("m_"))))
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
  cols = c("#247D3F", "#04A3BD", "#F0BE3D", "#DA7901", "#931E18"),
  stadium = levels_stadium
)
#
# colors feasibility urgency
cols_feasurge <- c("#04A3BD", "#DA7901")
#
# color to highlight data
col_hl <- "#F0BE3D"
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
# axis labels to highlight
# these (only work if y axis not facetted)
labs_hl_allspec <- highlight_labs(
  labs = levels_vern_name_nld_allspec,
  pattern = "kreeft|muntjak",
  color = col_labs
)
labs_hl <- highlight_labs(
  labs = levels_vern_name_nld,
  pattern = "kreeft|muntjak",
  color = col_labs
)
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
      on_unionlist ~ "on unionlist",
      !on_unionlist ~ "not on unionlist"
    ),
    part = dplyr::case_when(
      dplyr::row_number() < (dplyr::n()/2) ~ "part 1",
      TRUE ~ "part 2"
    )
  )
res_plot <- factorize(
  res = res_plot_tmp,
  varnames = c("species", "vern_name_eng", "vern_name_nld",
               "on_unionlist_upd", "stadium"),
  varlevels = list(
    levels_species_allspec,
    levels_vern_name_eng_allspec,
    levels_vern_name_nld_allspec,
    c("on unionlist", "not on unionlist"),
    levels_stadium
  )
)
#
# plot
# (split in 2 using facet_wrap - requires attention to labels)
plot_ranking <- ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = m_feasurge, y = vern_name_nld, color = stadium)) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = 0, xmax = m_feasurge), linetype = "dotted") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = m_min, xmax = m_max)) +
  ggplot2::geom_point(ggplot2::aes(x = m_feasurge), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = m_feas), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = m_feas), shape = "F") +
  ggplot2::geom_point(ggplot2::aes(x = m_urge), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = m_urge), shape = "U") +
  ggplot2::geom_point(ggplot2::aes(shape = on_unionlist_upd), size = 2) +
  ggplot2::scale_shape_manual(values = c(16, 1)) +
  ggplot2::scale_color_manual(
    values = cols_stadium |>
      dplyr::filter(stadium %in% (res_plot$stadium |> unique())) |>
      dplyr::pull(cols)) +
  ggplot2::coord_cartesian(xlim = c(
    res_scored_allspec$response_score |> na.omit() |> min(),
    res_scored_allspec$response_score |> na.omit() |> max()
  )) +
  #ggplot2::scale_y_discrete(labels = labs_hl_allspec) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    axis.text.y = ggtext::element_markdown(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "mean of feasibilty and urgency scores (grand mean)",
    y = "species (vernacular name nld)"
  )

# plot_ranking
# closer to F if species is present as F then has more questions
# closer to U if species is absent as U then has more questions

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
#
# --- plotting - scoring of questions ---------------------------------------------------
#
# get questions
q_long <- get(load(paste0(questions_path, "questions_long.rda")))
#
#
# prepare data for plotting
q_plot_tmp <- q_long |>
  # filter rows
  dplyr::filter(question_include_in_form == 1, question_use_for_ranking == 1) |>
  tidyr::drop_na(score_response_option) |>
  # shorten responses & insert line breaks within still long responses
  dplyr::rowwise() |>
  dplyr::mutate(
    response_option = response_option |>
      stringr::str_trunc(string = _, width = 65) |>
      paste("-", y = _) |>
      stringr::str_wrap(width = 40) |>
      paste(x = _, NULL, collapse = "\n"),
  ) |>
  # merge responses based on response scores
  tidyr::pivot_wider(
    values_from = response_option,
    names_from = score_response_option,
    values_fn = function(x) paste(x, collapse = "\n")
  ) |>
  tidyr::pivot_longer(
    cols = paste(1:4),
    names_to = "score",
    values_to = "response"
  ) |>
  dplyr::mutate(
    score = as.numeric(score)
  ) |>
  # add question text short
  dplyr::left_join(
    x = _,
    y = res_scored |>
      dplyr::select(tidyselect::contains(c("question_id", "question_text_short"))) |>
      dplyr::distinct(question_id, .keep_all = TRUE)
  )
#
q_plot <- factorize(
  q_plot_tmp,
  c("question_text_short", "section_title"),
  list(
    q_plot_tmp$question_text_short |> unique() |> rev(),
    q_plot_tmp$section_title |> unique()
  )
)
#
# plot
plot_scoring <- ggplot2::ggplot(
  data = q_plot,
  mapping = ggplot2::aes(x = score, y = question_text_short)) +
  ggplot2::geom_label(
    ggplot2::aes(label = response, fill = score_category),
    hjust = 0,
    size = 3,
    angle = 0,
    alpha = .2
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(section_title),
    scales = "free",
    space = "free") +
  ggplot2::scale_x_continuous(
    breaks = 1:4,
    labels = paste(1:4, c("\nLow feasibility / urgency", "", "", "\nHigh feasibility / urgency"))
  ) +
  ggplot2::scale_fill_manual(values = cols_feasurge) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_cartesian(xlim = c(0.8,4.8)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )
#
# prep data for display in table
#
data_table_scoring <- q_plot |>
  dplyr::left_join(
    x = _,
    y = q_long |>
      dplyr::select(
        tidyselect::contains(c("question_id", "score_response", "response_option"))
        ),
    by = c("score" = "score_response_option", "question_id")
  ) |>
  dplyr::mutate(
    response_option_no= 1:dplyr::n(),
    .by = c(question_id, score)
  ) |>
  tidyr::pivot_wider(
    id_cols = c(question_text_short, response_option_no, section_number, score_category),
    names_from = score,
    values_from = response_option
  ) |>
  dplyr::select(-response_option_no) |>
  dplyr::group_by(question_text_short) |>
  tidyr::fill(tidyselect::everything()) |>
  dplyr::ungroup()

#
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

