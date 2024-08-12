make_ganttchart <- function(
    data_sheet,
    col_var,
    phase_no_plot = 1,
    show_tasks_withoutdate = FALSE,
    tasklabel_textsize = 3,
    tasklabel_linebreak_nchar = 20,
    show_brackets = TRUE,
    brackets_vjust = -0.6,
    brackets_distance = 2,
    barwidth_scalingfactor = 5,
    cols4all_palette = "carto.pastel"
 ) {
  #
  # process data for plotting
  data_sheet_upd <- data_sheet |>
    dplyr::rename(
      !!"start" := "termijn_begin",
      !!"end" := "termijn_einde",
      !!"task_no" := "taak_nr",
      !!"task" := "taak_naam",
      !!"phase_no" := "fase_nr",
      !!"step" := "stap_naam",
      !!"block" := "bouwsteen_naam",
      !!"element" := "element_naam",
      !!"responsible" := "trekker"
    ) |>
    dplyr::mutate(
      start = as.Date(start),
      end = as.Date(end),
      phase = paste("fase", get("phase_no")),
      status = dplyr::case_when(
        grepl("niet gestart", status) ~ "not started",
        grepl("lopend", status) ~ "ongoing",
        grepl("voltooid", status) ~ "done"
      ),
      # adjust appearance based on status
      text_col = dplyr::case_when(
        grepl("^done", status) ~ "darkgrey",
        TRUE ~ "black"
      ),
      text_fontface = dplyr::case_when(
        grepl("ongoing", status) ~ "bold",
        TRUE ~ "plain"
      ),
      bar_alpha = dplyr::case_when(
        grepl("not started", status) ~ 1,
        TRUE ~ .3
      )
    )
  # add line breaks and spaces to tasks
  data_sheet_upd$task <- sapply(
    seq_len(nrow(data_sheet_upd)), function(x) {
      stringi::stri_wrap(
        str = data_sheet_upd$task[x],
        width = tasklabel_linebreak_nchar
        ) |>
        paste0(collapse = "\n")
    }
  )

  data_sheet_upd$task <- paste0(" ", data_sheet_upd$task)
  # bar size based on number of linebreaks
  data_sheet_upd <- data_sheet_upd |>
    dplyr::mutate(
      bar_size = (
        stringr::str_count(get("task"), "\n") + 2
        ) * barwidth_scalingfactor
    )
  #
  # rearrange, filter data for plotting
  if (show_tasks_withoutdate) {
    # dummy dates (not shown) if  show_tasks_withoutdate = TRUE
    data_plot <- data_sheet_upd |>
      dplyr::filter(is.na(start) & is.na(end)) |>
      dplyr::mutate(
        start = as.Date("2024-08-05"), # monday
        end = as.Date("2024-08-09"), # friday
        phase = paste(get("phase"), "(taken zonder termijn)")
      )
  } else {
    data_plot <- data_sheet_upd |>
      tidyr::drop_na(dplyr::any_of(c("start", "end")))
  }
  # reorder tasks & responsibles
  data_plot <- data_plot |>
    dplyr::arrange(get("task_no")) |>
    dplyr::filter(get("phase_no") %in% get("phase_no_plot"))
  task_ordered <- data_plot |> dplyr::pull(get("task")) |> rev()
  responsible_ordered <- c(
    c("Janne", "Diederick"), data_plot$responsible
  ) |> unique()
  data_plot_final <- data_plot |>
    dplyr::mutate(
      task = factor(
        x = get("task"),
        levels = task_ordered
      ),
      responsible = factor(
        x = get("responsible"),
        levels = responsible_ordered
      )
    )
  #
  # get weekends / weekdays
  date_min <- data_plot_final$start |> min(x = _, na.rm = TRUE)
  date_max <- data_plot_final$end |> max(x = _, na.rm = TRUE)
  data_dates <- data.frame(
    date = seq(
      date_min,
      date_max,
      by = "1 day")
  ) |>
    dplyr::mutate(
      weekday = weekdays(date)
    ) |>
    dplyr::mutate(
      weekend = dplyr::case_when(
        grepl("Saturday|Sunday|zaterdag|zondag", weekday) ~ date,
        TRUE ~ NA
      ),
      weekend_mid = dplyr::case_when(
        grepl("Saturday|zaterdag", weekday) ~ date + 0.5,
        TRUE ~ NA
      ),
      workday = dplyr::case_when(
        grepl("Saturday|Sunday|zaterdag|zondag", weekday) ~ NA,
        TRUE ~ date
      ),
      monday = dplyr::case_when(
        grepl("Monday|maandag", weekday) ~ date,
        TRUE ~ NA
      )
    )
  #
  # remove dates christmas break
  breakdates <- seq(
    paste0(lubridate::year(date_min), "-12-24") |> as.Date(),
    paste0(lubridate::year(date_min) + 1, "-01-01") |> as.Date(),
    by = "1 day")
  data_dates <- data_dates |> dplyr::filter(! date %in% breakdates)
  #
  # data for brackets
  if (show_brackets) {
    data_brackets <- lapply(c("element", "block", "step"), function(x) {
      dplyr::full_join(
        data_plot_final |>
          dplyr::group_by(dplyr::across(dplyr::all_of(x))) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::select(c("task", x)) |>
          dplyr::rename(!!"xmin" := "task", !!"label" := x),
        data_plot_final |>
          dplyr::group_by(dplyr::across(dplyr::all_of(x))) |>
          dplyr::slice(dplyr::n()) |>
          dplyr::ungroup() |>
          dplyr::select(c("task", x)) |>
          dplyr::rename(!!"xmax" := "task", !!"label" := x)
      ) |>
        dplyr::mutate(type = x)
    }
    )  |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        ypos = dplyr::case_when(
          grepl("element", type) ~ date_min - (1 * brackets_distance),
          grepl("block", type) ~ date_min - (2 * brackets_distance),
          grepl("step", type) ~ date_min - (3 * brackets_distance),
        )
      ) |>
      na.omit()
    # adapt xmin, xmax
    data_brackets <- data_brackets |>
      dplyr::mutate(
        xmin = as.numeric(get("xmin")) + 0.25,
        xmax = as.numeric(get("xmax")) - 0.25
      )
    # reformat brackets label for plot
    data_brackets$spaces <- stringr::str_count(data_brackets$label, " ")
    spaces_max <- data_brackets$spaces |> max(x = _, na.rm = TRUE)
    data_brackets$label_re <- sapply(
      seq_len(nrow(data_brackets)),
      function(x) {
        paste0(
          paste0(
            rep("\n", spaces_max - data_brackets$spaces[x]),
            collapse = ""),
          data_brackets$label[x]
        )
      }
    )
    data_brackets$label_re <- gsub(" ", "\n", data_brackets$label_re)
  }
  #
  # colors
  col_var_upd <- switch(
    col_var,
    trekker = "responsible"
  )
  # trick to get full legend
  responsible_diff <- setdiff(
    data_sheet_upd$responsible |> na.omit() |> unique(),
    data_plot_final$responsible |> na.omit() |> unique()
  )
  if (length(responsible_diff) > 0) {
    tmp <- data_plot_final[1, ] |>
      dplyr::slice(rep(1, each = length(responsible_diff))) |>
      dplyr::mutate(
        responsible = responsible_diff,
        start = NA,
        end = NA
      )
    data_plot_final <- dplyr::bind_rows(
      tmp,
      data_plot_final
    )
  }
  #
  # make plot
  ggplot2::ggplot(data_plot_final) + {
    # weekends
    if (!show_tasks_withoutdate)
      ggplot2::geom_hline(
        data = data_dates,
        ggplot2::aes(yintercept = .data$weekend_mid),
        color = "grey90",
        linewidth = 5
      )
    } + {
    # current date
      if (!show_tasks_withoutdate)
      ggplot2::geom_hline(
        yintercept = Sys.Date(),
        linetype = "dashed"
      )
    } +
    # bars
    ggplot2::geom_segment(
      ggplot2::aes(
        y = get("start"), yend = end, x = get("task"),
        color = get(col_var_upd)
      ),
      size = data_plot_final$bar_size,
      lineend = "butt",
      alpha = data_plot_final$bar_alpha
    ) +
    # tasks
    ggplot2::geom_text(
      ggplot2::aes(y = get("start"), x = get("task"), label = get("task")),
      col = data_plot_final$text_col,
      fontface = data_plot_final$text_fontface,
      hjust = 0,
      size = tasklabel_textsize,
      lineheight = .8
    ) + {
    # brackets
      if (show_brackets)
      ggpubr::geom_bracket(
        data = data_brackets,
        ggplot2::aes(
          xmin = get("xmin"),
          xmax = get("xmax"),
          y.position = get("ypos")
        ),
        label = data_brackets$label_re |> unique(),
        tip.length = c(-0.01, -0.01),
        size = 0.5,
        coord.flip = TRUE,
        vjust = brackets_vjust,
        angle = 90,
        label.size = tasklabel_textsize,
        lineheight = .8
      )
    } +
    # facet by phase
    ggplot2::facet_wrap(
      ggplot2::vars(get("phase")),
      ncol = 1,
      scales = "free"
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "",
      colour = col_var
    ) +
    cols4all::scale_color_discrete_c4a_cat(palette = cols4all_palette) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = if (show_tasks_withoutdate) {
        ggplot2::element_blank()
      } else {
        NULL
        },
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = if (show_tasks_withoutdate) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_line(linewidth = 2)
      },
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = if (show_tasks_withoutdate) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_text(
          angle = 45,
          hjust = 1,
          size = tasklabel_textsize * 3
        )
      },
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(0, 0, 0, 20, "pt"),
        size = tasklabel_textsize * 3
      ),
      legend.title = ggplot2::element_text(
        size = tasklabel_textsize * 3
      ),
      legend.position = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = tasklabel_textsize * 3)
    ) +
    ggplot2::scale_y_date(
      date_labels = "%a %d %b",
      limits = c(
        date_min - ifelse(show_brackets, 3 * brackets_distance, 0),
        date_max + 5
      ),
      breaks = data_dates$monday |> na.omit()
    ) +
    ggplot2::guides(color =  ggplot2::guide_legend(
      override.aes = list(linewidth = 10, width = 15)
    )) +
    ggplot2::coord_flip()
}
