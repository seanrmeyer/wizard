
wiz_viz = function(wiz_frame, categories = NULL, variables = NULL, grouping = NULL, predictor_type = "rolling", id = NULL, n_ids = NULL, plot = 'rugplot') {

  # if an id is provided set n_ids to 1 else if n_ids is null then show all ids
  if(!is.null(id)) {
    n_ids = 1
  } else if (is.null(n_ids)) { # set number of IDs to sample
      n_ids = wiz_frame$temporal_data %>%
        pull(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
        length()
    }

  # identify left and right time limits to include
  if(predictor_type == "baseline") {
    timespan_min = min(wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    # timespan_min = lookback # first prediction minus lookback convert by step_unit
    timespan_max = 0
  } else if (predictor_type == "growing") {
    timespan_min = 0
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  } else { # default
    timespan_min = min(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  }

  sampled_data = wiz_frame$temporal_data %>%
    filter_if(!is.null(id), !!rlang::parse_expr(wiz_frame$temporal_id) == id) %>%
    select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
    sample_n(n_ids) %>%
    inner_join(wiz_frame$temporal_data) %>%
    filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
           !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max)


  if(!is.null(categories)) {
    sampled_data %>%
      filter(grepl(categories, !!rlang::parse_expr(wiz_frame$temporal_category)))
  } else if (!is.null(variables)) {
    # grouping = "variable"
    sampled_data %>%
      filter(grepl(variables, !!rlang::parse_expr(wiz_frame$temporal_variable)))
  }

  if(grouping == "category") {
    grouping_var = wiz_frame$temporal_category
  } else if (grouping == "variable") {
    grouping_var = wiz_frame$temporal_variable
  } else { # default
    grouping_var = wiz_frame$temporal_variable
  }

  if(plot == "rugplot") {
  sampled_data %>%
    ggplot(aes(x = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)),
               y = !!rlang::parse_expr(grouping_var))) +
    ggridges::geom_density_ridges(
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(width = 0.05, height = 0),
      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    ) +
    # geom_vline(xintercept=as.numeric(seq(from = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% min(),
    #                                                           to = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% max(),
    #                                                           by=wiz_frame$step)),
    # linetype=4, colour="black") +
    xlab(paste0("time in ", wf$step_units, "s")) #+
  # facet_grid(rows = vars(!!rlang::parse_expr(grouping_var)))
  } else if (plot == "boxplot") {
    sampled_data %>%
      ggplot(aes(x = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)),
                 y = !!rlang::parse_expr(grouping_var))) +
      ggridges::geom_density_ridges(
        jittered_points = TRUE,
        position = ggridges::position_points_jitter(width = 0.05, height = 0),
        point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
      ) +
      # geom_vline(xintercept=as.numeric(seq(from = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% min(),
      #                                                           to = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% max(),
      #                                                           by=wiz_frame$step)),
      # linetype=4, colour="black") +
      xlab(paste0("time in ", wf$step_units, "s")) #+
    # facet_grid(rows = vars(!!rlang::parse_expr(grouping_var)))
  }
}

#' A visualization to to evaluate lookback timespan in preparation for building wizard predictors
#'
#' @param wiz_frame a wiz_frame object
#' @param categories one or more categories separated by |
#' @param variables one or more variables separated by |
#' @param grouping options are category or variable
#' @param predictor_type options are baseline, rolling, or growing
#' @param n_ids number of IDs to include in plot
#'
#' @return plot
#' @import ggplot2 dplyr ggridges
#' @export
#'
#' @examples wiz_viz_lookback(wiz_frame, categories = "labs", predictor_type = "baseline")
wiz_viz_lookback = function(wiz_frame, categories = NULL, variables = NULL, grouping = "variable", predictor_type = "rolling", n_ids = NULL) {

  # identify left and right time limits to include
  if(predictor_type == "baseline") {
    timespan_min = min(wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    # timespan_min = lookback # first prediction minus lookback convert by step_unit
    timespan_max = 0
  } else if (predictor_type == "growing") {
    timespan_min = 0
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  } else { # default
    timespan_min = min(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  }

  # set number of IDs to sample
  if(is.null(n_ids)) {
    n_ids = wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      length()
  }

  if(!is.null(categories)) {
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(categories, !!rlang::parse_expr(wiz_frame$temporal_category)))
  } else if (!is.null(variables)) {
    # grouping = "variable"
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(variables, !!rlang::parse_expr(wiz_frame$temporal_variable)))
  } else { # default
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max)
  }

  if(grouping == "category") {
    grouping_var = wiz_frame$temporal_category
  } else if (grouping == "variable") {
    grouping_var = wiz_frame$temporal_variable
  } else { # default
    grouping_var = wiz_frame$temporal_variable
    }

  sampled_data %>%
    ggplot(aes(x = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)),
               y = !!rlang::parse_expr(grouping_var))) +
    ggridges::geom_density_ridges(
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(width = 0.05, height = 0),
      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    ) +
    # geom_vline(xintercept=as.numeric(seq(from = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% min(),
    #                                                           to = sampled_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% max(),
    #                                                           by=wiz_frame$step)),
    # linetype=4, colour="black") +
    xlab(paste0("time in ", wf$step_units, "s")) #+
  # facet_grid(rows = vars(!!rlang::parse_expr(grouping_var)))
}


#' A visualization to to evaluate intervals in preparation for building wizard predictors
#'
#' @param wiz_frame a wiz_frame object
#' @param categories one or more categories
#' @param variables one or more variables
#' @param grouping options are category or variable
#' @param predictor_type options are baseline, rolling, or growing
#' @param n_ids number of IDs to include in plot
#'
#' @return plot
#' @export
#'
#' @examples wiz_viz_interval(wiz_frame, variables = "cr", predictor_type = "growing", n_ids = 1)
wiz_viz_interval = function(wiz_frame, categories = NULL, variables = NULL, grouping = "variable", predictor_type = "rolling", n_ids = NULL) {

  # identify left and right time limits to include
  if(predictor_type == "baseline") {
    timespan_min = min(wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    # timespan_min = lookback # first prediction minus lookback convert by step_unit
    timespan_max = 0
  } else if (predictor_type == "growing") {
    timespan_min = 0
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  } else { # default
    timespan_min = min(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  }

  # set number of IDs to sample
  if(is.null(n_ids)) {
    n_ids = wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      length()
  }

  if(!is.null(categories)) {
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(categories, !!rlang::parse_expr(wiz_frame$temporal_category)))
  } else if (!is.null(variables)) {
    # grouping = "variable"
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max)#,
             # grepl(variables, !!rlang::parse_expr(wiz_frame$temporal_variable)))
  } else { # default
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max)
  }

  if(grouping == "category") {
    grouping_var = wiz_frame$temporal_category
  } else if (grouping == "variable") {
    grouping_var = wiz_frame$temporal_variable
  } else { # default
    grouping_var = wiz_frame$temporal_variable
  }

  sampled_data %>%
    filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min, !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max) %>%
    group_by(!!rlang::parse_expr(wiz_frame$temporal_id), !!rlang::parse_expr(grouping_var)) %>%
    arrange(!!rlang::parse_expr(wiz_frame$temporal_time)) %>%
    summarize(!!rlang::parse_expr(wiz_frame$temporal_id), !!rlang::parse_expr(grouping_var), !!rlang::parse_expr(wiz_frame$temporal_time), prev_time = lag(!!rlang::parse_expr(wiz_frame$temporal_time)), interval = !!rlang::parse_expr(wiz_frame$temporal_time) - prev_time, .groups = "keep") %>%
    filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min, !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max) %>%
    na.omit() %>%
    ggplot(aes(x = !!rlang::parse_expr(grouping_var), y = interval, fill = !!rlang::parse_expr(grouping_var))) +
    geom_boxplot() +
    coord_flip()+
    # labs(title = "measurement interval by patient") +
    ylab(paste0("time in ", wf$step_units, "s")) #+
  # facet_grid(rows = vars(!!rlang::parse_expr(grouping_var)))
}


#' A visualization to to evaluate stats in preparation for building wizard predictors
#'
#' @param wiz_frame a wiz_frame
#' @param categories one or more categories
#' @param variables one or more variables
#' @param grouping options are category or variable
#' @param predictor_type options are baseline, rolling, or growing
#' @param n_ids number of IDs to include in plot
#' @param n_breaks a number of histogram breaks
#'
#' @return plot
#' @export
#'
#' @examples wiz_viz_stats(wiz_frame, variables = "cr", predictor_type = "growing", n_ids = 2, n_breaks = 10)
wiz_viz_stats = function(wiz_frame, categories = NULL, variables = NULL, grouping = "variable", predictor_type = "rolling", n_ids = NULL, n_breaks = 10) {

  # grouping = "category"

  # identify left and right time limits to include
  if(predictor_type == "baseline") {
    timespan_min = min(wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    # timespan_min = lookback # first prediction minus lookback convert by step_unit
    timespan_max = 0
  } else if (predictor_type == "growing") {
    timespan_min = 0
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  } else { # default
    timespan_min = min(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  }

  # set number of IDs to sample
  if(is.null(n_ids)) {
    n_ids = wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      length()
  }

  if(!is.null(categories)) {
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(categories, !!rlang::parse_expr(wiz_frame$temporal_category)))
  } else if (!is.null(variables)) {
    # grouping = "variable"
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(variables, !!rlang::parse_expr(wiz_frame$temporal_variable)))
  } else { # default
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max)
  }

  if(grouping == "category") {
    grouping_var = wiz_frame$temporal_category
  } else if (grouping == "variable") {
    grouping_var = wiz_frame$temporal_variable
  } else { # default
    grouping_var = wiz_frame$temporal_variable
  }

  # calculate breaks and midpoints using histogram
  h1 = sampled_data %>%
    pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% hist(breaks = n_breaks, plot = FALSE)

  sampled_data %>%
    # group_by(id, !!rlang::parse_expr(grouping)) %>%
    mutate(value = value %>% as.numeric(),
           time_bin = !!rlang::parse_expr(wiz_frame$temporal_time) %>% as.numeric() %>% cut(breaks = h1$breaks, labels = h1$mids)) %>%
    na.omit() %>%
    group_by(!!rlang::parse_expr(wiz_frame$temporal_id), !!rlang::parse_expr(grouping_var)) %>%
    mutate(mean_by_id = mean(value)) %>%
    ggplot(mapping = aes(x=time_bin, y=value)) +
    geom_jitter(aes(color="blue",alpha=0.2)) +
    geom_boxplot(fill="bisque",color="black",alpha=0.3) +
    labs(x=paste0('time (45 bin width)')) +
    guides(color=FALSE, alpha=FALSE) +
    theme_minimal() +
    xlab(paste0("time in ", wf$step_units, "s")) +
    facet_grid(rows = vars(!!rlang::parse_expr(grouping_var)))#,
  # stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red")
}


#' A visualization to to evaluate stats in preparation for building wizard predictors
#'
#' @param wiz_frame a wiz_frame
#' @param categories one or more categories
#' @param variables one or more variables
#' @param grouping options are category and variable
#' @param predictor_type options are baseline, rolling, or growing
#' @param n_ids number of IDs to include in plot
#' @param n_breaks a number of histogram breaks
#'
#' @return plot
#' @export
#'
#' @examples wiz_viz_ohlc(wiz_frame, variables = "cr", predictor_type = "baseline", n_ids = 1, n_breaks = 12)
wiz_viz_ohlc = function(wiz_frame, categories = NULL, variables = NULL, grouping = "variable", predictor_type = "rolling", n_ids = 5, n_breaks = 10) {

  # grouping = "category"

  # identify left and right time limits to include
  if(predictor_type == "baseline") {
    timespan_min = min(wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    # timespan_min = lookback # first prediction minus lookback convert by step_unit
    timespan_max = 0
  } else if (predictor_type == "growing") {
    timespan_min = 0
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  } else { # default
    timespan_min = min(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
    timespan_max = max(wiz_frame$temporal_data %>%
                         pull(!!rlang::parse_expr(wiz_frame$temporal_time)))
  }

  # set number of IDs to sample
  if(is.null(n_ids)) {
    n_ids = wiz_frame$temporal_data %>% pull(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      length()
  }

  if(!is.null(categories)) {
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(categories, !!rlang::parse_expr(wiz_frame$temporal_category)))
  } else if (!is.null(variables)) {
    # grouping = "variable"
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max,
             grepl(variables, !!rlang::parse_expr(wiz_frame$temporal_variable)))
  } else { # default
    sampled_data = wiz_frame$temporal_data %>%
      select(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      sample_n(n_ids) %>%
      inner_join(wiz_frame$temporal_data) %>%
      filter(!!rlang::parse_expr(wiz_frame$temporal_time) >= timespan_min,
             !!rlang::parse_expr(wiz_frame$temporal_time) <= timespan_max)
  }

  if(grouping == "category") {
    grouping_var = wiz_frame$temporal_category
  } else if (grouping == "variable") {
    grouping_var = wiz_frame$temporal_variable
  } else { # default
    grouping_var = wiz_frame$temporal_variable
  }

  # calculate breaks and midpoints using histogram
  h1 = sampled_data %>%
    pull(!!rlang::parse_expr(wiz_frame$temporal_time)) %>% hist(breaks = n_breaks, plot = FALSE)

  sampled_data %>%
    mutate(value = !!rlang::parse_expr(wiz_frame$temporal_value) %>% as.numeric(),
           time_bin = !!rlang::parse_expr(wiz_frame$temporal_time) %>% as.numeric() %>% cut(breaks = h1$breaks, labels = h1$mids)) %>%
    na.omit() %>%
    mutate(time_bin = time_bin %>% as.character() %>% as.numeric()) %>%
    group_by(!!rlang::parse_expr(wiz_frame$temporal_id), !!rlang::parse_expr(grouping_var), time_bin) %>%
    summarize(!!rlang::parse_expr(wiz_frame$temporal_id),
              !!rlang::parse_expr(grouping_var), time_bin,
              low = min(value), high = max(value),
              open = first(!!rlang::parse_expr(wiz_frame$temporal_value)), close = last(value),
              change = if_else(close > open, "up", "dn"), .groups = "keep") %>%
    ggplot(aes(x=time_bin, colour = change)) +
    theme_bw() +
    geom_linerange(aes(ymin=low, ymax=high)) +
    geom_segment(aes(y = open, yend = open, xend = time_bin - (h1$mids[2] - h1$mids[1])/3)) +
    geom_segment(aes(y = close, yend = close, xend = time_bin + (h1$mids[2] - h1$mids[1])/3)) +
    scale_colour_manual(values = c("dn" = "darkred", "up" = "darkgreen")) + guides(colour = FALSE) +
    ylab("value") +
    xlab(paste0("time midpoint in ", wf$step_units, "s")) +
    scale_y_continuous() +
    scale_x_continuous(breaks = h1$mids, labels = h1$mids) +
    facet_grid(rows = vars(!!rlang::parse_expr(grouping_var)))#,
  # cols = vars(!!rlang::parse_expr(grouping_var)))
}
