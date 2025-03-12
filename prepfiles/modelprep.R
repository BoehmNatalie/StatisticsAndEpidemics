library(dplyr)
library(data.table)
library(epinowcast)
library(tidyr)
library(ggplot2)


###############################################################################
###############################################################################
######################## function for modelusage###############################
###############################################################################
###############################################################################
combine_epinow_results <- function(start_date, end_date, df, locationfilter, removed_days, included_days, maxed_delay) {

  combined_ergebnis <- data.frame()
  combined_wis <- data.frame()
  combined_final_summary <- data.frame()
  combined_df_truth_filtered <- data.frame()
  combined_df_nowcast <- data.frame()
  
  current_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  while (current_date <= end_date) {
    nowcast_try <- epinow_einfach(
      df = df,
      latest_date = current_date,
      locationfilter = locationfilter,
      removed_days = removed_days,
      included_days = included_days,
      maxed_delay = maxed_delay
    )
    
    ergebnis_t <- nowcast_try[["ergebnis"]]
    wis <- nowcast_try[["wis"]]
    final_summary_t <- nowcast_try[["final_summary"]]
    df_truth_t <- nowcast_try[["df_truth_filtered"]]
    df_nowcast_t <- nowcast_try[["df_nowcast"]]
    
    combined_ergebnis <- rbind(combined_ergebnis, ergebnis_t)
    combined_wis <- rbind(combined_wis, wis)
    combined_final_summary <- rbind(combined_final_summary, final_summary_t)
    combined_df_truth_filtered <- rbind(combined_df_truth_filtered, df_truth_t)
    combined_df_nowcast <- rbind(combined_df_nowcast, df_nowcast_t)
    
    current_date <- current_date + 1
  }
  
  return(list(
    combined_ergebnis = combined_ergebnis,
    combined_wis = combined_wis,
    combined_final_summary = combined_final_summary,
    combined_df_truth_filtered = combined_df_truth_filtered,
    combined_df_nowcast = combined_df_nowcast
  ))
}
###############################################################################
###############################################################################
######################## function for one day   ###############################
###############################################################################
###############################################################################
# followed : https://package.epinowcast.org/articles/epinowcast.html
epinow_einfach <- function(df,latest_date,locationfilter,removed_days,included_days,maxed_delay){

  
  ##################e## fill dataset for every combi ####################
  
  all_combinations <- df %>%
    select(geoRegion, date_hospitalization) %>%
    distinct() %>%
    mutate(date_report = date_hospitalization)
  
  df_complete <- all_combinations %>%
    anti_join(df, by = c("geoRegion", "date_hospitalization", "date_report")) %>% 
    mutate(total = 0) %>%                                           
    bind_rows(df) %>%                                                
    arrange(geoRegion, date_hospitalization, date_report)                     
  
  #################### rename columns and dataset ####################
  
  setnames(df_complete, old = c("date_hospitalization", "date_report", "geoRegion", "total"),
           new = c("reference_date", "report_date", "location", "confirm"))
  
  swiss_covid19_hosp <-df_complete
  
  #################### decide which data to use ####################
  
  nat_swiss_hosp <- swiss_covid19_hosp %>%
    filter(as.character(location) == locationfilter) %>%
    enw_filter_report_dates(latest_date = latest_date)
  
  #################### complete Data ####################
  
  
  nat_swiss_hosp <- enw_complete_dates(
    nat_swiss_hosp,
    by = c("location")
  )
  
  #################### choose information ####################
  
  retro_nat_swiss <- nat_swiss_hosp |>
    enw_filter_report_dates(remove_days = removed_days) |> 
    enw_filter_reference_dates(include_days = included_days)
  retro_nat_swiss
  
  #################### create reporting triangle ####################
  
  pobs_swiss <- enw_preprocess_data(retro_nat_swiss, max_delay = maxed_delay)
  pobs_swiss
  
  #################### create Nowcast (wochentagseffekt) ####################
  
  expectation_module <- enw_expectation(
    ~ 0 + (1 | day), data = pobs_swiss
  )
  reference_module <- enw_reference(~1, distribution = "lognormal", data = pobs_swiss)
  report_module <- enw_report(~ (1 | day_of_week), data = pobs_swiss)
  model <- enw_model(threads = TRUE)
  options(mc.cores = 1)
  nowcast <- epinowcast(data = pobs_swiss,
                        expectation = expectation_module,
                        reference = reference_module,
                        report = report_module,
                        fit = enw_fit_opts(
                          save_warmup = FALSE, pp = TRUE,
                          chains = 2, threads_per_chain = 2,
                          iter_sampling = 500, iter_warmup = 500,
                          show_messages = FALSE
                        ),
                        model = model
  )
  
  #################### Daten aus Nowcast ziehen ####################
  
  summary_nowcast<-summary(nowcast, type = "nowcast")
  
  #################### Daten aus Nowcast ziehen mit anderen Quantilen ####################
  
  summary_nowcast<-summary(nowcast, type = "nowcast")
  
  #################### change data for compute_scores ####################
  
  modified_summary <- summary_nowcast %>%
    gather(key = "quantile", value = "value", starts_with("q")) %>%
    mutate(
      location = location, 
      forecast_date = report_date,  
      target_end_date = reference_date, 
      type = "quantile",  
      pathogen = "COVID-19",  
      model = "Epiforecast",  
      retrospective = FALSE 
    ) %>%
    mutate(quantile = as.numeric(gsub("q", "", quantile)) / 100) %>%  
    select(location, forecast_date, target_end_date, type, quantile, value, pathogen, model, retrospective)  
  
  #################### add median  ####################
  
  modified_summary_median <- summary_nowcast %>%
    select(location, reference_date, report_date, median) %>%
    mutate(
      forecast_date = report_date,  
      target_end_date = reference_date,  
      type = "median",  
      quantile = 0.50, 
      value = median, 
      pathogen = "COVID-19",  
      model = "Epiforecast", 
      retrospective = FALSE
    ) %>%
    select(location, forecast_date, target_end_date, type, quantile, value, pathogen, model, retrospective)  
  
  modified_summary_mean <- summary_nowcast %>%
    select(location, reference_date, report_date, mean) %>% # Annahme: "mean" ist im Datensatz vorhanden
    mutate(
      forecast_date = report_date,  
      target_end_date = reference_date,  
      type = "mean",  
      quantile = NA,  # Mean hat keinen zugeordneten Quantilwert
      value = mean, 
      pathogen = "COVID-19",  
      model = "Epiforecast", 
      retrospective = FALSE
    ) %>%
    select(location, forecast_date, target_end_date, type, quantile, value, pathogen, model, retrospective)
  
  final_summary <- bind_rows(modified_summary,modified_summary_median,modified_summary_mean)
  
  tail(final_summary)
  
  #################### add real values ####################
  
  source("prepfiles/truth_erstellen.R")
  df_truth <- truth_erstellen(df,maxed_delay)
  
  final_summary <- final_summary %>%
    mutate(target_end_date = as.Date(target_end_date)) %>%
    left_join(df_truth, by = c("location", "target_end_date" = "date"))
  
  #################### compute scores ####################
  
  final_summary <- final_summary %>%
    mutate(quantile = as.numeric(quantile))
  
  ergebnis <- compute_scores(final_summary)
  
  #################### compute WIS ####################
  
  wis_ergebnis <- compute_wis(final_summary)
  
  #################### plot ####################
  df_truth_filtered <- final_summary %>%
    select(truth, target_end_date)
  
  df_nowcast <- summary_nowcast

  return(list(ergebnis=ergebnis,wis=wis_ergebnis,final_summary=final_summary,df_truth_filtered =df_truth_filtered,df_nowcast = df_nowcast))
}
plot_nowcast <-function(df_truth_filtered,df_nowcast){
  unique_dates <- unique(df_nowcast$report_date)
  ggplot(df_truth_filtered) +
    geom_ribbon(
      data = df_nowcast,
      aes(x = reference_date, ymin = q5, ymax = q95, fill = "95% interval"),
      alpha = 0.8
    ) +
    geom_ribbon(
      data = df_nowcast,
      aes(x = reference_date, ymin = q20, ymax = q80, fill = "50% interval"), 
      alpha = 0.95
    ) +
    
    geom_line(
      data = df_nowcast, aes(x = reference_date, y = median, color = "Median"), 
      linewidth = 0.8
    ) +
    geom_line(
      aes(x = target_end_date, y = truth, color = "Truth"), 
      linewidth = 0.6
    ) +
    
    geom_vline(
      data = data.frame(forecast_date = unique_dates),
      aes(xintercept = forecast_date, linetype = "Date of nowcast"),
      color = "skyblue4", linewidth = 0.8
    ) +
    
    labs(
      x = NULL,
      y = "hospitalization"
    ) +
    
    scale_color_manual(
      values = c("Median" = "skyblue3", "Truth" = "black"),  
      name = "Line Type"
    ) +
    
    scale_fill_manual(
      values = c("95% interval" = "lightblue", "50% interval" = "skyblue"),  
      name = "Interval"
    ) +
    
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "#f4f4f4"), 
      plot.background = element_rect(fill = "white")  
    )
}
###############################################################################
############################### needed functions ##############################
###############################################################################

summary.epinowcast <- function(object, type = c(
  "nowcast", "nowcast_samples",
  "fit", "posterior_prediction"
), max_delay = object$max_delay, ...) {
  type <- match.arg(type)
  arg_max_delay <- max_delay 
  
  s <- with(object, switch(type,
                           nowcast = enw_nowcast_summary(
                             fit = fit[[1]], obs = latest[[1]], max_delay = arg_max_delay,
                             timestep = timestep[[1]]
                           ),
                           nowcast_samples = enw_nowcast_samples(
                             fit = fit[[1]], obs = latest[[1]], max_delay = arg_max_delay,
                             timestep = timestep[[1]], ...
                           ),
                           fit = enw_posterior(fit[[1]], ...),
                           posterior_prediction = enw_pp_summary(fit[[1]], new_confirm[[1]], ...),
                           cli::cli_abort("unimplemented type: {type}")
  ))
  
  return(s)
}

check_timestep_by_group <- function(obs, date_var, timestep = "day",
                                    exact = TRUE) {
  # Coerce to data.table and check for required columns
  obs <- coerce_dt(obs, required_cols = date_var, copy = FALSE, group = TRUE)
  
  # Check the timestep within each group
  obs[,
      check_timestep(
        .SD, date_var = date_var, timestep, exact, check_nrow = FALSE),
      by = ".group"
  ]
  
  return(invisible(NULL))
}
check_timestep <- function(obs, date_var, timestep = "day", exact = TRUE,
                           check_nrow = TRUE) {
  obs <- coerce_dt(obs, required_cols = date_var, copy = FALSE)
  if (!is.Date(obs[[date_var]])) {
    cli::cli_abort("{date_var} must be of class Date")
  }
  
  dates <- obs[[date_var]]
  dates <- sort(dates)
  dates <- dates[!is.na(dates)]
  
  if (length(dates) <= 1) {
    if (check_nrow) {
      cli::cli_abort("There must be at least two observations")
    } else {
      return(invisible(NULL))
    }
  }
  
  internal_timestep <- get_internal_timestep(timestep)
  
  if (internal_timestep == "month") {
    check_calendar_timestep(dates, date_var, exact)
  } else {
    check_numeric_timestep(dates, date_var, internal_timestep, exact)
  }
  
  return(invisible(NULL))
}
check_numeric_timestep <- function(dates, date_var, timestep, exact = TRUE) {
  diffs <- as.numeric(
    difftime(dates[-1], dates[-length(dates)], units = "days")
  )
  
  if (any(diffs == 0)) {
    cli::cli_abort(
      "{date_var} has a duplicate date. Please remove duplicate dates."
    )
  }
  
  if (any(diffs < timestep)) {
    cli::cli_abort(
      paste0(
        "{date_var} has a shorter timestep than the specified timestep of ",
        "{timestep} day(s)"
      )
    )
  }
  
  if (exact) {
    check <- all(diffs == timestep)
  } else {
    check <- sum(diffs %% timestep) == 0
  }
  
  if (check) {
    return(invisible(NULL))
  } else {
    cli::cli_abort(
      "{date_var} does not have the specified timestep of {timestep} day(s)"
    )
  }
}
###############################################################################
###############################################################################
########## score computation according to ##########
#https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation
###############################################################################
###############################################################################
compute_scores <- function(df) {
  df <- df %>%
    rowwise() %>%
    mutate(score = score(value, truth, type, quantile),
           score = round(score, digits = 5)) %>% 
    select(-c(pathogen, value, truth))
}
score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    if (is.na((prediction - observation)^2)){
    }
    return((prediction - observation)^2)
  } else if (type == "median") {
    if (is.na(abs(prediction - observation))){
    }
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    if (is.na(abs(prediction - observation))){
    }
    return(qs(prediction, observation, quantile))
  }
}
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
}

compute_wis <- function(df) {
  unique_dates <- unique(df$forcast_date)
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    rename(med = value) %>%
    select(-any_of(c("quantile", "pathogen", "retrospective", "truth")))
  df_median <- df_median %>%
    filter(type == "quantile")
  df <- df %>%
    filter(type == "quantile") %>%
    left_join(df_median, by = c("forecast_date", "target_end_date","location"))    %>%
    select(
      forecast_date,
      target_end_date,
      location,
      type = type.x, 
      quantile,
      value,
      pathogen,
      retrospective,
      model = model.x,
      truth,
      med,
    )

  df <- df %>%
    rowwise() %>%
    mutate(
      score = score(value, truth, type, quantile),
      spread = score(value, med, type, quantile),
      overprediction = ifelse(med > truth, score - spread, 0),
      underprediction = ifelse(med < truth, score - spread, 0)
    )
  df_safe<-df
  
  
  df <- df %>%
    group_by(model) %>%
    summarize(
      target_end_date = unique_dates,  
      spread = mean(spread),
      overprediction = mean(overprediction),
      underprediction = mean(underprediction),
      score = mean(score)
    )
  
  return(df)
}


###############################################################################
###############################################################################
############## all relevant function from the Epinowcast code #################
#https://github.com/epinowcast/epinowcast/tree/main/R
############## that should be run one time before usage #######################
###############################################################################
###############################################################################

#####utils#####
#' @rawNamespace import(data.table, except = transpose)
#' @import cmdstanr
#' @import ggplot2
#' @importFrom stats median rnorm
NULL

#' @title Check an object is a Date
#' @description Checks that an object is a date
#' @param x An object
#' @return A logical
#' @family utils
is.Date <- function(x) {
  # nolint
  inherits(x, "Date")
}

stan_fns_as_string <- function(files, include) {
  functions <- paste0(
    "\n functions{ \n",
    paste(
      purrr::map_chr(
        files,
        ~ paste(readLines(file.path(include, .)), collapse = "\n")
      ),
      collapse = "\n"
    ),
    "\n }"
  )
  return(functions)
}


enw_example <- function(type = c(
  "nowcast", "preprocessed_observations",
  "observations", "script"
)) {
  type <- match.arg(type)
  
  if (type %in% c("nowcast", "preprocessed_observations", "observations")) {
    return(readRDS(
      system.file("extdata", sprintf("%s.rds", type), package = "epinowcast")
    ))
  } else if (type == "script") {
    return(
      system.file("examples", "germany_dow.R", package = "epinowcast")
    )
  }
}

coerce_date <- function(dates = NULL) {
  if (is.null(dates)) {
    return(data.table::as.IDate(numeric()))
  }
  if (length(dates) == 0) {
    return(data.table::as.IDate(dates))
  }
  
  res <- data.table::as.IDate(vapply(dates, function(d) {
    tryCatch(
      data.table::as.IDate(d, optional = TRUE),
      error = function(e) {
        return(data.table::as.IDate(NA))
      }
    )
  }, FUN.VALUE = data.table::as.IDate(0)))
  
  if (anyNA(res)) {
    cli::cli_abort(paste0(
      "Failed to parse with `as.IDate`: {toString(dates[is.na(res)])} ",
      "(indices {toString(which(is.na(res)))})."
    ))
  } else {
    return(res)
  }
}

get_internal_timestep <- function(timestep) {
  # check if the input is a character
  if (is.character(timestep)) {
    switch(
      timestep,
      day = 1,
      week = 7,
      month = "month",  # months are not a fixed number of days
      cli::cli_abort(
        "Invalid timestep. Acceptable string inputs are 'day', 'week', 'month'."
      )
    )
  } else if (is.numeric(timestep) && timestep == round(timestep)) {
    # check if the input is a whole number
    return(timestep)
  } else {
    cli::cli_abort(
      paste0(
        "Invalid timestep. If timestep is a numeric, it should be a whole ",
        "number representing the number of days."
      )
    )
  }
}


aggregate_rolling_sum <- function(dt, internal_timestep, by = NULL) {
  dt <- dt[,
           `:=`(
             confirm = {
               n_vals <- if (.N <= internal_timestep) {
                 seq_len(.N)
               } else {
                 c(
                   1:(internal_timestep - 1),
                   rep(internal_timestep, .N - (internal_timestep - 1))
                 )
               }
               frollsum(confirm, n_vals, adaptive = TRUE)
             }
           ),
           by = by
  ]
  return(dt[])
}


date_to_numeric_modulus <- function(dt, date_column, timestep) {
  mod_col_name <- paste0(date_column, "_mod")
  
  dt[, c(mod_col_name) := as.numeric(
    get(date_column) - min(get(date_column), na.rm = TRUE)
  ) %% timestep
  ]
  return(dt[])
}

cache_location_message <- function() {
  cache_location <- Sys.getenv("enw_cache_location")
  if (check_environment_unset(cache_location)) {
    # nolint start
    msg <- c(
      "!" = "`enw_cache_location` is not set.",
      i = "Using `tempdir()` at {tempdir()} for the epinowcast model cache location.",
      i = "Set a specific cache location using `enw_set_cache` to control Stan recompilation in this R session or across R sessions.",
      i = "For example: `enw_set_cache(tools::R_user_dir(package =
            \"epinowcast\", \"cache\"), type = c('session', 'persistent'))`.",
      i = "See `?enw_set_cache` for details."
    )
    # nolint end 
  } else {
    msg <- c(
      i = sprintf(
        "Using `%s` for the epinowcast model cache location.", # nolint line_length
        cache_location
      )
    )
  }
  
  return(msg)
}

check_environment_unset <- function(x) {
  return(is.null(x) || x == "")
}

get_renviron_contents <- function() {
  
  env_location <- getwd()
  
  if (file.exists(file.path(env_location, ".Renviron"))) {
    env_path <- file.path(env_location, ".Renviron")
  } else {
    env_location <- Sys.getenv("HOME")
    env_path <- file.path(env_location, ".Renviron")
  }
  
  if (!file.exists(env_path)) {
    file.create(env_path)
  }
  
  env_contents <- readLines(env_path)
  
  output <- list(
    env_contents = env_contents,
    env_path = env_path
  )
  
  return(output)
}

unset_cache_from_environ <- function(alert_on_not_set = TRUE) {
  environ <- get_renviron_contents()
  cache_loc_environ <- check_renviron_for_cache(environ)
  if (any(cache_loc_environ)) {
    new_environ <- environ
    new_environ[["env_contents"]] <-
      environ[["env_contents"]][!cache_loc_environ]
    writeLines(new_environ$env_contents, new_environ$env_path)
    cli::cli_alert_success(
      "Removed `enw_cache_location` setting from `.Renviron`."
    )
  } else {
    if (isTRUE(alert_on_not_set)) {
      cli::cli_alert_danger(
        "`enw_cache_location` not set in `.Renviron`. Nothing to remove."
      )
    }
  }
  return(invisible(NULL))
}

check_renviron_for_cache <- function(environ) {
  cache_loc_environ <- grepl(
    "enw_cache_location", environ[["env_contents"]], fixed = TRUE
  )
  return(cache_loc_environ)
}


create_cache_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(path)) {
      cli::cli_alert_success(
        "Created cache directory at {path}"
      )
      return(invisible(NULL))
    } else {
      cli::cli_abort(
        "Failed to create cache directory at {path}"
      )
    }
  }
  return(invisible(NULL))
}

dir_create_with_parents <- function(path) {
  dirs <- strsplit(path, "/+")[[1]]
  for (i in seq_along(dirs)) {
    path <- paste(dirs[seq_len(i)], collapse = "/")
    if (!dir.exists(path) && nzchar(path)) {
      dir.create(path)
    }
  }
}


enw_posterior <- function(fit, variables = NULL,
                          probs = c(0.05, 0.2, 0.8, 0.95), ...) {
  # order probs
  probs <- sort(probs, na.last = TRUE)
  
  # extract summary parameters of interest and join
  sfit <- list(
    fit$summary(
      variables = variables, mean, median, sd, mad,
      .args = list(na.rm = TRUE), ...
    ),
    fit$summary(
      variables = variables, quantile2,
      .args = list(probs = probs, na.rm = TRUE),
      ...
    ),
    fit$summary(
      variables = variables, posterior::default_convergence_measures(), ...
    )
  )
  cbind_custom <- function(x, y) {
    x <- data.table::setDT(x)
    y <- data.table::setDT(y)[, variable := NULL]
    cbind(x, y)
  }
  sfit <- purrr::reduce(sfit, cbind_custom)
  return(sfit[])
}

enw_nowcast_summary <- function(fit, obs, max_delay = NULL, timestep = "day",
                                probs = c(
                                  0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95
                                )) {
  nowcast <- enw_posterior(
    fit,
    variables = "pp_inf_obs",
    probs = probs
  )
  
  max_delay_model <- nrow(nowcast) / max(obs$.group)
  if (is.null(max_delay)) {
    max_delay <- max_delay_model
  }
  if (max_delay < max_delay_model) {
    cli::cli_abort(paste0(
      "The specified maximum delay must be equal to or larger than ",
      "the modeled maximum delay."
    ))
  }
  
  internal_timestep <- get_internal_timestep(timestep)
  
  ord_obs <- build_ord_obs(obs, max_delay, internal_timestep,
                           timestep)
  
  # add observations for modelled dates
  obs_model <- subset_obs(ord_obs, max_delay_model, internal_timestep,
                          reference_subset = ">")
  nowcast <- cbind(obs_model, nowcast)
  
  # add not-modelled earlier dates with artificial summary statistics
  if (max_delay > max_delay_model) {
    obs_spec <- subset_obs(ord_obs, max_delay_model,
                           internal_timestep, reference_subset = "<=")
    nowcast <- rbind(obs_spec, nowcast, fill = TRUE)
    nowcast[seq_len(nrow(obs_spec)), c("mean", "median") := confirm]
    cols_quantile <- grep("q\\d+", colnames(nowcast), value = TRUE) # nolint
    nowcast[seq_len(nrow(obs_spec)), (cols_quantile) := confirm]
    nowcast[seq_len(nrow(obs_spec)), c("sd", "mad") := 0]
  }
  
  data.table::setorderv(nowcast, c(".group", "reference_date"))
  nowcast[, variable := NULL]
  return(nowcast[])
}

enw_nowcast_samples <- function(fit, obs, max_delay = NULL, timestep = "day") {
  nowcast <- fit$draws(
    variables = "pp_inf_obs",
    format = "draws_df"
  )
  nowcast <- coerce_dt(
    nowcast,
    required_cols = c(".chain", ".iteration", ".draw")
  )
  nowcast <- melt(
    nowcast,
    value.name = "sample", variable.name = "variable",
    id.vars = c(".chain", ".iteration", ".draw")
  )
  
  max_delay_model <- nrow(nowcast) / max(obs$.group) / max(nowcast$.draw,
                                                           na.rm = TRUE)
  if (is.null(max_delay)) {
    max_delay <- max_delay_model
  }
  if (max_delay < max_delay_model) {
    cli::cli_abort(paste0(
      "The specified maximum delay must be equal to or larger than ",
      "the modeled maximum delay."
    ))
  }
  
  internal_timestep <- get_internal_timestep(timestep)
  
  ord_obs <- build_ord_obs(obs, max_delay, internal_timestep, timestep,
                           nowcast)
  
  # add observations for modelled dates
  obs_model <- subset_obs(ord_obs, max_delay_model, internal_timestep,
                          reference_subset = ">")
  
  nowcast <- cbind(obs_model, nowcast)
  
  # add artificial samples for not-modelled earlier dates
  if (max_delay > max_delay_model) {
    obs_spec <- subset_obs(ord_obs, max_delay_model,
                           internal_timestep, reference_subset = "<=")
    obs_spec[, c(".chain", ".iteration") := NA]
    obs_spec[, .draw := rep(1:max(nowcast$.draw, na.rm = TRUE),
                            nrow(obs_spec) / max(nowcast$.draw, na.rm = TRUE))]
    obs_spec[, variable := NA]
    obs_spec[, sample := confirm]
    nowcast <- rbind(obs_spec, nowcast, fill = TRUE)
  }
  
  data.table::setorderv(nowcast, c(".group", "reference_date"))
  nowcast[, variable := NULL][, .draws := NULL]
  return(nowcast[])
}

enw_summarise_samples <- function(samples, probs = c(
  0.05, 0.2, 0.35, 0.5,
  0.65, 0.8, 0.95
),
by = c("reference_date", ".group"),
link_with_obs = TRUE) {
  obs <- samples[.draw == min(.draw, na.rm = TRUE)]
  suppressWarnings(obs[, c(".draw", ".iteration", "sample", ".chain") := NULL])
  
  summary <- samples[,
                     .(
                       mean = mean(sample),
                       median = median(sample),
                       sd = sd(sample),
                       mad = posterior::mad(sample)
                     ),
                     by = by
  ]
  
  quantiles <- unique(samples[, c(..by, "sample")][,
                                                   paste0("q", probs * 100) := as.list(
                                                     quantile(sample, probs, na.rm = TRUE)
                                                   ),
                                                   by = by
  ][, sample := NULL])
  
  dts <- list(summary, quantiles)
  if (link_with_obs) {
    dts <- c(list(obs), dts)
  }
  summary <- purrr::reduce(dts, merge, by = by)
  return(summary[])
}


enw_add_latest_obs_to_nowcast <- function(nowcast, obs) {
  obs <- coerce_dt(obs, select = c("reference_date", "confirm"), group = TRUE)
  data.table::setnames(obs, "confirm", "latest_confirm")
  out <- merge(
    nowcast, obs,
    by = c("reference_date", ".group"), all.x = TRUE
  )
  data.table::setcolorder(
    out,
    neworder = c("reference_date", ".group", "latest_confirm", "confirm")
  )
  return(out[])
}

enw_pp_summary <- function(fit, diff_obs,
                           probs = c(
                             0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95
                           )) {
  pp <- enw_posterior(
    fit,
    variables = "pp_obs",
    probs = probs
  )
  
  ord_obs <- coerce_dt(
    diff_obs,  required_cols = "new_confirm", dates = TRUE, group = TRUE
  )
  data.table::setorderv(ord_obs, c(".group", "reference_date"))
  pp <- cbind(
    ord_obs,
    pp
  )
  data.table::setorderv(pp, c(".group", "reference_date"))
  pp[, variable := NULL]
  return(pp[])
}

enw_quantiles_to_long <- function(posterior) {
  posterior <- coerce_dt(posterior)
  long <- melt(posterior,
               measure.vars = patterns("^q[0-9]"),
               value.name = "prediction", variable.name = "quantile"
  )
  long[, quantile := gsub("q", "", quantile, fixed = TRUE)]
  long[, quantile := as.numeric(quantile) / 100]
  return(long[])
}

build_ord_obs <- function(obs, max_delay, internal_timestep, timestep, nowcast = NULL) { # nolint
  ord_obs <- coerce_dt(
    obs, required_cols = c("reference_date", "confirm"), group = TRUE
  )
  check_timestep_by_group(
    ord_obs, "reference_date", timestep, exact = TRUE
  )
  
  ord_obs <- subset_obs(ord_obs, max_delay, internal_timestep,
                        reference_subset = ">")
  
  data.table::setorderv(ord_obs, c(".group", "reference_date"))
  if (!is.null(nowcast)) {
    ord_obs <- data.table::data.table(
      .draws = 1:max(nowcast$.draw),
      obs = rep(list(ord_obs), max(nowcast$.draw))
    )
    ord_obs <- ord_obs[, rbindlist(obs), by = .draws]
    ord_obs <- ord_obs[order(.group, reference_date)]
  }
  return(ord_obs)
}

subset_obs <- function(ord_obs, max_delay, internal_timestep,
                       reference_subset) {
  to_keep <- match.fun(reference_subset)(ord_obs$reference_date,
                                         (max(ord_obs$reference_date,
                                              na.rm = TRUE) -
                                            max_delay * internal_timestep))
  if (!is.logical(to_keep)) {
    stop("reference_subset must be a relational operator")
  }
  return(ord_obs[to_keep, ])
}

check_quantiles <- function(posterior, req_probs = c(0.5, 0.95, 0.2, 0.8)) {
  if (!all(data.table::between(req_probs, 0, 1, incbounds = FALSE))) {
    cli::cli_abort("Please provide probabilities as numbers between 0 and 1.")
  }
  return(coerce_dt(
    posterior,
    required_cols = sprintf("q%g", req_probs * 100), copy = FALSE,
    msg_required = "The following quantiles must be present (set with `probs`):"
  ))
}


check_group <- function(obs) {
  return(coerce_dt(
    obs,
    forbidden_cols = c(".group", ".new_group", ".old_group"), copy = FALSE,
    msg_forbidden = "The following are reserved grouping columns:"
  ))
}


check_group_date_unique <- function(obs) {
  group_cols <- c("reference_date", "report_date", ".group")
  obs <- coerce_dt(obs, required_cols = group_cols, copy = FALSE)
  cells <- obs[, .(count = .N), by = group_cols]
  if (any(cells[, count > 1])) {
    cli::cli_abort(
      paste0(
        "The input data seems to be stratified by more variables than ",
        "specified via the `by` argument. Please provide additional grouping ",
        "variables to `by`, or aggregate the observations beforehand."
      )
    )
  }
  return(invisible(NULL))
}


check_module <- function(module) {
  if (!"data" %in% names(module)) {
    cli::cli_abort(
      paste0(
        "Must contain a list component specifying the data requirements for ",
        "further modelling as a list"
      )
    )
  }
  if (!is.list(module[["data"]])) {
    cli::cli_abort("`data` must be a list of required data")
  }
  return(invisible(NULL))
}


check_modules_compatible <- function(modules) {
  if (
    modules[[4]]$data$model_miss &&
    !modules[[6]]$data$likelihood_aggregation
  ) {
    cli::cli_warn(
      c(
        paste0(
          "Incompatible model specification: A missingness model has been ",
          "specified but likelihood aggregation is specified as by snapshot. ",
          "Switching to likelihood aggregation by group. ",
          "This has no effect on the nowcast but limits the number of threads ",
          "per chain to the number of groups."
        ),
        paste0(
          "To silence this warning, set the `likelihood_aggregation` argument ",
          "in `enw_fit_opts` to 'groups'. "
        )
      ),
      immediate. = TRUE
    )
  }
  return(invisible(NULL))
}

coerce_dt <- function(
    data, select = NULL, required_cols = select,
    forbidden_cols = NULL, group = FALSE,
    dates = FALSE, copy = TRUE,
    msg_required = "The following columns are required: ",
    msg_forbidden = "The following columns are forbidden: "
) {
  if (copy) {
    dt <- data.table::as.data.table(data)
  } else {
    dt <- data.table::setDT(data)
  }
  
  if (dates) {
    required_cols <- c(required_cols, c("report_date", "reference_date"))
    if (length(select) > 0) {
      select <- c(select, c("report_date", "reference_date"))
    }
  }
  
  if ((length(required_cols) > 0)) { # if we have required columns ...
    if (!is.character(required_cols)) { # ... check they are check-able
      cli::cli_abort("`required_cols` must be a character vector")
    }
    # check that all required columns are present
    if (!all(required_cols %in% colnames(dt))) {
      cli::cli_abort(
        paste0(
          "{msg_required}",
          "{toString(required_cols[!(required_cols %in% colnames(dt))])} ",
          "but are not present among ",
          "{toString(colnames(dt))} ",
          "(all {.arg required_cols}: {toString(required_cols)})"
        )
      )
    }
  }
  
  if ((length(forbidden_cols) > 0)) { # if we have forbidden columns ...
    if (!is.character(forbidden_cols)) { # ... check they are check-able
      cli::cli_abort("`forbidden_cols` must be a character vector")
    }
    # check that no forbidden columns are present
    if (any(forbidden_cols %in% colnames(dt))) {
      cli::cli_abort(
        paste0(
          "{msg_forbidden}",
          "{toString(forbidden_cols[forbidden_cols %in% colnames(dt)])}",
          "but are present among",
          "{toString(colnames(dt))}",
          "(all `forbidden_cols`: {toString(forbidden_cols)})"
        )
      )
    }
  }
  
  if (group) { # if we want to ensure a .group column ...
    if (is.null(dt[[".group"]])) { # ... check it's presence
      dt <- dt[, .group := 1] # ... and add it if it's not there
    }
    if (length(select) > 0) { # if we have a select list ...
      select <- c(select, ".group") # ... add ".group" to it
    }
  }
  
  if (dates) {
    dt[
      ,
      c("report_date", "reference_date") := .(
        as.IDate(report_date), as.IDate(reference_date)
      )
    ]
  }
  
  if (length(select) > 0) { # if selecting particular list ...
    return(dt[, .SD, .SDcols = c(select)][])
  } else {
    return(dt[])
  }
}

check_max_delay <- function(data,
                            max_delay = data$max_delay,
                            cum_coverage = 0.8,
                            maxdelay_quantile_outlier = 0.97,
                            warn = TRUE, warn_internal = FALSE) {
  
  if (!is.numeric(max_delay)) {
    cli::cli_abort("`max_delay` must be an integer and not NA")
  }
  max_delay <- as.integer(max_delay)
  if (max_delay < 1) {
    cli::cli_abort("`max_delay` must be greater than or equal to one")
  }
  if (!(cum_coverage > 0 && cum_coverage <= 1)) {
    cli::cli_abort("`cum_coverage` must be between 0 and 1, e.g. 0.8 for 80%.")
  }
  if (!(maxdelay_quantile_outlier > 0 && maxdelay_quantile_outlier <= 1)) {
    cli::cli_abort(
      "`maxdelay_quantile_outlier` must be between 0 and 1, e.g. 0.97 for 97%."
    )
  }
  
  timestep <- data$timestep
  internal_timestep <- get_internal_timestep(timestep)
  daily_max_delay <- internal_timestep * max_delay
  
  obs <- data.table::copy(data$obs[[1]])
  obs[, delay := internal_timestep * delay]
  
  max_delay_obs <- obs[, max(delay, na.rm = TRUE)] + internal_timestep
  if (max_delay_obs < daily_max_delay) {
    warning_message <- c(
      paste0(
        "You specified a maximum delay of ", daily_max_delay, " days, ",
        "but the maximum observed delay is only ", max_delay_obs, " days. "
      ),
      paste0(
        "This is justified if you don't have much data yet (e.g. early ",
        "phase of an outbreak) and expect a longer maximum delay than ",
        "currently observed. epinowcast will then extrapolate the delay ",
        "distribution beyond the observed maximum delay."
      ),
      paste0(
        "Otherwise, we recommend using a shorter maximum delay to speed up ",
        "the nowcasting."
      )
    )
    names(warning_message) <- c("", "*", "*")
    cli::cli_warn(warning_message)
  }
  
  max_delay_ref <-  obs[
    !is.na(reference_date) & cum_prop_reported == 1,
    .(.group, reference_date, delay)
  ]
  data.table::setorderv(max_delay_ref, c(".group", "reference_date", "delay"))
  max_delay_ref <- max_delay_ref[,
                                 .SD[, .(delay = first(delay)), by = reference_date]
  ] # we here assume the same maximum delay for all groups
  
  max_delay_obs_q <- ceiling(
    max_delay_ref[, quantile(delay, maxdelay_quantile_outlier, na.rm = TRUE)]
  ) + 1
  
  # Filter by the user-specified maximum delay with daily resolution
  obs <- enw_filter_delay(obs, max_delay = daily_max_delay, timestep = "day")
  
  # filter by earliest observed report date
  obs <- obs[,
             .SD[reference_date >= min(report_date) | is.na(reference_date)],
             by = .group
  ]
  
  latest_obs <- enw_latest_data(obs)
  fully_observed_date <- latest_obs[, max(report_date)] - max_delay_obs_q + 1
  # filter by the maximum observed delay to reduce right truncation bias
  latest_obs <- enw_filter_reference_dates(
    latest_obs,
    latest_date = fully_observed_date
  )
  
  if (warn && !(max_delay_obs < daily_max_delay) && (latest_obs[, .N] < 5)) {
    warning_message <- c(
      paste0(
        "The coverage of the specified maximum delay could not be ",
        "reliably checked."
      ),
      "*" = paste0(
        "There are only very few (", latest_obs[, .N], ") reference dates",
        " that are sufficiently far in the past (more than ",
        max_delay_obs_q, " days) to compute coverage statistics for the ",
        "maximum delay. "
      )
    )
    if (warn_internal) {
      warning_message <- c(
        warning_message,
        "*" = paste0(
          "You can test different maximum delays and obtain coverage ",
          "statistics using the function ",
          "{.help [check_max_delay()](epinowcast::check_max_delay)}."
        )
      )
    } else {
      warning_message <- c(
        warning_message,
        "*" = paste0(
          "If you think the truncation threshold of ", max_delay_obs_q, " ",
          "days is based on an outlier, and the true maximum delay is likely ",
          "shorter, you can decrease `maxdelay_quantile_outlier` to ",
          "silence this warning."
        )
      )
    }
    cli::cli_warn(warning_message)
  }
  
  low_coverage <- latest_obs[, .(
    below_coverage =
      sum(cum_prop_reported < cum_coverage, na.rm = TRUE) /
      sum(!is.na(cum_prop_reported))
  ), by = .group]
  mean_coverage <- low_coverage[, mean(below_coverage)]
  
  if (warn && mean_coverage > 0.5) {
    cli::cli_warn(paste0(
      "The specified maximum reporting delay ",
      "(", daily_max_delay, " days) ",
      "covers less than ", 100 * cum_coverage,
      "% of cases for the majority (>50%) of reference dates. ",
      "Consider using a larger maximum delay to avoid potential model ",
      "misspecification."
    ),
    immediate. = TRUE
    )
  }
  
  low_coverage <- rbind(low_coverage, list("all", mean_coverage))
  low_coverage[, coverage := cum_coverage]
  data.table::setcolorder(low_coverage, c(".group", "coverage"))
  return(low_coverage[])
}

check_calendar_timestep <- function(dates, date_var, exact = TRUE) {
  diff_dates <- dates[-1] %m-% months(1L)
  sequential_dates <- dates[-length(dates)] == diff_dates
  all_sequential_dates <- all(sequential_dates)
  
  if (any(diff_dates < dates[-length(dates)])) {
    cli::cli_abort(
      "{date_var} has a shorter timestep than the specified timestep of a month"
    )
  }
  
  if (all_sequential_dates) {
    return(invisible(NULL))
  } else {
    if (exact) {
      cli::cli_abort("{date_var} does not have the specified timestep of month")
    } else {
      cli::cli_abort(
        "Non-sequential dates are not currently supported for monthly data"
      )
    }
  }
}




check_timestep_by_date <- function(obs, timestep = "day", exact = TRUE) {
  obs <- coerce_dt(obs, copy = TRUE, dates = TRUE, group = TRUE)
  cnt_obs_rep <- obs[, .(.N), by = c("report_date", ".group")]
  cnt_obs_ref <- obs[, .(.N), by = c("reference_date", ".group")]
  if (all(cnt_obs_rep$N <= 1) || all(cnt_obs_ref$N <= 1)) {
    cli::cli_abort(
      paste0(
        "There must be at least two observations by group and date",
        " combination to establish a timestep"
      )
    )
  }
  obs[,
      check_timestep(
        .SD, date_var = "report_date", timestep, exact, check_nrow = FALSE
      ),
      by = c("reference_date", ".group")
  ]
  obs[,
      check_timestep(
        .SD, date_var = "reference_date", timestep, exact, check_nrow = FALSE
      ),
      by = c("report_date", ".group")
  ]
  return(invisible(NULL))
}

check_observation_indicator <- function(
    new_confirm, observation_indicator = NULL
) {
  if (!is.null(observation_indicator) &&
      !is.logical(new_confirm[[observation_indicator]])) {
    cli::cli_abort("observation_indicator must be a logical")
  }
  return(invisible(NULL))
}

check_design_matrix_sparsity <- function(matrix, sparsity_threshold = 0.9,
                                         min_matrix_size = 50,
                                         name = "checked") {
  if (length(matrix) < min_matrix_size) {
    return(invisible(NULL))
  }
  
  zero_proportion <- sum(matrix == 0) / length(matrix)
  
  if (zero_proportion > sparsity_threshold) {
    cli::cli_alert_info(
      c(
        "The {name} design matrix is sparse (>{sparsity_threshold*100}% ",
        "zeros). Consider using `sparse_design = TRUE` in `enw_fit_opts()` ",
        "to potentially reduce memory usage and computation time."
      )
    )
  }
  
  return(invisible(NULL))
}

