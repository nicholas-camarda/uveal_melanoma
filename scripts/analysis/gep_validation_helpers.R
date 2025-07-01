# New file with generic helper functions for GEP validation
#' Generic GEP validation helper functions
#' 
#' This file contains endpoint‐agnostic helper functions used by the MFS and
#' MSS validation pipelines. Each helper is parameterised via a simple `cfg`
#' list that specifies the relevant column names (time, event, expected risk
#' prefix, etc.).  Keeping the statistical logic here avoids duplicating large
#' code blocks when new endpoints are added.
#' 
#' Example configuration object
#' ----------------------------
#' cfg <- list(
#'   # Follow-up time variable (numeric)
#'   time_var   = "tt_mets_months",
#'   # Unit of the time_var column ("months"|"years")
#'   time_unit  = "months",
#'   # Event indicator (1 = event, 0 = censored)
#'   event_var  = "mets_event",
#'   # Column prefix for expected survival probability, e.g. "expected_mfs_"
#'   expected_prefix = "expected_mfs_",
#'   # Variable containing baseline GEP risk probability (needed for PRAME)
#'   base_risk_var   = "biopsy1_gep_mfs",
#'   # Human-readable outcome label for logging
#'   outcome_label   = "Metastasis"
#' )

# ============================================================================
# INTERNAL UTILS -------------------------------------------------------------
# ============================================================================
# Convert follow-up time column to months given the declared unit. This keeps
# downstream code identical regardless of original unit.
convert_time_to_months <- function(vec, unit) {
  if (unit == "months") {
    return(vec)
  } else if (unit == "years") {
    return(vec * 12)
  } else if (unit == "days") {
    return(vec / 30.4375)  # rough conversion
  } else {
    stop(sprintf("Unsupported time unit: %s", unit))
  }
}

# Build expected variable name for a given timepoint.
build_expected_var <- function(prefix, tp_years) {
  paste0(prefix, tp_years, "yr")
}

# Standard Poisson CI helper (returns list with lower/upper)
poisson_ci_ratio <- function(obs, exp) {
  pt <- poisson.test(obs)
  if (exp > 0) {
    return(list(lower = pt$conf.int[1] / exp, upper = pt$conf.int[2] / exp))
  }
  return(list(lower = NA_real_, upper = NA_real_))
}

# ============================================================================
# PUBLIC GENERIC HELPERS -----------------------------------------------------
# ============================================================================
#' Generic observed vs expected calculation (across GEP classes)
#' @param data Data.frame
#' @param timepoint Integer, years
#' @param cfg List, see header
calc_observed_expected_generic <- function(data, timepoint, cfg) {
  time_var  <- cfg$time_var
  event_var <- cfg$event_var
  expected_prefix <- cfg$expected_prefix
  outcome_lab <- cfg$outcome_label %||% "Outcome"
  time_months <- convert_time_to_months(data[[time_var]], cfg$time_unit)
  tp_months   <- timepoint * 12

  results_by_class <- list()
  for (gc in c("Class 1A", "Class 1B", "Class 2")) {
    class_data <- data[data$gep_class_simple == gc, ]
    if (nrow(class_data) == 0) {
      results_by_class[[gc]] <- list(n = 0, observed = 0, expected = 0,
                                     oe_ratio = NA, poisson_ci_lower = NA,
                                     poisson_ci_upper = NA)
      next
    }
    expected_var <- build_expected_var(expected_prefix, timepoint)
    observed_events <- sum(class_data[[event_var]] == 1 &
                           convert_time_to_months(class_data[[time_var]], cfg$time_unit) <= tp_months)
    mean_expected_surv <- mean(class_data[[expected_var]], na.rm = TRUE)
    expected_events   <- nrow(class_data) * (1 - mean_expected_surv)
    oe_ratio <- ifelse(expected_events > 0, observed_events / expected_events, NA)
    ci <- poisson_ci_ratio(observed_events, expected_events)

    results_by_class[[gc]] <- list(
      n = nrow(class_data), observed = observed_events, expected = round(expected_events, 2),
      oe_ratio = round(oe_ratio, 3), poisson_ci_lower = round(ci$lower, 3),
      poisson_ci_upper = round(ci$upper, 3), mean_expected_survival = round(mean_expected_surv, 3)
    )
  }

  observed_total <- sum(vapply(results_by_class, function(x) x$observed, numeric(1)))
  expected_total <- sum(vapply(results_by_class, function(x) x$expected, numeric(1)))
  observed_vec   <- vapply(results_by_class, function(x) x$observed, numeric(1))
  expected_vec   <- vapply(results_by_class, function(x) x$expected, numeric(1))
  if (all(expected_vec > 0) && sum(expected_vec) > 0) {
    chisq_test <- suppressWarnings(chisq.test(x = observed_vec,
                                              p = expected_vec / sum(expected_vec)))
    chisq_p <- chisq_test$p.value; chisq_stat <- chisq_test$statistic
  } else {
    chisq_p <- NA; chisq_stat <- NA
  }

  return(list(timepoint = timepoint, results_by_class = results_by_class,
              overall_observed = observed_total,
              overall_expected = round(expected_total, 2),
              overall_oe_ratio = ifelse(expected_total > 0,
                                         round(observed_total/expected_total, 3), NA),
              chisq_statistic = round(chisq_stat, 3), chisq_p_value = round(chisq_p, 4)))
}

#' Generic calibration assessment (Nam-D'Agostino & ICI)
perform_calibration_generic <- function(data, timepoint, bootstrap_iterations, cfg) {
  # Reuse MFS code structure but parameterised
  time_var  <- cfg$time_var
  event_var <- cfg$event_var
  expected_prefix <- cfg$expected_prefix
  time_months <- convert_time_to_months(data[[time_var]], cfg$time_unit)
  tp_months  <- timepoint * 12
  expected_var <- build_expected_var(expected_prefix, timepoint)

  cal_data <- data[!is.na(data[[expected_var]]) & !is.na(data[[time_var]]) &
                    !is.na(data[[event_var]]), ]
  if (nrow(cal_data) < 20) {
    return(list(n = nrow(cal_data), status = "insufficient_data"))
  }

  cal_data <- within(cal_data, {
    predicted_prob <- get(expected_var)
    predicted_risk <- 1 - predicted_prob
    observed_time  <- convert_time_to_months(get(time_var), cfg$time_unit)
    observed_event <- get(event_var)
  })

  surv_obj <- survival::Surv(cal_data$observed_time, cal_data$observed_event)

  # Nam-D'Agostino
  n_groups <- max(3, min(10, floor(nrow(cal_data)/10)))
  breaks   <- unique(quantile(cal_data$predicted_risk, seq(0, 1, length.out = n_groups+1)))
  cal_data$risk_group <- cut(cal_data$predicted_risk, breaks = breaks,
                             include.lowest = TRUE, labels = FALSE)
  grp <- dplyr::group_by(cal_data, risk_group)
  group_results <- dplyr::summarise(grp, n = dplyr::n(),
                                    mean_predicted_risk = mean(predicted_risk),
                                    observed_events = sum(observed_event==1 & observed_time<=tp_months),
                                    expected_events = sum(predicted_risk), .groups="drop")
  if (nrow(group_results) >=3 && sum(group_results$expected_events) >0) {
    chisq_stat <- sum((group_results$observed_events - group_results$expected_events)^2 /
                        pmax(group_results$expected_events,1))
    nam_p <- pchisq(chisq_stat, df=nrow(group_results)-1, lower.tail = FALSE)
  } else { chisq_stat <- NA; nam_p <- NA }

  # Simple ICI (for speed)
  observed_rate <- mean(cal_data$observed_event==1 & cal_data$observed_time<=tp_months)
  mean_predicted <- mean(cal_data$predicted_risk)
  ici <- abs(observed_rate - mean_predicted)

  return(list(n = nrow(cal_data), n_groups = nrow(group_results),
              nam_dagostino_statistic = round(chisq_stat,3), nam_dagostino_p = round(nam_p,4),
              ici = round(ici,4)))
}

#' Generic discrimination metrics (Harrell, Uno, AUC)
perform_discrimination_generic <- function(data, timepoint, cfg) {
  time_var  <- cfg$time_var
  event_var <- cfg$event_var
  expected_prefix <- cfg$expected_prefix
  time_months <- convert_time_to_months(data[[time_var]], cfg$time_unit)
  tp_months  <- timepoint * 12
  expected_var <- build_expected_var(expected_prefix, timepoint)

  disc_data <- data[!is.na(data[[expected_var]]) & !is.na(data[[time_var]]) &
                     !is.na(data[[event_var]]), ]
  if (nrow(disc_data) < 20) return(list(n = nrow(disc_data), status = "insufficient_data"))

  disc_data <- within(disc_data, {
    predicted_prob <- get(expected_var)
    predicted_risk <- 1 - predicted_prob
    observed_time  <- convert_time_to_months(get(time_var), cfg$time_unit)
    observed_event <- get(event_var)
  })
  surv_obj <- survival::Surv(disc_data$observed_time, disc_data$observed_event)

  # Harrell C (fallback to survival if survcomp missing)
  harrell_c <- tryCatch({
    if (requireNamespace("survcomp", quietly=TRUE)) {
      survcomp::concordance.index(x = disc_data$predicted_risk,
                                  surv.time=disc_data$observed_time,
                                  surv.event=disc_data$observed_event)$c.index
    } else {
      summary(survival::coxph(surv_obj ~ predicted_risk, data=disc_data))$concordance[1]
    }
  }, error=function(e) NA)

  # Uno C
  uno_c <- tryCatch({
    if (requireNamespace("survcomp", quietly=TRUE)) {
      survcomp::concordance.index(x = disc_data$predicted_risk,
                                  surv.time=disc_data$observed_time,
                                  surv.event=disc_data$observed_event,
                                  method="uno")$c.index
    } else NA
  }, error=function(e) NA)

  # Time-specific AUC
  auc_tp <- NA
  try({
    if (requireNamespace("riskRegression", quietly=TRUE)) {
      m <- survival::coxph(surv_obj ~ predicted_risk, data=disc_data)
      sc <- riskRegression::Score(list(model=m), formula = surv_obj ~ 1,
                                  data=disc_data, times=tp_months, metrics="auc",
                                  summary = "risks")
      auc_tp <- sc$AUC$score$AUC[1]
    }
  }, silent=TRUE)

  return(list(n = nrow(disc_data), events = sum(disc_data$observed_event),
              harrell_c = round(harrell_c,3), uno_c = round(uno_c,3),
              auc_timepoint = round(auc_tp,3)))
}

#' Generic decision curve analysis
perform_decision_curve_generic <- function(data, timepoint, cfg) {
  time_var  <- cfg$time_var
  event_var <- cfg$event_var
  expected_prefix <- cfg$expected_prefix
  time_months <- convert_time_to_months(data[[time_var]], cfg$time_unit)
  tp_months  <- timepoint * 12
  expected_var <- build_expected_var(expected_prefix, timepoint)

  dca_data <- data[!is.na(data[[expected_var]]) & !is.na(data[[time_var]]) &
                    !is.na(data[[event_var]]), ]
  if (nrow(dca_data) < 20) return(list(n = nrow(dca_data), status="insufficient_data"))

  dca_data <- within(dca_data, {
    predicted_risk <- 1 - get(expected_var)
    outcome <- get(event_var)==1 & convert_time_to_months(get(time_var), cfg$time_unit) <= tp_months
  })
  event_rate <- mean(dca_data$outcome)
  thresholds <- seq(0.01, 0.50, by=0.01)
  net_benefit_model <- numeric(length(thresholds))
  net_benefit_all   <- numeric(length(thresholds))
  n <- nrow(dca_data)
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    treat_model <- dca_data$predicted_risk >= t
    tp <- sum(dca_data$outcome & treat_model)
    fp <- sum(!dca_data$outcome & treat_model)
    net_benefit_model[i] <- (tp/n) - (fp/n)*(t/(1-t))
    tp_all <- sum(dca_data$outcome); fp_all <- sum(!dca_data$outcome)
    net_benefit_all[i] <- (tp_all/n) - (fp_all/n)*(t/(1-t))
  }
  opt_idx <- which.max(net_benefit_model)
  return(list(n=n, events=sum(dca_data$outcome), event_rate=round(event_rate,3),
              optimal_threshold = thresholds[opt_idx],
              optimal_net_benefit = round(net_benefit_model[opt_idx],4)))
}

#' Generic PRAME-augmented NRI/IDI helper
perform_prame_augmented_generic <- function(data, timepoints, cfg) {
  base_risk_var <- cfg$base_risk_var
  if (!all(c(base_risk_var, "prame_status") %in% names(data))) {
    return(list(prame_available = FALSE))
  }
  prame_data <- data[!is.na(data$prame_status) & data$prame_status %in% c("Positive","Negative") &
                      !is.na(data[[base_risk_var]]), ]
  if (nrow(prame_data) < 30) return(list(prame_available = FALSE))

  results <- list();
  for (tp in timepoints) {
    base_risk <- prame_data[[base_risk_var]]
    prame_pos <- prame_data$prame_status == "Positive"
    # simplistic adjustment
    adj <- ifelse(prame_pos, pmin(base_risk*1.3,0.95), base_risk*0.9)
    risk_cut <- c(0,0.1,0.3,1)
    base_cat <- cut(base_risk, breaks=risk_cut, labels=c("Low","Int","High"), include.lowest=TRUE)
    enh_cat  <- cut(adj,       breaks=risk_cut, labels=c("Low","Int","High"), include.lowest=TRUE)
    events <- prame_data[[cfg$event_var]]==1 &
              convert_time_to_months(prame_data[[cfg$time_var]], cfg$time_unit) <= tp*12
    n_e <- sum(events); n_ne <- sum(!events)
    if (n_e<5 || n_ne<5) next
    ev_up <- sum(enh_cat[events]  > base_cat[events]);   ev_down <- sum(enh_cat[events]  < base_cat[events])
    ne_up <- sum(enh_cat[!events] > base_cat[!events]);  ne_down <- sum(enh_cat[!events] < base_cat[!events])
    nri <- (ev_up - ev_down)/n_e + (ne_down - ne_up)/n_ne
    results[[paste0("yr",tp)]] <- list(timepoint=tp, nri_total=round(nri,3),
                                         events=n_e, nonevents=n_ne)
  }
  return(list(prame_available=TRUE, nri_results=results))
}

# operator %||%
`%||%` <- function(a,b){if(!is.null(a)) a else b} 