# Get hardware specs for a mac
get_mac_hardware_info <- function() {
  system("system_profiler SPHardwareDataType", intern = TRUE)
}

#' Wrapper for canaper::cpr_rand_test()
#'
#' Additional arguments:
#' @param workers Number of CPUs to use in parallel
#' @param hardware_info Character vector; output of get_mac_hardware_info(),
#' includes hardware specs when running on a Mac
#' @param req_spec Character vector; strings to detect in `hardware_info`;
#' if any are not detected, the function will fail
#' @param seed The seed used for set.seed()
run_canape <- function(
  comm, phy, null_model, n_reps = 500, n_iterations = 100000, workers = 6,
  hardware_info = NULL, req_spec = NULL, seed) {
  on.exit(plan(sequential))
  set.seed(seed)
  # Check hardware requirements
  if (!is.null(hardware_info)) {
    hardware_check <- purrr::map_lgl(
      req_spec,
      ~any(stringr::str_detect(hardware_info, fixed(.)))
    )
    assertthat::assert_that(
      all(hardware_check),
      msg = "Required hardware not detected"
    )
  }
  # Set up parallelization
  plan(multisession, workers = workers)
  # Run randomization test, returning output as tibble (not a dataframe)
  rand_res <- cpr_rand_test(
    comm = comm, phy = phy,
    null_model = null_model,
    n_reps = n_reps, n_iterations = n_iterations,
    tbl_out = TRUE
  )
  # Unset parallelization
  plan(sequential)
  # Classify endemism
  res <- cpr_classify_endem(rand_res) %>%
    mutate(site = str_remove_all(site, "'"))
  return(res)
}

#' Classify endemism types for output from Biodiverse
#'
#' Description of Biodiverse output columns and corresponding canaper columns
#' as follows
#'
#' PE_WE_P: Phylogenetic weighted endemism as a proportion of the total tree
#' length
#' = pe_obs_p_upper
#'
#' PHYLO_RPE2: Relative Phylogenetic Endemism (RPE). The ratio of the tree's PE
#' to a null model where PE is calculated using a tree where all branches are of
#' equal length.
#' = rpe_obs_p_upper
#'
#' PHYLO_RPE_NULL2: Null score used as the denominator in the RPE2 calculations
#' = pe_alt_obs_p_upper
#' @param biod_res_raw Output from Biodiverse (SPATIAL RESULTS, including
#' only observed values)
#' @param biod_res_prank Output from Biodiverse (p_rank >> SPATIAL RESULTS,
#' including p-rank of observed values in comparison with randomizations)
classify_biod <- function(biod_res_raw, biod_res_prank) {
  biod_res_prank %>%
  # Replace missing values with 0.5 as per SL code
  # https://gist.github.com/shawnlaffan/2935dfd3efa92f23a440aa3db5916645
  mutate(
    PE_WE_P = case_when(
      is.na(PE_WE_P) & (!is.na(PHYLO_RPE_NULL2) | !is.na(PHYLO_RPE2)) ~ 0.5,
      TRUE ~ PE_WE_P
    ),
    PHYLO_RPE_NULL2 = case_when(
      is.na(PHYLO_RPE_NULL2) & (!is.na(PE_WE_P) | !is.na(PHYLO_RPE2)) ~ 0.5,
      TRUE ~ PHYLO_RPE_NULL2
    ),
    PHYLO_RPE2 = case_when(
      is.na(PHYLO_RPE2) & (!is.na(PHYLO_RPE_NULL2) | !is.na(PE_WE_P)) ~ 0.5,
      TRUE ~ PHYLO_RPE2
    )
  ) %>%
  # Select metrics corresponding to canaper classification
  transmute(
    site = ELEMENT,
    rpe_obs_p_upper = PHYLO_RPE2,
    rpe_obs_p_lower = 1 - PHYLO_RPE2,
    pe_obs_p_upper = PE_WE_P,
    pe_alt_obs_p_upper = PHYLO_RPE_NULL2
    ) %>%
  # Classify according to canaper code
  cpr_classify_endem() %>%
  # Still have NAs left over. Fill these based on original PE values
  left_join(
    select(biod_res_raw, site = ELEMENT, P_PE_WE_P),
    by = "site") %>%
  # Classify cells as non-significant if Biodiverse p-ranks are NA and
  # original PE is not NA
  mutate(
    endem_type = case_when(
      is.na(endem_type) & !is.na(P_PE_WE_P) ~ "not significant",
      TRUE ~ endem_type
    )
  )
}

#' Calculate percentage agreement in endemism types between two dataframes
calc_agree_endem <- function(df_1, df_2) {
  left_join(
    df_1 |> select(site, endem_type_1 = endem_type),
    df_2 |> select(site, endem_type_2 = endem_type),
    by = "site"
  ) |>
    mutate(agree = endem_type_1 == endem_type_2) |>
    summarize(agree = sum(agree), total = n()) |>
    mutate(
      p_agree = agree / total
    )
}

#' Calculate elapsed time for Biodiverse to run randomizations
#'
#' Biodiverse records a log at ~/Library/Logs/BiodiverseGUI/BiodiverseGUI.log
#' (on a mac). However, it does not save the time each action is taken.
#' To record time elapsed, first delete the log (it will be recreated with
#' each instance of Biodiverse). Then, set up the analysis, but do not hit
#' the start button. In the terminal, record the start time with `date`, then
#' immediately hit the start button. When the analysis finishes, check the last
#' modification of the log with `stat -x BiodiverseGUI.log`. The difference
#' between the two times is the elapsed time to conduct the analysis.
#'
#' @param text Character vector; text read in with readLines() or read_lines()
#' copied from terminal when running Biodiverse.
extract_biod_elapsed_min <- function(text) {
  # Get start time
  start <-
    text[2] %>%
    lubridate::parse_date_time(orders = "a b d H M S Y")
  # Get end time
  end <- text[str_detect(text, "Change:")] %>%
    str_remove_all("Change: ") %>%
    lubridate::parse_date_time(orders = "a b d H M S Y")
  # Calculate difference in minutes
  difftime(end, start, units = "min") %>% 
    as.numeric() %>%
    round(1)
}