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
run_canape <- function(
  comm, phy, null_model, n_reps = 500, n_iterations = 100000, workers = 6,
  hardware_info = NULL, req_spec = NULL) {
  on.exit(plan(sequential))
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

#' Test number of optimal iterations needed for a given null model
test_iter <- function(comm, null_model, n_iter, binary) {
  if (isTRUE(binary)) {
    # binary algorithms produce binary matrices, so convert input
    # matrix to binary too
    comm <- as.matrix(comm)
    comm[comm > 0] <- 1
  }
  # Use a tibble to hold the data and run loops
  tibble::tibble(
    n_iter = n_iter,
    # Make one random community for each value of `n_iter`
    rand_comm = purrr::map(
      n_iter,
      ~ canaper::cpr_rand_comm(
        comm,
        null_model = null_model, n_iterations = .
      )
    ),
    # Calculate the Pearson correlation (r) between each randomized
    # matrix and the original matrix by converting
    # the matrices to vectors
    corr = map_dbl(
      rand_comm,
      ~ cor(c(comm), c(.)) # c() converts a matrix to vector
    )
  )
}

#' Classify endemism types for output from Biodiverse
#'
#' Description of Biodiverse output columns and corresponding canaper columns
#' as follows
#'
#' PE_WE_P: Phylogenetic weighted endemism as a proportion of the total tree
#' length
#'
#' PHYLO_RPE2: Relative Phylogenetic Endemism (RPE). The ratio of the tree's PE
#' to a null model where PE is calculated using a tree where all branches are of
#' equal length.
#' = rpe_obs_p_upper
#'
#' PHYLO_RPE_DIFF2: How much more or less PE is there than expected, in original
#' tree units.
#' = pe_obs_p_upper
#'
# PHYLO_RPE_NULL2: Null score used as the denominator in the RPE2 calculations
# = pe_alt_obs_p_upper
classify_biod <- function(biod_res) {
  biod_res %>%
  transmute(
    site = ELEMENT,
    rpe_obs_p_upper = PHYLO_RPE2,
    rpe_obs_p_lower = 1 - PHYLO_RPE2,
    pe_obs_p_upper = PHYLO_RPE_DIFF2,
    pe_alt_obs_p_upper = PHYLO_RPE_NULL2
    ) %>%
  cpr_classify_endem() %>%
  mutate(endem_type = replace_na(endem_type, "not significant"))
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
