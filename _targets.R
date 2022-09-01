library(targets)
library(tarchetypes)

source("R/packages.R")
source("R/functions.R")

tar_plan(
  # Load acacia dataset
  acacia = canaper::acacia,
  # Test optimal number of iterations for making random communities
  acacia_iter_test_res = test_iter(
    comm = acacia$comm,
    null_model = "curveball",
    # test iterations from 10 to 10^7
    n_iter = magrittr::raise_to_power(10, 1:7),
    binary = TRUE
  ),
  # Run CANAPE using canaper (2 cores)
  acacia_canape_cpr = run_canape(
    comm = acacia$comm,
    phy = acacia$phy,
    null_model = "curveball",
    n_reps = 999,
    n_iterations = 100000,
    workers = 2
  ),
  # Repeat, with only one core for comparison
  acacia_canape_cpr_3 = run_canape(
    comm = acacia$comm,
    phy = acacia$phy,
    null_model = "curveball",
    n_reps = 999,
    n_iterations = 100000,
    workers = 3
  ),
  acacia_canape_cpr_single = run_canape(
    comm = acacia$comm,
    phy = acacia$phy,
    null_model = "curveball",
    n_reps = 999,
    n_iterations = 100000,
    workers = 1
  ),
  # Load results from Biodiverse for Acacia
  tar_file_read(
    acacia_biod_res,
    "_targets/user/data_raw/CANAPE_Acacia/spatial_results_prank.csv",
    read_csv(!!.x)),
  # Classify endemism types for Biodiverse results
  acacia_canape_biod = classify_biod(acacia_biod_res),
  # Compare % agreement between canaper and Biodiverse
  acacia_canape_comp = calc_agree_endem(
    df_1 = acacia_canape_cpr,
    df_2 = acacia_canape_biod
  ),
  # Render MS
  tar_quarto(
    ms_doc,
    "ms/ms.Qmd",
    extra_files = c(
      "ms/methods-in-ecology-and-evolution.csl",
      "ms/references.yaml"
    )
  )
)