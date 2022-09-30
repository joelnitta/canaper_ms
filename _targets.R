library(targets)
library(tarchetypes)

source("R/packages.R")
source("R/functions.R")

tar_plan(
  # Load acacia dataset
  acacia = canaper::acacia,
  # Run CANAPE using canaper on mac laptop
  # test up to three cores
  mac_laptop_specs = c(
    "Model Name: MacBook Pro",
    "Processor Name: Quad-Core Intel Core i7",
    "Total Number of Cores: 4",
    "Memory: 16 GB"),
  acacia_rep = 999,
  acacia_iter = 50000,
  acacia_canape_cpr_1 = run_canape(
    comm = acacia$comm,
    phy = acacia$phy,
    null_model = "curveball",
    n_reps = acacia_rep,
    n_iterations = acacia_iter,
    workers = 1,
    hardware_info = get_mac_hardware_info(),
    req_spec = mac_laptop_specs,
    seed = 12345
  ),
  acacia_canape_cpr_2 = run_canape(
    comm = acacia$comm,
    phy = acacia$phy,
    null_model = "curveball",
    n_reps = acacia_rep,
    n_iterations = acacia_iter,
    workers = 2,
    hardware_info = get_mac_hardware_info(),
    req_spec = mac_laptop_specs,
    seed = 12345
  ),
  acacia_canape_cpr_3 = run_canape(
    comm = acacia$comm,
    phy = acacia$phy,
    null_model = "curveball",
    n_reps = acacia_rep,
    n_iterations = acacia_iter,
    workers = 3,
    hardware_info = get_mac_hardware_info(),
    req_spec = mac_laptop_specs,
    seed = 12345
  ),
  # Load results from Biodiverse for Acacia
  # - p-rank results
  tar_file_read(
    acacia_biod_res_prank,
    "_targets/user/data_raw/acacia_canape_prank_spatial_results.csv",
    read_csv(!!.x)
  ),
  # - raw (observed) values
  tar_file_read(
    acacia_biod_res_raw,
    "_targets/user/data_raw/acacia_canape_spatial_results.csv",
    read_csv(!!.x)
  ),
  # Classify endemism types for Biodiverse results
  acacia_canape_biod = classify_biod(
    biod_res_raw = acacia_biod_res_raw,
    biod_res_prank = acacia_biod_res_prank),
  # Compare % agreement between canaper and Biodiverse
  acacia_canape_comp = calc_agree_endem(
    df_1 = acacia_canape_cpr_1,
    df_2 = acacia_canape_biod
  ),
  # Calculate time to run Biodiverse
  tar_file_read(
    acacia_biod_times_raw,
    "_targets/user/data_raw/acacia_canape_biod_times.txt",
    read_lines(!!.x)
  ),
  acacia_biod_elapsed_min = extract_biod_elapsed_min(acacia_biod_times_raw),
  # Render MS
  tar_quarto(
    ms,
    "ms/ms.Qmd",
    extra_files = c(
      "ms/methods-in-ecology-and-evolution.csl",
      "ms/references.yaml"
    )
  ),
  # Render SI
  tar_quarto(
    si,
    "ms/si.Qmd",
    extra_files = c(
      "ms/methods-in-ecology-and-evolution.csl",
      "ms/references.yaml"
    )
  )
)