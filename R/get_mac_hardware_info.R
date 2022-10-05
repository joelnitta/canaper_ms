# Get hardware info on a mac
# Run this outside of the docker image before running _targets.R

saveRDS(
  system("system_profiler SPHardwareDataType", intern = TRUE),
  "_targets/user/data_raw/my_hardware_info.RDS")