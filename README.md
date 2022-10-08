# canaper_ms

Repository including code and data for Nitta et al. "canaper: Categorical analysis of neo- and paleo-endemism in R". *bioRxiv* <https://doi.org/10.1101/2022.10.06.511072>

## Dependencies

- R (only if running [pre-work](#pre-work))
- R packages as listed in [renv.lock](renv.lock) OR docker

## Pre-work

Before running the workflow, the [get_mac_hardware_info.R](R/get_mac_hardware_info.R) script needs to be run.
This script records the hardware configuration of the computer, and only works on a Mac.
The workflow assumes a particular hardware configuration, and will error if this is not met.

To skip this check edit the following lines in [_targets.R](_targets.R) as follows:

```r
hardware_info = readRDS("_targets/user/data_raw/my_hardware_info.RDS"),
req_spec = mac_laptop_specs,
```

to

```r
hardware_info = NULL,
req_spec = NULL,
```

## Docker image

A Docker image is provided to run the code, [joelnitta/canaper_ms](https://hub.docker.com/r/joelnitta/canaper_ms).

The workflow can be run (after completing [Pre-work](#pre-work)) using the Docker image as follows:

```
docker run --rm -v ${PWD}:/wd -w /wd joelnitta/canaper_ms:1.0.0 Rscript -e 'targets::tar_make()'
```

## Output

After completing, the workflow will output files `ms.pdf`, `ms.docx`, `si.pdf`, `figure_2.pdf`, and `figure_3.pdf` to the `ms/` folder.

## Licenses

- Code: [MIT](LICENSE)
- Preprint: [CC-BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/)
- Data (files in `_targets/user/data_raw`): [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/)