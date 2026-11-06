# sloshR
R package to read, analyse and visualise NOAA SLOSH storm surge model runs: batch-convert rex files, build per-storm event tables, surge metrics, flood footprints, tide and rainfall timing, validation, maps and animations.

First release of sloshR (22 December 2024).

sloshR reads, analyses and visualises NOAA SLOSH storm surge model runs (rex files) in R.

Highlights
- slosh_convert(): batch-converts rex files to text using the SLOSH Display Program's own library (Windows)
- slosh_events(): one-row-per-storm event table at any site, with duplicate runs flagged
- slosh_storm_metrics(), slosh_footprint(): peak, hours above thresholds, rate of rise, flooded area
- slosh_tide_timing(), slosh_driver_lag(): surge timing against high tide, rainfall or river flow
- slosh_validate(), slosh_transfer(): skill against observations; site-to-gauge surge ratios
- slosh_plot_*(), slosh_animate(): maps, time series, peak charts and GIF animations
- Synthetic example data, a getting-started vignette and full help pages

Install
  remotes::install_github("sachinbhere/sloshR", build_vignettes = TRUE)

Converting rex files needs Windows and NOAA's SLOSH Display Program; all other functions run on any platform. SLOSH output is for planning, not engineering design.
