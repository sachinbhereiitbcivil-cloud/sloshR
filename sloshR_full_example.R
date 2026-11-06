## =====================================================================
## sloshR_full_example.R
## Runs every sloshR function once, start to finish, and saves the
## results (tables as CSV, figures as PNG, one animation as GIF).
##
## Two modes:
##   use_example_data <- TRUE   runs on the synthetic demo storms that ship
##                              with sloshR (works anywhere, ~1 minute)
##   use_example_data <- FALSE  runs on your real converted SLOSH storms
## =====================================================================

library(sloshR)
library(data.table)
library(ggplot2)

## ---- 1. Settings --------------------------------------------------------
use_example_data <- TRUE

## real-data settings (used when use_example_data is FALSE)
sdp_dir      <- "C:/slosh.pkg/sloshdsp"            # SLOSH Display Program folder
run_convert  <- FALSE                              # TRUE = convert rex files first (Windows)
site         <- list(name = "Galveston", lat = 29.310, lon = -94.793)
map_storm    <- "2008Ike_gl2"                      # storm for the map, metrics and animation
tide_csv     <- NULL   # optional CSV with columns time (UTC), level (m)
driver_csv   <- NULL   # optional CSV with columns time (UTC), value (rain or river flow)
obs_csv      <- NULL   # optional CSV with columns storm, obs_peak_m (, obs_peak_time)
targets      <- data.frame(target = c("Texas City", "Kemah"),
                           lat = c(29.385, 29.543), lon = c(-94.895, -95.020))

radius_km    <- 3
thresholds   <- c(0.5, 1, 2, 3)                    # metres
out_dir      <- file.path(getwd(), "sloshR_results")

## demo-data settings
if (use_example_data) {
  txt_dir   <- slosh_example_dir()
  site      <- list(name = "Demo site", lat = 29.31, lon = -94.79)
  map_storm <- "2099Demo_tst"
  targets   <- data.frame(target = c("Bay head", "Open coast"),
                          lat = c(29.36, 29.26), lon = c(-94.86, -94.74))
  out_dir   <- file.path(getwd(), "sloshR_demo_results")
} else {
  txt_dir <- file.path(sdp_dir, "output", "rex_txt")
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
save_csv <- function(x, name) fwrite(x, file.path(out_dir, name))
save_png <- function(p, name, w = 9, h = 5.5) {
  print(p)
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 150)
}

## ---- 2. Convert rex files (Windows only, real data) -----------------------
if (!use_example_data && run_convert) {
  slosh_convert(sdp_dir)                                     # 1  slosh_convert
}
log <- slosh_read_log(txt_dir)                               # 2  slosh_read_log
cat("\nConversion log:", nrow(log), "rex files,",
    sum(log$status != "ok"), "failed\n")

## ---- 3. Read ----------------------------------------------------------------
storms <- slosh_list_storms(txt_dir)                         # 3  slosh_list_storms
print(storms)
save_csv(storms, "storms.csv")

frames <- slosh_read_frames(map_storm, txt_dir)              # 4  slosh_read_frames
track  <- slosh_read_track(map_storm, txt_dir)               # 5  slosh_read_track
cat("\n", map_storm, ":", length(unique(frames$frame)), "frames,",
    length(unique(frames$cell)), "grid cells\n")
save_csv(track, paste0(map_storm, "_track.csv"))

## ---- 4. Process -------------------------------------------------------------
envelope <- slosh_envelope(frames)                           # 6  slosh_envelope
save_csv(envelope, paste0(map_storm, "_envelope.csv"))

cells <- slosh_site_cells(frames, site$lat, site$lon,        # 7  slosh_site_cells
                          radius_km)
cat("\nWet/dry cells within", radius_km, "km of", site$name, ":", nrow(cells), "\n")

series <- slosh_site_series(frames, site$lat, site$lon,      # 8  slosh_site_series
                            radius_km)
save_csv(series, paste0(map_storm, "_site_series.csv"))

events <- slosh_events(txt_dir, site$lat, site$lon,          # 9  slosh_events
                       radius_km = radius_km, site_name = site$name,
                       save_dir = file.path(out_dir, "per_storm"))
print(events[covers_site == TRUE,
             .(storm, peak_surge_m, peak_time_utc, duplicate_event)])
save_csv(events, "event_table.csv")

## site series for every storm that reaches the site (one each)
hits <- events[covers_site == TRUE & duplicate_event == FALSE, storm]
all_series <- rbindlist(lapply(hits, function(s)
  slosh_site_series(slosh_read_frames(s, txt_dir), site$lat, site$lon, radius_km)))

## ---- 5. Metrics --------------------------------------------------------------
metrics <- slosh_storm_metrics(all_series, thresholds)       # 10 slosh_storm_metrics
print(metrics)
save_csv(metrics, "storm_metrics.csv")

footprint <- slosh_footprint(envelope, thresholds)           # 11 slosh_footprint
print(footprint)
save_csv(footprint, paste0(map_storm, "_footprint.csv"))

## ---- 6. Compound timing ------------------------------------------------------
## tide: your CSV if given; otherwise an illustrative 0.4 m semidiurnal tide
if (!is.null(tide_csv)) {
  tide <- fread(tide_csv)
} else {
  message("No tide file given: using an ILLUSTRATIVE tide, not real predictions.")
  t0 <- min(all_series$time) - 86400
  t1 <- max(all_series$time) + 86400
  tide <- data.frame(time = seq(t0, t1, by = 600))
  tide$level <- 0.4 * cos(2 * pi * as.numeric(tide$time - t0) / (12.42 * 3600))
}
tide_timing <- slosh_tide_timing(all_series, tide)          # 12 slosh_tide_timing
print(tide_timing)
save_csv(tide_timing, "tide_timing.csv")

## driver (rainfall or river flow): your CSV if given; otherwise illustrative
if (!is.null(driver_csv)) {
  driver <- fread(driver_csv)
} else {
  message("No driver file given: using an ILLUSTRATIVE rainfall peak 10 h after each surge peak.")
  driver <- rbindlist(lapply(split(all_series, all_series$storm), function(s) {
    pk <- s$time[which.max(s$surge_m)]
    data.table(storm = s$storm[1], time = pk + seq(-48, 48) * 3600,
               value = 40 * exp(-((seq(-48, 48) - 10) / 6)^2))
  }))
}
driver_lag <- slosh_driver_lag(all_series, driver)          # 13 slosh_driver_lag
print(driver_lag)
save_csv(driver_lag, "driver_lag.csv")

## ---- 7. Validation and transfer -----------------------------------------------
if (!is.null(obs_csv)) {
  obs <- fread(obs_csv)
} else {
  message("No observations given: using ILLUSTRATIVE observed peaks (model +/- 0.2 m).")
  obs <- events[covers_site == TRUE & duplicate_event == FALSE,
                .(storm, obs_peak_m = peak_surge_m + rep_len(c(0.2, -0.1, 0.15), .N))]
}
validation <- slosh_validate(events, obs)                    # 14 slosh_validate
print(validation)
save_csv(validation$table, "validation_by_storm.csv")
save_csv(validation$summary, "validation_summary.csv")

transfer <- slosh_transfer(txt_dir, gauge = list(lat = site$lat, lon = site$lon),
                           targets = targets, radius_km = radius_km)  # 15 slosh_transfer
print(transfer)
save_csv(transfer$table, "transfer_by_storm.csv")
save_csv(transfer$summary, "transfer_summary.csv")

## ---- 8. Figures ---------------------------------------------------------------
save_png(slosh_plot_peaks(events, site$name), "1_peaks_by_year.png")       # 16
save_png(slosh_plot_series(all_series, site$name), "2_site_series.png",    # 17
         h = 2 + 1.8 * ceiling(length(hits) / 2))
save_png(slosh_plot_map(envelope, track, site), "3_max_surge_map.png",     # 18
         w = 8, h = 7)
save_png(slosh_plot_grids(events), "4_storms_per_grid.png",                # 19
         h = max(3, 0.25 * uniqueN(events$basin) + 1.5))

## ---- 9. Animation -------------------------------------------------------------
slosh_animate(map_storm, txt_dir,                                          # 20
              file = file.path(out_dir, paste0(map_storm, ".gif")),
              site = site, radius_km = radius_km,
              every_nth = if (use_example_data) 1 else 2)

## slosh_example_dir() (function 21) supplied the demo data above.
cat("\nAll results saved in:", out_dir, "\n")
print(list.files(out_dir, recursive = TRUE))
