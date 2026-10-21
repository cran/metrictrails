## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4)

## ----setup--------------------------------------------------------------------
library(metrictrails)

activity_file = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
route_file = system.file("extdata", "gpx_route.gpx", package = "metrictrails")
dem_file = system.file("extdata", "test_dem.tif", package = "metrictrails")

## -----------------------------------------------------------------------------
raw = read_stream(activity_file)
raw

## -----------------------------------------------------------------------------
raw = uniform_stream(raw)
names(raw)
head(uniform_stream(strava_streams), 3)

## -----------------------------------------------------------------------------
clean = sanitize_stream(raw)
attr(clean, "sanitize_report")$counts

## -----------------------------------------------------------------------------
methods = c("kalman", "loess", "gaussian", "savgol", "pipeline")
lengths = sapply(methods, function(m) {
  summarise_stream(sanitize_stream(raw, method = m))$distance_2d
})
round(c(raw = summarise_stream(raw)$distance_2d, lengths))

## -----------------------------------------------------------------------------
resolution_stream(clean)

## -----------------------------------------------------------------------------
resampled = resample_stream(clean)
attr(resampled, "res")
nrow(resampled)

## ----fractal------------------------------------------------------------------
res = c(1, 2, 3, 5, 8, 13, 20, 30, 50, 80)
distance = sapply(res, function(r) summarise_stream(resample_stream(clean, res = r))$distance_2d)
plot(res, distance / 1000, log = "x", type = "b", pch = 19,
     xlab = "Resolution (m, log scale)", ylab = "Distance (km)")
abline(v = attr(resampled, "res"), lty = 2)

## -----------------------------------------------------------------------------
streams = list(activity = clean, route = read_stream(route_file))
resolution_stream(streams)[, c("stream", "q50", "res")]
common = max(resolution_stream(streams)$res)

## -----------------------------------------------------------------------------
fd = fractal_stream(clean)
fd$dimension

## -----------------------------------------------------------------------------
final = ele_correction(resampled, dem = dem_file)

## ----eval = FALSE-------------------------------------------------------------
# # same result with a downloaded DEM, refined to 1 m by bilinear interpolation
# dem = get_dem(resampled, res = 1)
# final = ele_correction(resampled, dem = dem)

## ----elevation----------------------------------------------------------------
d_raw = differential_stream(raw)
d_final = differential_stream(final)
plot(d_raw$distance / 1000, raw$ele, type = "l", col = "grey60",
     xlab = "Distance (km)", ylab = "Elevation (m)")
lines(d_final$distance / 1000, final$ele, lwd = 1.5)
legend("topright", c("device", "pipeline + DEM"), col = c("grey60", "black"), lwd = c(1, 1.5), bty = "n")

## -----------------------------------------------------------------------------
cols = c("distance_2d", "elevation_gain", "elevation_loss", "km_effort", "resolution")
cbind(stream = c("raw", "pipeline"),
      rbind(summarise_stream(raw)[, cols], summarise_stream(final)[, cols]))

## ----tobler-------------------------------------------------------------------
params = tobler_stream(final)
params

d = differential_stream(final)
keep = is.finite(d$speed) & is.finite(d$slope) & abs(d$slope) <= 0.6
plot(d$slope[keep], d$speed[keep], pch = 16, cex = 0.4, col = "grey60",
     xlab = "Slope (rise over run)", ylab = "Horizontal speed (m/s)")
slope = seq(-0.6, 0.6, by = 0.01)
lines(slope, tobler_speed(slope, params), lwd = 2)

## -----------------------------------------------------------------------------
route = read_stream(route_file)
detect_stream(raw, route)

## -----------------------------------------------------------------------------
located = locate_stream(clean, route)
summary(abs(located$offset_cross))

compare_stream(resample_stream(clean, res = 2), resample_stream(route, res = 2))

## -----------------------------------------------------------------------------
reference = route %>% resample_stream(res = 2) %>% ele_correction(dem = dem_file)
matched = match_stream(clean[1:800, ], reference, params = params)
attr(matched, "match_report")
summary(matched[, c("offset_d", "offset_along", "offset_cross")])

## ----matched------------------------------------------------------------------
plot(matched$time / 60, matched$s_ref / 1000, type = "l",
     xlab = "Time (min)", ylab = "Position along the reference (km)")

## -----------------------------------------------------------------------------
out = tempfile(fileext = ".gpx")
write_stream(final, out, name = "Manquehue loop")
nrow(read_stream(out))

