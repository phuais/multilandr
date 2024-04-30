# Test: mland_metrics()

ernesdesign <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
ernesdesign <- suppressMessages(mland_load(ernesdesign))
ed_metrics_test <- mland_metrics(ernesdesign, level = "class", metric = c("pland", "np"),
                                 absence_values = list("pland" = 0, "np" = 0),
                                 points = 1, radii = 1000, progress = F)
mean_sd <- function(x){ mean(x)/sd(x) }

test_that("colnames of metric dataframe have not changed", {
  expect_equal({
    colnames(ed_metrics_test@data)
  }, c("rasterlayer", "layer_name", "point_id", "site", "radius", "level",
       "class", "classname", "patch_id", "metric", "value"))
})

test_that("metrics calculation are ok", {
  expect_equal({
    ed_metrics_test@data$value
  }, c(3, 2, 1, 1, 0, 0, 26.55902, 1.0857461, 71.4643653, 0.8908686, 0, 0))
  expect_equal({
    ed_metrics2 <- mland_metrics(ernesdesign, level = "class", metric = c("pland", "np"),
                                 absence_values = list("pland" = 0, "np" = 0),
                                 ext_calc = list(c(1, "mean")),
                                 points = 1, radii = 1000, progress = F)
    ed_metrics2@data$value
  }, c(3, 2, 1, 1, 0, 0, 26.55902, 1.0857461, 71.4643653, 0.8908686, 0,
       0, 0.4910521))
})

# Test: metrics_filter()

test_that("metric filtering is ok", {
  expect_equal({
    otf_subset <- metrics_filter(otf_metrics,
                                 conditions = conditions(list(NA, "Forest", 2000, "pland", 20, 30)),
                                 output = "MLM")
    otf_subset@n_points
  }, 145)

  expect_equal({
    otf_subset2 <- metrics_filter(otf_metrics,
                                  conditions = conditions(list(NA, "Forest", 2000, "pland", 20, 30),
                                                          list(NA, "Crops", 2000, "pland", -Inf, 60)),
                                  output = "MLM")
    otf_subset2@n_points
  }, 67)
})

# Test: metrics_corr and metrics_plots
test_that("metrics correlations and plots going silently", {
expect_silent({
  metrics_corr(ed_metrics, radii = 5000, classnames = TRUE)})
expect_silent({
  metrics_plots(ed_metrics, classes = 1:4, radii = 3000, classnames = TRUE, c_level = "pland")})
})

# Test: metrics_bind
test_that("metric binding is going fine", {
sites <- ed_metrics@points$name
sampling_data <- data.frame(site = rep(sites, each = 10), richness = sample(1:500, 150))
expect_silent({
  metrics_bind(ed_metrics, sampling_data)})
expect_equal({
  new_data <- metrics_bind(ed_metrics, sampling_data)
  dim(new_data)}
, c(150, 73))
})

