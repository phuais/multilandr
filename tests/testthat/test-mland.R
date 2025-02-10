# Test: mland() arguments

test_that("loads MultiLand successfully", {
  expect_s4_class({
    elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
    elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
    mm <- mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "name",
          rast_names = "landcover",
          segs = 20,
          progress = F)
  }, "MultiLand")

  expect_error({
    elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
    elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
    mland(points_layer = elchaco_sites2,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "name",
          rast_names = "landcover",
          segs = 20,
          progress = F)
  })

  expect_error({
    elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
    elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
    mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = c("a", "b", "c"),
          site_ref = "name",
          rast_names = "landcover",
          segs = 20,
          progress = F)
  }, "- argument 'radii' must be a vector of positive numbers.")

  expect_warning({
    elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
    elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
    mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "site",
          rast_names = "landcover",
          segs = 20,
          progress = F)
  })

  expect_warning({
    elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
    elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
    mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "name",
          class_names = c(1, "Forest",
                          2, "Grassland",
                          3, "Crops",
                          4, "Pastures",
                          5, "Water",
                          6, "Urban"),
          rast_names = "landcover",
          segs = 20,
          progress = F)
  })

  expect_warning({
    elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
    elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
    mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "name",
          class_names = list(c(1, "Forest",
                               2, "Grassland",
                               3, "Crops")),
          rast_names = "landcover",
          segs = 20,
          progress = F)
  })

})

# Test: mland_overlap()

ernesdesign <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
ernesdesign <- suppressMessages(mland_load(ernesdesign))

test_that("perform overlapping calculations", {
expect_error({
  mland_overlap(ernesdesign, points = 17:20)
  })
expect_warning({
  mland_overlap(ernesdesign, points = 1:2, title = "site")
})
})

# Test: generate_points

test_that("generation of points is going fine", {
  elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
  expect_s4_class({
    generate_points(elchaco, approach = "random", values = 1, n = 500, progress = F)},
    "SpatVector")
  expect_equal({
    pp <- generate_points(elchaco, approach = "patch",
                          patch_conditions = conditions(list(1, "area", 8, 8.1)), progress = F)
    length(pp)
  }, 35)
})
