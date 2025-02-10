# Test: mland() arguments

test_that("loads MultiLand successfully", {
  expect_s4_class({
    temp_file1 <- tempfile(fileext = ".tif")
    download.file(multilandr_data[1], destfile = temp_file1)
    elchaco <- terra::rast(temp_file1)
    temp_file2 <- tempfile(fileext = ".gpkg")
    download.file(multilandr_data[4], destfile = temp_file2)
    elchaco_sites <- terra::vect(temp_file2)
    mm <- mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "name",
          rast_names = "landcover",
          segs = 20,
          progress = F)
    unlink(temp_file1)
    unlink(temp_file2)
    mm
  }, "MultiLand")

  expect_error({
    temp_file1 <- tempfile(fileext = ".tif")
    download.file(multilandr_data[1], destfile = temp_file1)
    elchaco <- terra::rast(temp_file1)
    temp_file2 <- tempfile(fileext = ".gpkg")
    download.file(multilandr_data[4], destfile = temp_file2)
    elchaco_sites <- terra::vect(temp_file2)
    mland(points_layer = elchaco_sites2,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "name",
          rast_names = "landcover",
          segs = 20,
          progress = F)
    unlink(temp_file1)
    unlink(temp_file2)
  })

  expect_error({
    temp_file1 <- tempfile(fileext = ".tif")
    download.file(multilandr_data[1], destfile = temp_file1)
    elchaco <- terra::rast(temp_file1)
    temp_file2 <- tempfile(fileext = ".gpkg")
    download.file(multilandr_data[4], destfile = temp_file2)
    elchaco_sites <- terra::vect(temp_file2)
    mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = c("a", "b", "c"),
          site_ref = "name",
          rast_names = "landcover",
          segs = 20,
          progress = F)
    unlink(temp_file1)
    unlink(temp_file2)
  }, "- argument 'radii' must be a vector of positive numbers.")

  expect_warning({
    temp_file1 <- tempfile(fileext = ".tif")
    download.file(multilandr_data[1], destfile = temp_file1)
    elchaco <- terra::rast(temp_file1)
    temp_file2 <- tempfile(fileext = ".gpkg")
    download.file(multilandr_data[4], destfile = temp_file2)
    elchaco_sites <- terra::vect(temp_file2)
    mland(points_layer = elchaco_sites,
          rast_layer = elchaco,
          radii = seq(1000, 5000, 1000),
          site_ref = "site",
          rast_names = "landcover",
          segs = 20,
          progress = F)
    unlink(temp_file1)
    unlink(temp_file2)
  })

  expect_warning({
    temp_file1 <- tempfile(fileext = ".tif")
    download.file(multilandr_data[1], destfile = temp_file1)
    elchaco <- terra::rast(temp_file1)
    temp_file2 <- tempfile(fileext = ".gpkg")
    download.file(multilandr_data[4], destfile = temp_file2)
    elchaco_sites <- terra::vect(temp_file2)
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
    unlink(temp_file1)
    unlink(temp_file2)
  })

  expect_warning({
    temp_file1 <- tempfile(fileext = ".tif")
    download.file(multilandr_data[1], destfile = temp_file1)
    elchaco <- terra::rast(temp_file1)
    temp_file2 <- tempfile(fileext = ".gpkg")
    download.file(multilandr_data[4], destfile = temp_file2)
    elchaco_sites <- terra::vect(temp_file2)
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
    unlink(temp_file1)
    unlink(temp_file2)
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
  temp_file1 <- tempfile(fileext = ".tif")
  download.file(multilandr_data[1], destfile = temp_file1)
  elchaco <- terra::rast(temp_file1)
  expect_s4_class({
    generate_points(elchaco, approach = "random", values = 1, n = 500, progress = F)},
    "SpatVector")
  expect_equal({
    pp <- generate_points(elchaco, approach = "patch",
                          patch_conditions = conditions(list(1, "area", 8, 8.1)), progress = F)
    length(pp)
  }, 35)
  unlink(temp_file1)
})

