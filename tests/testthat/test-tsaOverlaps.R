# Tests for resolving stands listed in more than one TSA (module parameter resolveTsaOverlaps).
# Run from the module folder: testthat::test_file("tests/testthat/test-tsaOverlaps.R")

moduleFile <- if (file.exists("spades_ws3_dataInit.R")) "spades_ws3_dataInit.R" else
  testthat::test_path("..", "..", "spades_ws3_dataInit.R")
for (e in parse(moduleFile))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("tsaOverlapDrops", "dropInventoryCells", "resolveTsaOverlaps"))
    eval(e)

# A 3-band inventory (hash, age, block) like the datalad inventory_init.tif, cells 1 m wide
.inv <- function(xmin, vals) {
  r <- terra::rast(nrows = 1, ncols = length(vals), xmin = xmin, xmax = xmin + length(vals), ymin = 0, ymax = 1,
                   nlyrs = 3, crs = "EPSG:3005")
  terra::values(r) <- cbind(vals, vals * 10, vals * 100)
  r
}
.box <- function(xmin, xmax, name) {
  v <- terra::vect(terra::ext(xmin, xmax, -1, 2), crs = "EPSG:3005"); v$charTSA <- name; v
}

test_that("each cell in two inventories goes to the TSA whose boundary holds its centre", {
  a <- .inv(0, c(1, 2, 3, 4))      # x 0-4
  b <- .inv(2, c(5, 6, 7, 8))      # x 2-6: cells at x 2.5 and 3.5 are in both
  bnd <- rbind(.box(0, 3, "tsa41"), .box(3, 6, "tsa40"))   # x 2.5 -> tsa41, x 3.5 -> tsa40
  d <- tsaOverlapDrops(list(tsa41 = a, tsa40 = b), bnd)
  expect_equal(d$tsa41, 4L)        # tsa41's cell at x 3.5
  expect_equal(d$tsa40, 1L)        # tsa40's cell at x 2.5
})

test_that("a cell inside neither (or both) boundaries stays with the first-listed TSA", {
  a <- .inv(0, c(1, 2, 3)); b <- .inv(2, c(5, 6, 7))       # x 2.5 in both
  bnd <- rbind(.box(10, 11, "tsa41"), .box(20, 21, "tsa40"))
  d <- tsaOverlapDrops(list(tsa41 = a, tsa40 = b), bnd)
  expect_length(d$tsa41, 0)
  expect_equal(d$tsa40, 1L)
})

test_that("NA cells and non-overlapping TSAs give no drops", {
  a <- .inv(0, c(1, 2, NA)); b <- .inv(2, c(5, 6, 7))      # x 2.5: NA in tsa41
  bnd <- rbind(.box(0, 3, "tsa41"), .box(3, 6, "tsa40"))
  expect_true(all(lengths(tsaOverlapDrops(list(tsa41 = a, tsa40 = b), bnd)) == 0))
  far <- .inv(100, c(1, 2))
  expect_true(all(lengths(tsaOverlapDrops(list(tsa41 = a, tsa08 = far), bnd)) == 0))
})

test_that("dropInventoryCells writes a real file in place of a link, and never touches the link's target", {
  d <- withr::local_tempdir()
  src <- file.path(d, "annex.tif"); terra::writeRaster(.inv(0, c(1, 2, 3)), src, datatype = "INT4S")
  lnk <- file.path(d, "inventory_init.tif"); file.symlink(src, lnk)
  dropInventoryCells(lnk, 2L)
  expect_false(nzchar(Sys.readlink(lnk)))                  # no longer a link
  out <- terra::values(terra::rast(lnk))
  expect_true(all(is.na(out[2, ])))                        # all bands
  expect_equal(out[c(1, 3), 2], c(10, 30))
  expect_equal(unname(terra::values(terra::rast(src))[2, 1]), 2)   # the annexed original is unchanged
  expect_equal(terra::datatype(terra::rast(lnk))[1], "INT4S")
})

test_that("resolveTsaOverlaps edits every inventory file of the TSA that loses a cell, and is idempotent", {
  d <- withr::local_tempdir()
  for (bn in c("tsa41", "tsa40")) dir.create(file.path(d, bn))
  terra::writeRaster(.inv(0, c(1, 2, 3)), file.path(d, "tsa41", "inventory_init.tif"), datatype = "INT4S")
  terra::writeRaster(.inv(0, c(1, 2, 3)), file.path(d, "tsa41", "inventory_2020.tif"), datatype = "INT4S")
  terra::writeRaster(.inv(2, c(5, 6, 7)), file.path(d, "tsa40", "inventory_init.tif"), datatype = "INT4S")
  bnd <- rbind(.box(0, 2, "tsa41"), .box(2, 6, "tsa40"))  # x 2.5 belongs to tsa40
  n <- resolveTsaOverlaps(d, list("tsa41", "tsa40"), bnd)
  expect_equal(n, c(tsa41 = 1L, tsa40 = 0L))
  for (f in c("inventory_init.tif", "inventory_2020.tif"))
    expect_true(is.na(terra::values(terra::rast(file.path(d, "tsa41", f)))[3, 1]))
  expect_equal(resolveTsaOverlaps(d, list("tsa41", "tsa40"), bnd), c(tsa41 = 0L, tsa40 = 0L))
})

test_that("the module parameter defaults to FALSE, so runs are unchanged unless asked", {
  src <- paste(readLines(moduleFile), collapse = "\n")
  expect_match(src, 'defineParameter\\("resolveTsaOverlaps", "logical", FALSE')
  expect_match(src, "isTRUE\\(P\\(sim\\)\\$resolveTsaOverlaps\\)")
})
