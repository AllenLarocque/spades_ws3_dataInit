# Tests for combining several TSA inventories into one landscape. Run from the module folder:
#   testthat::test_file("tests/testthat/test-tsaLandscape.R")

moduleFile <- if (file.exists("spades_ws3_dataInit.R")) "spades_ws3_dataInit.R" else
  testthat::test_path("..", "..", "spades_ws3_dataInit.R")
for (e in parse(moduleFile))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("labelsToNumeric", "combineTsaRasters", "tsaOverlapCount"))
    eval(e)

# One TSA's landscape as spades_ws3_dataInit's recompile.rs() makes it: fmuid numeric; thlb and au
# categorical (terra::subst() with the HDT's character values), each TSA with its own category table.
.tsaStack <- function(xmin, fmuid, thlb, au, age) {
  key <- terra::rast(nrows = 1, ncols = length(age), xmin = xmin, xmax = xmin + length(age), ymin = 0, ymax = 1,
                     vals = seq_along(age))
  r <- c(terra::subst(key, seq_along(age), rep(fmuid, length(age))),
         terra::subst(key, seq_along(age), as.character(thlb)),
         terra::subst(key, seq_along(age), as.character(au)),
         terra::subst(key, seq_along(age), rep(1, length(age))),
         terra::rast(key, vals = age))
  names(r) <- c("fmuid", "thlb", "au", "blockid", "age")
  r
}

test_that("labelsToNumeric replaces category indices with the labels, and leaves numeric layers alone", {
  r <- .tsaStack(0, 41, thlb = c(0, 1, 1), au = c(4101002, 4101006, 4101002), age = c(10, 20, 30))
  expect_true(terra::is.factor(r[["thlb"]]))
  n <- labelsToNumeric(r)
  expect_false(any(terra::is.factor(n)))
  expect_equal(terra::values(n[["thlb"]])[, 1], c(0, 1, 1))
  expect_equal(terra::values(n[["au"]])[, 1], c(4101002, 4101006, 4101002))
  expect_equal(terra::values(n[["age"]])[, 1], c(10, 20, 30))
  expect_equal(names(n), names(r))
})

test_that("combining two TSAs keeps each TSA's own thlb and AU (mosaic and merge)", {
  # Regression: mosaic(fun = "mean") averaged the category indices of the two TSAs' different
  # category tables, so every pixel read as THLB and no AU matched a yield curve.
  a <- .tsaStack(0, 41, thlb = c(0, 1, 0), au = c(4101002, 4101006, 4103008), age = c(10, 20, 30))
  b <- .tsaStack(3, 40, thlb = c(1, 1, 0), au = c(4001002, 4001009, 4001002), age = c(40, 50, 60))
  for (approach in c("mosaic", "merge")) {
    m <- combineTsaRasters(list(tsa41 = a, tsa40 = b), approach)
    expect_equal(terra::values(m[["thlb"]])[, 1], c(0, 1, 0, 1, 1, 0), label = approach)
    expect_equal(terra::values(m[["au"]])[, 1], c(4101002, 4101006, 4103008, 4001002, 4001009, 4001002), label = approach)
    expect_equal(terra::values(m[["fmuid"]])[, 1], c(41, 41, 41, 40, 40, 40), label = approach)
    expect_equal(terra::values(m[["age"]])[, 1], c(10, 20, 30, 40, 50, 60), label = approach)
  }
})

test_that("a single TSA is returned unchanged (categorical layers kept)", {
  a <- .tsaStack(0, 41, thlb = c(0, 1, 0), au = c(4101002, 4101006, 4103008), age = c(10, 20, 30))
  m <- combineTsaRasters(list(tsa41 = a), "mosaic")
  expect_true(terra::is.factor(m[["thlb"]]))
  expect_equal(terra::values(m), terra::values(a))
})

test_that("cells in more than one TSA inventory are counted and reported", {
  a <- .tsaStack(0, 41, thlb = c(0, 1, 0), au = c(1, 2, 3), age = c(10, 20, 30))
  b <- .tsaStack(2, 40, thlb = c(1, 1, 0), au = c(4, 5, 6), age = c(40, 50, 60))   # cell x = 2.5 in both
  expect_equal(tsaOverlapCount(list(tsa41 = a, tsa40 = b)), 1L)
  expect_warning(combineTsaRasters(list(tsa41 = a, tsa40 = b), "mosaic"), "1 cell.*more than one TSA")
})
