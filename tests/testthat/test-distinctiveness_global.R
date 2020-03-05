context("Test global distinctiveness")


# Should fail if provided object is not dist() nor matrix
testthat::test_that("fails gracefully with bad input", {

  # Random object
  expect_error(distinctiveness_global("A"),
               regexp = "Input should be a dist() object or a square matrix",
               fixed = TRUE)
  expect_error(distinctiveness_global(1),
               regexp = "Input should be a dist() object or a square matrix",
               fixed = TRUE)
  # data.frame
  expect_error(distinctiveness_global(data.frame(a = c(1, 0, -1))),
               regexp = "Input should be a dist() object or a square matrix",
               fixed = TRUE)
  # Non-square matrix
  expect_error(distinctiveness_global(matrix(1, nrow = 3, ncol = 2)),
               regexp = "Input should be a dist() object or a square matrix",
               fixed = TRUE)

  expect_error(distinctiveness_global(dist(c(1, 0, -1)), 45.3),
               paste0("Provided column name for regional/global Di should be",
                      " character"),
               fixed = TRUE)

})

# Should work with dist or matrix objects
testthat::test_that("works with dist() objects & distance matrices", {
  expect_silent(distinctiveness_global(dist(c(1, 0, -1))))
  expect_silent(distinctiveness_global(as.matrix(dist(c(1, 0, -1)))))

  dist_di = distinctiveness_global(dist(c(1, 0, -1)))
  mat_di  = distinctiveness_global(as.matrix(dist(c(1, 0, -1))))


  expect_s3_class(dist_di, "data.frame")
  expect_named(dist_di, c("species", "global_di"))
  expect_type(dist_di$species, "character")
  expect_type(dist_di$global_di, "double")
  expect_equal(dist_di,
               data.frame(
                 species   = c("1", "2", "3"),
                 global_di = c(1.5, 1, 1.5),
                 stringsAsFactors = FALSE
               ))
  expect_equal(dist_di, mat_di)

  # Test for distance object named differently
  named_dist = as.matrix(dist(c(1, 0, -1)))
  names(dimnames(named_dist)) = rep("sp", 2)

  named_di = distinctiveness_global(named_dist)
  expect_s3_class(named_di, "data.frame")
  expect_named(named_di, c("sp", "global_di"))
  expect_type(named_di$sp, "character")
  expect_type(named_di$global_di, "double")
  expect_equal(named_di,
               data.frame(
                 sp        = c("1", "2", "3"),
                 global_di = c(1.5, 1, 1.5),
                 stringsAsFactors = FALSE
               ))

  # Rename distinctiveness
  renamed_di = distinctiveness_global(named_dist, "reg_di")
  expect_s3_class(renamed_di, "data.frame")
  expect_named(renamed_di, c("sp", "reg_di"))
  expect_type(renamed_di$sp, "character")
  expect_type(renamed_di$reg_di, "double")
  expect_equal(renamed_di,
               data.frame(
                 sp     = c("1", "2", "3"),
                 reg_di = c(1.5, 1, 1.5),
                 stringsAsFactors = FALSE
               ))
})
