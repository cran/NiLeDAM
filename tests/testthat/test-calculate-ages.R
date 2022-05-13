test_that("calculateAges function returns the right output with right input", {
  # Load data into environment + prepare data
  data("srilanka")
  
  # Tests with right input
  expect_s4_class(calculateAges(srilanka, nloops = 10), "ages")
  
  expect_invisible(calculateAges(srilanka, nloops = 10))
  expect_silent(calculateAges(srilanka, nloops = 10, verbose = FALSE))
  
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  
  expect_equal(class(res@data), "data.frame")
  expect_equal(class(res@ages), "numeric") # should I test for vector instead (as specified in ages.R) ?
  expect_equal(class(res@ci), c("matrix","array"))
  expect_equal(class(res@sd), "numeric") # should I test for vector instead (as specified in ages.R) ?
  expect_equal(class(res@nloops), "numeric")
  expect_equal(class(res@level), "numeric")
  
  expect_equal(nrow(srilanka) == length(res@ages), TRUE)
})

test_that("calculateAges function returns the right output with wrong input", {
  # Load data into environment + prepare data
  data("srilanka")
  srilanka2 <- srilanka
  srilanka2$other <- 1
  
  # Tests wiht other inputs
  expect_error(calculateAges(srilanka2, nloops = 10), 
               "The input measures' class/number of columns is not correct. It must be a 'data.frame' containing exactly 6 columns. 
         Check the dataset.")
  expect_error(calculateAges(srilanka, nloops = 0), 
               "Invalid entry for the nloops parameter. It must be a numeric strictly larger than 1.")
  expect_error(calculateAges(srilanka, nloops = 10, level = "5%"),
               "Invalid entry for the level parameter. It must be a numeric between 0 and 1.")
})