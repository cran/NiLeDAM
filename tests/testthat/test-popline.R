test_that("test result on 'oneAgeTest' plotted correctly", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  res.test <- tests(res, nbmin = 1)
  p <- popline(res.test)
  
  # Test
  expect_equal(class(p), c("gg","ggplot"))
  expect_equal(typeof(p), "list")
})

test_that("test result on 'ageTests' plotted correctly", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  res.test <- tests(res, nbmin = 1, nbmax = 3)
  p <- popline(res.test)
  
  # Test
  expect_equal(class(p), c("gg","ggplot"))
  expect_equal(typeof(p), "list")
})