test_that("'oneAgeTest' has right format", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  
  # Tests
  expect_s4_class(tests(res), "oneAgeTest")
  
  res.test <- tests(res)
  
  expect_equal(res.test@h0, FALSE)
  
  expect_s4_class(res.test@data, "ages")
  expect_equal(class(res.test@ages), "numeric") # should be vector ?
  expect_equal(class(res.test@sd), "numeric") # should be vector ?
  expect_equal(class(res.test@ic), c("matrix","array"))
  expect_equal(class(res.test@S), "numeric")
  expect_equal(class(res.test@thres), "numeric")
  expect_equal(class(res.test@df), "numeric")
  expect_equal(class(res.test@level), "numeric")
  expect_equal(class(res.test@which.pop), "numeric") # should be vector ?
  
  expect_equal(length(res.test@ages), 1)
})

test_that("'ageTests' has right format", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  
  # Tests
  expect_s4_class(tests(res, nbmin = 1, nbmax = 3), "ageTests")
  
  res.test <- tests(res, nbmin = 1, nbmax = 3)
  
  expect_equal(res.test@best.res@h0, TRUE)
  
  expect_s4_class(res.test@best.res, "oneAgeTest")
  expect_equal(class(res.test@nb.pop), "integer")
  expect_equal(class(res.test@best.nb), "integer")
  
  expect_equal(length(res.test@nb.pop) > 1, TRUE)
})

test_that("tests method gives right output", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  
  expect_invisible(tests(res))
  expect_silent(tests(res, verbose = FALSE))
  
  # Test with wrong input
  # expect_error(tests(srilanka), "Invalid input for object. It must be an object of 'ages' class.")
  expect_error(tests(res, nbmin = 0), "Invalid input for nbmin. Must be a strictly positive numeric value.")
  expect_error(tests(res, nbmin = 1, nbmax = 1), "'nbmax' must be strictly larger than 'nbmin'.")
  
  # Test with right input
  # only 1 number
  ## H0 FALSE
  res.one <- tests(res, nbmax = NULL)
  expect_equal(print(res.one), {
               cat("Test if the",length(res.one@which.pop),"estimated ages are coming from",
                   length(res.one@ages),"population(s).\n\n")
               cat("    The ages are found to be unlikely coming from",length(res.one@ages),
                   "population(s) at level",res.one@level*100,"%.\n",
                   "   Chi2 test statistic:",res.one@S," ~ df:",res.one@df,"\n\n")
  })
  ## HO TRUE
  res.one2 <- tests(res, nbmin = 3, nbmax = NULL)
  expect_equal(print(res.one2), {
    cat("Test if the",length(res.one2@which.pop),"estimated ages are coming from",
        length(res.one2@ages),"population(s).\n\n")
    cat("    The ages are not found to be unlikely coming from",length(res.one2@ages),
        "population(s) at level",res.one2@level*100,"%.\n",
        "   Chi2 test statistic:",res.one2@S," ~ df:",res.one2@df,"\n\n")
    cat("    Estimated ages:", res.one2@ages, "\n\n")
    cat("    Population numbers:\n", res.one2@which.pop)
  })
  
  # multiple
  res.mult <- tests(res, nbmin = 1, nbmax = 3)
  expect_equal(print(res.mult), {
    cat("Test if the",length(res.mult@best.res@data@ages),
        "estimated ages are coming from\n",res.mult@nb.pop,"\npopulation(s).\n\n")
    cat("    The ages are found to be likely coming from",res.mult@best.nb,
        "population(s) at level",res.mult@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",res.mult@best.res@S," ~ df:",res.mult@best.res@df,"\n\n")
    cat("    Estimated ages:", res.mult@best.res@ages, "\n\n")
    cat("    Population numbers:\n", res.mult@best.res@which.pop)
  })
  
  # multiple when nbmin higher than "true" number
  res.mult.high <- tests(res, nbmin = 4, nbmax = 6)
  ## printing object
  expect_equal(print(res.mult.high), {
    cat("Test if the",length(res.mult.high@best.res@data@ages),
        "estimated ages are coming from\n",res.mult.high@nb.pop,"\npopulation(s).\n\n")
    cat("    No valid number of populations in the sequence tested. (",
        min(res.mult.high@nb.pop)," to ",max(res.mult.high@nb.pop),")\n\n")
    cat("    The ages are not found to be likely coming from",res.mult.high@best.nb,
        "population(s) at level",res.mult.high@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",res.mult.high@best.res@S," ~ df:",res.mult.high@best.res@df,"\n\n")
    cat("    Estimated ages:", res.mult.high@best.res@ages, "\n\n")
    cat("    Population numbers:\n", res.mult.high@best.res@which.pop, "\n\n")
  })
  ## executing method
  expect_warning(tests(res, nbmin = 4, nbmax = 6),
    warning("The population numbers tested are too high. The test's final result is lower than 'nbmin' input.")
  )
})