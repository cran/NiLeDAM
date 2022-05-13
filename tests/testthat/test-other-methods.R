test_that("methods for 'ages' objects work correctly", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  
  # Tests
  expect_equal(print(res), {cat(nrow(res@data),"ages and confidence intervals estimated from",
                                res@nloops,"bootstrap samples.\n","Summary:\n")
                            print(summary(res@ages))}
               )
  expect_equal(show(res), {cat(nrow(res@data),"ages and confidence intervals estimated from",
                               res@nloops,"bootstrap samples.\n","Summary:\n")
                           print(summary(res@ages))}
  )
  expect_equal(summary(res), {cat(nrow(res@data),"ages and confidence intervals estimated from",
                                  res@nloops,"bootstrap samples.\n","Summary:\n")
                              print(summary(res@ages))}
  )
})

test_that("methods for 'oneAgeTest' objects work correctly", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  res.test <- tests(res, nbmin = 1)
  
  # Tests
  expect_equal(print(res.test), {cat("Test if the",length(res.test@which.pop),"estimated ages are coming from",
                                     length(res.test@ages),"population(s).\n\n")
    if (res.test@h0) {
      cat("    The ages are not found to be unlikely coming from",length(res.test@ages),
          "population(s) at level",res.test@level*100,"%.\n",
          "   Chi2 test statistic:",res.test@S," ~ df:",res.test@df,"\n\n")
      cat("    Estimated ages:", res.test@ages, "\n\n")
      cat("    Population numbers:\n", res.test@which.pop)
      } else {
        cat("    The ages are found to be unlikely coming from",length(res.test@ages),
            "population(s) at level",res.test@level*100,"%.\n",
            "   Chi2 test statistic:",res.test@S," ~ df:",res.test@df,"\n\n")}
    })
  expect_equal(show(res.test), {cat("Test if the",length(res.test@which.pop),"estimated ages are coming from",
                                    length(res.test@ages),"population(s).\n\n")
    if (res.test@h0) {
      cat("    The ages are not found to be unlikely coming from",length(res.test@ages),
          "population(s) at level",res.test@level*100,"%.\n",
          "   Chi2 test statistic:",res.test@S," ~ df:",res.test@df,"\n\n")
      cat("    Estimated ages:", res.test@ages, "\n\n")
      cat("    Population numbers:\n", res.test@which.pop)
    } else {
      cat("    The ages are found to be unlikely coming from",length(res.test@ages),
          "population(s) at level",res.test@level*100,"%.\n",
          "   Chi2 test statistic:",res.test@S," ~ df:",res.test@df,"\n\n")}
  })
  expect_equal(summary(res.test), {cat("Test if the",length(res.test@which.pop),"estimated ages are coming from",
                                       length(res.test@ages),"population(s).\n\n")
    if (res.test@h0) {
      cat("    The ages are not found to be unlikely coming from",length(res.test@ages),
          "population(s) at level",res.test@level*100,"%.\n",
          "   Chi2 test statistic:",res.test@S," ~ df:",res.test@df,"\n\n")
      cat("    Estimated ages:", res.test@ages, "\n\n")
      cat("    Population numbers:\n", res.test@which.pop)
    } else {
      cat("    The ages are found to be unlikely coming from",length(res.test@ages),
          "population(s) at level",res.test@level*100,"%.\n",
          "   Chi2 test statistic:",res.test@S," ~ df:",res.test@df,"\n\n")}
  })

})

test_that("methods for 'ageTests' objects work correctly", {
  # Load data into environment + prepare data
  data("srilanka")
  res <- calculateAges(srilanka, nloops = 10, seed = 40522)
  res.test <- tests(res, nbmin = 1, nbmax = 3)
  
  # Tests
  expect_equal(print(res.test), {
    cat("Test if the",length(res.test@best.res@data@ages),
        "estimated ages are coming from\n",res.test@nb.pop,"\npopulation(s).\n\n")
    cat("    The ages are found to be likely coming from",res.test@best.nb,
        "population(s) at level",res.test@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",res.test@best.res@S," ~ df:",res.test@best.res@df,"\n\n")
    cat("    Estimated ages:", res.test@best.res@ages, "\n\n")
    cat("    Population numbers:\n", res.test@best.res@which.pop)
  })
  expect_equal(show(res.test), {
    cat("Test if the",length(res.test@best.res@data@ages),
        "estimated ages are coming from\n",res.test@nb.pop,"\npopulation(s).\n\n")
    cat("    The ages are found to be likely coming from",res.test@best.nb,
        "population(s) at level",res.test@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",res.test@best.res@S," ~ df:",res.test@best.res@df,"\n\n")
    cat("    Estimated ages:", res.test@best.res@ages, "\n\n")
    cat("    Population numbers:\n", res.test@best.res@which.pop)
  })
  expect_equal(summary(res.test), {
    cat("Test if the",length(res.test@best.res@data@ages),
        "estimated ages are coming from\n",res.test@nb.pop,"\npopulation(s).\n\n")
    cat("    The ages are found to be likely coming from",res.test@best.nb,
        "population(s) at level",res.test@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",res.test@best.res@S," ~ df:",res.test@best.res@df,"\n\n")
    cat("    Estimated ages:", res.test@best.res@ages, "\n\n")
    cat("    Population numbers:\n", res.test@best.res@which.pop)
  })
})