## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NiLeDAM)

## ----helpData, eval=FALSE-----------------------------------------------------
# ?srilanka

## ----load_data----------------------------------------------------------------
data("srilanka")

## ----dimData------------------------------------------------------------------
dim(srilanka)

## ----summaryData--------------------------------------------------------------
summary(srilanka)

## ----helpCalculate, eval=FALSE------------------------------------------------
# ?calculateAges

## ----calculate----------------------------------------------------------------
calculated.ages <- calculateAges(srilanka, nloops = 10, seed = 12, verbose = TRUE)

## ----ages---------------------------------------------------------------------
calculated.ages@ages

## ----CI-----------------------------------------------------------------------
calculated.ages@ci

## ----helpTests, eval=FALSE----------------------------------------------------
# ?tests

## ----tests--------------------------------------------------------------------
res.tests <- tests(calculated.ages, nbmin = 1, nbmax = 3, verbose = TRUE)

## ----plotTests, fig.height=5, fig.width=8-------------------------------------
plot(res.tests)

## ----plotTests2, fig.height=5, fig.width=8------------------------------------
popline(res.tests)

## ----new_data-----------------------------------------------------------------
srilanka2 <- srilanka[-(1:8), ]

## ----calculate2---------------------------------------------------------------
calculated.ages <- calculateAges(srilanka2, nloops = 10, seed = 12)

## ----tests2-------------------------------------------------------------------
res.tests <- tests(calculated.ages, nbmax = 3)

## ----plotTestsB, fig.height=5, fig.width=8------------------------------------
plot(res.tests)

## ----plotTests2B, fig.height=5, fig.width=8-----------------------------------
popline(res.tests)

## ----niledamGUI, eval=FALSE---------------------------------------------------
# liveNiLeDAM()

