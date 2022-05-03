library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question

library(readxl)
library(dplyr)
library(ggplot2)
fexp <- read_excel("FieldExperiment.xlsx")
fexp <- data.frame(fexp)
fexp$DATE <- as.Date(fexp$DATE)
fexp$WEEK <- factor(fexp$WEEK)


test_that("Q2 Names (visible)", {
  
  expect_true(dim(fexp2)[1] == 4410)
  expect_true(dim(fexp2)[2] == 11)
  expect_true(names(fexp2)[1] == "DATE")
  expect_true(names(fexp2)[4] == "GP")
  expect_true(names(fexp2)[9] == "WebSales")

})

test_that("Q2 Values (visible)", {
  
  expect_equal(fexp2$GP[3], .85, tolerance = 1e-1)
  expect_equal(fexp2$FR[10], 2.78, tolerance = 1e-1)
  expect_equal(fexp2$POPN[1001], 74500, tolerance = 1e-1)
  expect_equal(fexp2$GR[3333], 0, tolerance = 1e-1)
  expect_equal(fexp2$FP[4300], 20.24, tolerance = 1e-1)

})

test_that("Q3 (visible)", {
  
  expect_equal(SpendTotals$GP_spend, 20517.62, tolerance = 1)
  expect_equal(SpendTotals$GB_spend, 20655.67, tolerance = 1)
  expect_equal(SpendTotals$FB_spend, 59727.54, tolerance = 1)

})

test_that("Q4 Structure (visible)", {
  
  expect_true(dim(top10gp)[1] == 10)
  expect_true(names(top10gp)[2] == "GP_spend")
              
})
  
test_that("Q4 Values (visible)", {
  
  expect_equal(top10gp$GP_spend[3], 849.19, tolerance = 1)
  expect_equal(sum(top10gp$GP_spend), 7270.9, tolerance = 1)
  
})

test_that("Q5 (visible)", {
  
  expect_equal(top10gr$GR_spend[3], 1034.17, tolerance = 1)
  expect_equal(sum(top10gr$GR_spend), 8581.76, tolerance = 1)
  
})

test_that("Q6 (visible)", {
  
  expect_equal(top10fp$FP_spend[3], 1194.42, tolerance = 1)
  expect_equal(sum(top10fp$FP_spend), 12509.34, tolerance = 1)
  
})

test_that("Q7 Structure (visible)", {
  
  expect_true(dim(top10spend)[1] == 10)
  expect_true(dim(top10spend)[2] == 6)
  expect_true(names(top10spend)[2] == "GP_spend")
  expect_true(names(top10spend)[5] == "FP_spend")
  
})

test_that("Q7 GP_values (visible)", {
  
  expect_equal(top10spend$GP_spend[4], 678.95, tolerance = 1) 
  expect_equal(top10spend$GP_spend[9], 521.19, tolerance = 1) 
  expect_equal(sum(top10spend$GP_spend), 7270.9, tolerance = 1) 
  
})

test_that("Q7 GR_values (visible)", {
  
  expect_equal(top10spend$GR_spend[4], 688.21, tolerance = 1) 
  expect_equal(top10spend$GR_spend[9], 626.26, tolerance = 1) 
  expect_equal(sum(top10spend$GR_spend), 8024.4, tolerance = 1) 
  
})

test_that("Q7 Other_values (visible)", {
  
  expect_equal(top10spend$FP_spend[4], 1194.42, tolerance = 1) 
  expect_equal(top10spend$GB_spend[9], 277.93, tolerance = 1) 
  expect_equal(sum(top10spend$GR_spend), 8024.4, tolerance = 1) 
  
})

test_that("Q8 Structure (visible)", {
  
  expect_true(dim(top25pcsales)[1] == 25)
  expect_true(dim(top25pcsales)[2] == 2)
  expect_true(names(top25pcsales)[1] == "DMA_NAME")
  expect_true(names(top25pcsales)[2] == "PCSales")
  
})

test_that("Q8 Values (visible)", {
  
  expect_equal(top25pcsales$PCSales[5], .002009428, tolerance = 1e-5) 
  expect_equal(top25pcsales$PCSales[14], .00166192, tolerance = 1e-5) 
  expect_equal(top25pcsales$PCSales[22], .00147231, tolerance = 1e-5) 

})

test_that("Q9 Structure (visible)", {
  
  expect_true(dim(top5days)[1] == 5)
  expect_true(dim(top5days)[2] == 4)
  expect_true(names(top5days)[1] == "DATE")
  expect_true(names(top5days)[3] == "Total_Google_Spend")
  
})

test_that("Q9 Values (visible)", {
  
  expect_equal(top5days$Total_Ad_Spend[2], 7445.44, tolerance = 1) 
  expect_equal(top5days$Total_Google_Spend[3], 3358.7, tolerance = 1) 
  expect_equal(top5days$Total_Ad_Spend[5], 6964.08, tolerance = 1) 
  expect_equal(top5days$Total_Facebook_Spend[4], 3902.59, tolerance = 1) 
  
})

test_that("Q10 Structure (visible)", {
  
  expect_true(dim(wksales)[1] == 9)
  expect_true(dim(wksales)[2] == 3)
  expect_true(names(wksales)[2] == "WEEK")
  expect_true(names(wksales)[3] == "TotSales")
  
})

test_that("Q10 Values A (visible)", {
  
  expect_equal(wksales$TotSales[2], 510.14, tolerance = 1) 
  expect_equal(wksales$TotSales[3], 118.53, tolerance = 1) 
  expect_equal(wksales$TotSales[5], 19.55, tolerance = 1) 
  
})

test_that("Q10 Values B (visible)", {
  
  expect_equal(wksales$TotSales[7], 893.78, tolerance = 1) 
  expect_equal(wksales$TotSales[8], 692.05, tolerance = 1) 
  expect_equal(wksales$TotSales[9], 1887.95, tolerance = 1) 
  
})

test_that("Q11 Structure (visible)", {
  
  expect_true(dim(salesstats)[1] == 1)
  expect_true(dim(salesstats)[2] == 4)
  expect_true(names(salesstats)[2] == "Max")
  expect_true(names(salesstats)[3] == "Medn")
  
})

test_that("Q11 Values (visible)", {
  
  expect_equal(salesstats$Min, 0, tolerance = 1e-2) 
  expect_equal(salesstats$Max, 39572.47, tolerance = 1) 
  expect_equal(salesstats$Medn, 1131.575, tolerance = 1) 
  expect_equal(salesstats$Mean, 2731.762, tolerance = 1) 
 
})

