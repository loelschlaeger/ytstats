test_that("ytdata can be printed", {
  expect_s3_class(
    statistik_mit_lennart, 
    c("ytdata", "tbl", "data.frames")
  )
})

test_that("ytdata can be summarized", {
  summary <- summary(statistik_mit_lennart)
  expect_s3_class(
    summary,
    "summary.ytdata"
  )
})
