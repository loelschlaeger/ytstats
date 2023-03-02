test_that("ytdata can be printed", {
  expect_s3_class(
    ytdata_example, 
    c("ytdata", "tbl", "data.frames")
  )
})

test_that("ytdata can be summarized", {
  summary <- summary(ytdata_example)
  expect_s3_class(
    summary,
    "summary.ytdata"
  )
  expect_snapshot(
    print(summary)
  )
})
