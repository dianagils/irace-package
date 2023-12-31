withr::with_output_sink("test-forbidden.Rout", {

test_that("checkForbidden", {
  
test.checkForbidden <- function(param.file)
{
  params <- readParameters(param.file)
  confs <- readConfigurationsFile("configurations.txt", params)
  exp.confs <- readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        6     "x1"   3.5   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params)
  confs <- irace:::checkForbidden(confs, params$forbidden)
  rownames(confs) <- rownames(exp.confs) <- NULL
  expect_equal(confs, exp.confs)
}
  test.checkForbidden("parameters.txt")
  test.checkForbidden("logparameters.txt")
})

})
