
# PPCA same q - UrineSpectra ---------------------------------------
test_that("Check q=1:4 PPCA output exists and has values using UrineSpectra", {
  PPCA_output <- PPCA(UrineSpectra[[1]], q_min=3, q_max=3)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCA same q - BrainSpectra ---------------------------------------
test_that("Check q=1:4 PPCA output exists and has values using BrainSpectra", {
  PPCA_output <- PPCA(BrainSpectra[[1]], q_min=3, q_max=3)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})
# PPCA q=1:4 - UrineSpectra ----------------------------------------
test_that("Check q=1:4 PPCA output exists and has values using UrineSpectra", {
  PPCA_output <- PPCA(UrineSpectra[[1]], q_max=4)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCA q=1:4 - BrainSpectra ----------------------------------------
test_that("Check q=1:4 PPCA output exists and has values using BrainSpectra", {
  PPCA_output <- PPCA(BrainSpectra[[1]], q_max=4)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings, x_axis_PC = 2, y_axis_PC = 3))
  expect_invisible(plot.PPCA_score(PPCA_output$score, x_axis_PC = 2, y_axis_PC = 3))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCA q=1:2, B=5 - UrineSpectra --------------------------------
test_that("Check q=1:2 with Bootstrap=5 PPCA output exists and has values with UrineSpectra", {
  PPCA_output <- PPCA(UrineSpectra[[1]], q_max=2, B=5)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$loadings_sd, "double")
  expect_type(PPCA_output$significant_x, "list")
  expect_type(PPCA_output$significant_x$PC1, "character")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  expect_is(PPCA_output$significant_x, "PPCA_significant")

  # Check plots
  expect_invisible(plot.PPCA(PPCA_output))
  expect_invisible(plot.PPCA_significant(PPCA_output$significant_x, UrineSpectra[[1]]))

  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCA q=1:2, B=5 - BrainSpectra --------------------------------
test_that("Check q=1:2 with Bootstrap=5 PPCA output exists and has values with BrainSpectra", {
  PPCA_output <- PPCA(BrainSpectra[[1]], q_max=2, B=5)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$loadings_sd, "double")
  expect_type(PPCA_output$significant_x, "list")
  expect_type(PPCA_output$significant_x$PC1, "character")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  expect_is(PPCA_output$significant_x, "PPCA_significant")

  # Check plots
  expect_invisible(plot.PPCA(PPCA_output))
  expect_invisible(plot.PPCA_significant(PPCA_output$significant_x, BrainSpectra[[1]]))

  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})

# PPCCA same q - UrineSpectra ----------------------------------------
test_that("Check q=1:4 PPCCA output exists and has values using UrineSpectra", {
  PPCA_output <- PPCA(UrineSpectra[[1]], covariates_data = UrineSpectra[[2]], q_min=3,q_max=3)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$influence, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})

# PPCCA same q - BrainSpectra ----------------------------------------
test_that("Check q=1:4 PPCCA output exists and has values using BrainSpectra", {
  PPCA_output <- PPCA(BrainSpectra[[1]], covariates_data = BrainSpectra[[2]], q_min=3,q_max=3)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$influence, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})

# PPCCA q=1:4 - UrineSpectra ----------------------------------------
test_that("Check q=1:4 PPCCA output exists and has values using UrineSpectra", {
  PPCA_output <- PPCA(UrineSpectra[[1]], covariates_data = UrineSpectra[[2]], q_max=4)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$influence, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCCA q=1:4 - BrainSpectra ----------------------------------------
test_that("Check q=1:4 PPCCA output exists and has values using BrainSpectra", {
  PPCA_output <- PPCA(BrainSpectra[[1]], covariates_data = BrainSpectra[[2]], q_max=4)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$influence, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  # Check plots
  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings, x_axis_PC = 2, y_axis_PC = 3))
  expect_invisible(plot.PPCA_score(PPCA_output$score, x_axis_PC = 2, y_axis_PC = 3))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCCA q=1:2, B=5 - UrineSpectra --------------------------------
test_that("Check q=1:2 with Bootstrap=5 PPCCA output exists and has values with UrineSpectra", {
  PPCA_output <- PPCA(UrineSpectra[[1]], covariates_data = UrineSpectra[[2]], q_max=2, B=5)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$loadings_sd, "double")
  expect_type(PPCA_output$significant_x, "list")
  expect_type(PPCA_output$significant_x$PC1, "character")

  expect_type(PPCA_output$influence, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  expect_is(PPCA_output$significant_x, "PPCA_significant")

  # Check plots
  expect_invisible(plot.PPCA(PPCA_output))
  expect_invisible(plot.PPCA_significant(PPCA_output$significant_x, UrineSpectra[[1]]))

  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})


# PPCCA q=1:2, B=5 - BrainSpectra --------------------------------
test_that("Check q=1:2 with Bootstrap=5 PPCCA output exists and has values with BrainSpectra", {
  PPCA_output <- PPCA(BrainSpectra[[1]], covariates_data = BrainSpectra[[2]], q_max=2, B=5)

  # Check values exists
  expect_type(PPCA_output, "list")
  expect_type(PPCA_output$sigma2, "double")
  expect_type(PPCA_output$loadings, "double")
  expect_type(PPCA_output$score, "list")
  expect_type(PPCA_output$score$score, "double")
  expect_type(PPCA_output$score$score_var, "double")
  expect_type(PPCA_output$bic, "double")
  expect_type(PPCA_output$aic, "double")
  expect_type(PPCA_output$PoV, "double")
  expect_type(PPCA_output$diagnostic, "list")
  expect_type(PPCA_output$diagnostic$BIC, "double")
  expect_type(PPCA_output$diagnostic$PoV, "double")
  expect_type(PPCA_output$max_ll, "double")

  expect_type(PPCA_output$loadings_sd, "double")
  expect_type(PPCA_output$significant_x, "list")
  expect_type(PPCA_output$significant_x$PC1, "character")

  expect_type(PPCA_output$influence, "double")

  # Check class is properly assigned
  expect_is(PPCA_output, "PPCA")
  expect_is(PPCA_output$loadings, "PPCA_loadings")
  expect_is(PPCA_output$score, "PPCA_score")
  expect_is(PPCA_output$diagnostic, "PPCA_diagnostic")

  expect_is(PPCA_output$significant_x, "PPCA_significant")

  # Check plots
  expect_invisible(plot.PPCA(PPCA_output))
  expect_invisible(plot.PPCA_significant(PPCA_output$significant_x, BrainSpectra[[1]]))

  expect_invisible(plot.PPCA_loadings(PPCA_output$loadings))
  expect_invisible(plot.PPCA_score(PPCA_output$score))
  expect_invisible(plot.PPCA_diagnostic(PPCA_output$diagnostic))
})
