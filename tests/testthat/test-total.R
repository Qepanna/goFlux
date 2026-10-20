# ---------------------------------------------------------------------------
# Total flux and error propagation.
#
# The additivity of the two pathways and the quadrature of their standard
# errors are the two algebraic claims of the paper, so they are tested
# exactly rather than approximately.
# ---------------------------------------------------------------------------

goAquaFlux.total <- get("goAquaFlux.total", envir = asNamespace("goFlux"))

mock_component <- function(flux, SE, F_tot2pts = NA_real_) {
  list(flux = flux, SE = SE, F_tot2pts = F_tot2pts)
}


test_that("the total flux is the exact sum of the two pathways", {

  res <- goAquaFlux.total(ebullition_flux = mock_component(3, 0.3),
                          diffusive_flux  = mock_component(4, 0.4))

  expect_equal(res$flux, 7)
})


test_that("the standard errors add in quadrature", {

  res <- goAquaFlux.total(ebullition_flux = mock_component(3, 3),
                          diffusive_flux  = mock_component(4, 4))

  expect_equal(res$SE, 5)                   # sqrt(3^2 + 4^2)
})


test_that("a zero ebullitive term leaves the diffusive flux untouched", {

  res <- goAquaFlux.total(ebullition_flux = mock_component(0, 0),
                          diffusive_flux  = mock_component(4, 0.4))

  expect_equal(res$flux, 4)
  expect_equal(res$SE,   0.4)
})


test_that("a missing component yields NA with an explanatory message", {

  res <- goAquaFlux.total(ebullition_flux = mock_component(NA_real_, NA_real_),
                          diffusive_flux  = mock_component(4, 0.4))

  expect_true(is.na(res$flux))
  expect_true(is.na(res$SE))
  expect_match(res$message, "could not be computed")
})


test_that("a NULL component is handled without error", {

  res <- goAquaFlux.total(ebullition_flux = NULL,
                          diffusive_flux  = mock_component(4, 0.4))

  expect_true(is.na(res$flux))
  expect_match(res$message, "NULL")
})


test_that("a missing SE propagates to NA without losing the flux", {

  res <- goAquaFlux.total(ebullition_flux = mock_component(3, NA_real_),
                          diffusive_flux  = mock_component(4, 0.4))

  expect_equal(res$flux, 7)
  expect_true(is.na(res$SE))
})


test_that("the ratio to the two-point estimate is computed when available", {

  res <- goAquaFlux.total(
    ebullition_flux = mock_component(3, 0.3, F_tot2pts = 14),
    diffusive_flux  = mock_component(4, 0.4))

  expect_equal(res$ratio, 0.5)              # 7 / 14
})


test_that("a zero two-point estimate does not produce a division by zero", {

  res <- goAquaFlux.total(
    ebullition_flux = mock_component(3, 0.3, F_tot2pts = 0),
    diffusive_flux  = mock_component(4, 0.4))

  expect_true(is.na(res$ratio))
})
