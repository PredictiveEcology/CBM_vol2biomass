
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: SK", {

  ## Run simInit and spades ----

  skip_if(!"main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")))

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "integration-SK",
    times       = list(start = 2026, end = 2026),
    modules     = c(
      paste0("PredictiveEcology/CBM_defaults@",    ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/CBM_dataPrep_SK@", ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/CBM_dataPrep@",    ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      "CBM_vol2biomass",
      paste0("PredictiveEcology/CBM_core@",        ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development"))
    ),

    masterRaster = terra::rast(
      ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      res  = 30,
      vals = 0L,
      crs  = "EPSG:3979"
    )
  )

  # Run simInit
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- suppressWarnings(SpaDES.core::spades(simTestInit))
  expect_s4_class(simTest, "simList")

  # Check outputs
  expect_true(!is.null(simTest$emissionsProducts))

})


