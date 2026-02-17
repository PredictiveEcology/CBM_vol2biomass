
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK curves: V1", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-SK-V1"

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_vol2biomass",
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),
    params = list(CBM_vol2biomass = list(.useCache = FALSE)),

    cbmAdmin = read.csv(file.path(spadesTestPaths$testdata, "cbmAdmin.csv")),

    userGcLocations = data.frame(admin_abbrev = "SK", eco_id = 9, curveID = 55),
    userGcMeta = read.csv(file.path(spadesTestPaths$testdata, "SK_v1", "userGcMeta.csv")),
    userGcM3   = read.csv(file.path(spadesTestPaths$testdata, "SK_v1", "userGcM3.csv"))
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")


  ## Check output 'gcMeta' ---

  expect_true(!is.null(simTest$gcMeta))
  expect_true(inherits(simTest$gcMeta, "data.table"))

  expect_identical(data.table::key(simTest$gcMeta), "gcids")

  expect_equal(nrow(simTest$gcMeta), 1)
  expect_true("SK_9_55" %in% simTest$gcMeta$gcids)


  ## Check output 'gcIncrements' ----

  expect_true(!is.null(simTest$gcIncrements))
  expect_true(inherits(simTest$gcIncrements, "data.table"))
  expect_identical(data.table::key(simTest$gcIncrements), c("gcids", "age"))

  expect_identical(names(simTest$gcIncrements), c("gcids", "age", "merch_inc", "foliage_inc", "other_inc"))
  expect_identical(data.table::key(simTest$gcIncrements), c("gcids", "age"))

  expect_equal(nrow(simTest$gcIncrements), 1 * 251)
  expect_true("SK_9_55" %in% simTest$gcIncrements$gcids)


  ## Check output 'cumPoolsClean' ----

  expect_true(!is.null(simTest$cPoolsClean))
  expect_true(inherits(simTest$cPoolsClean, "data.table"))

  expect_true("SK_9_55" %in% simTest$cPoolsClean$gcids)


  ## Check output 'cumPoolsClean' ----

  expect_true(!is.null(simTest$cPoolsClean))
  expect_true(inherits(simTest$cPoolsClean, "data.table"))

  expect_equal(nrow(simTest$cPoolsClean), 1 * 251)
  expect_true("SK_9_55" %in% simTest$cPoolsClean$gcids)

})


