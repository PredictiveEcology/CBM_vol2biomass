
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK curves: V2", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-SK-V2"

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
    params = list(CBM_vol2biomass = list(.useCache = FALSE, .plot = FALSE)),

    cbmAdmin = read.csv(file.path(spadesTestPaths$testdata, "cbmAdmin.csv")),

    userGcLocations = rbind(
      data.frame(admin_abbrev = "SK", eco_id = 6, species = "Trembling aspen", prodClass = "M"),
      data.frame(admin_abbrev = "SK", eco_id = 9, species = "Trembling aspen", prodClass = "P"),
      data.frame(admin_abbrev = "SK", eco_id = 9, species = "Jack pine",       prodClass = "P")
    ),
    userGcMeta = read.csv(file.path(spadesTestPaths$testdata, "SK_v2", "userGcMeta.csv")),
    userGcM3   = read.csv(file.path(spadesTestPaths$testdata, "SK_v2", "userGcM3.csv"))
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

  expect_identical(data.table::key(simTest$gcMeta), "gcID")

  expect_equal(nrow(simTest$gcMeta), 3)
  expect_true("SK_6_Trembling aspen_M" %in% simTest$cPoolsClean$gcID)
  expect_true("SK_9_Trembling aspen_P" %in% simTest$cPoolsClean$gcID)
  expect_true("SK_9_Jack pine_P"       %in% simTest$cPoolsClean$gcID)


  ## Check output 'gcIncrements' ----

  expect_true(!is.null(simTest$gcIncrements))
  expect_true(inherits(simTest$gcIncrements, "data.table"))

  expect_identical(names(simTest$gcIncrements), c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"))
  expect_identical(data.table::key(simTest$gcIncrements), c("gcID", "age"))

  expect_equal(nrow(simTest$gcIncrements), 3 * 251)
  expect_true("SK_6_Trembling aspen_M" %in% simTest$cPoolsClean$gcID)
  expect_true("SK_9_Trembling aspen_P" %in% simTest$cPoolsClean$gcID)
  expect_true("SK_9_Jack pine_P"       %in% simTest$cPoolsClean$gcID)


  ## Check output 'cumPoolsClean' ----

  expect_true(!is.null(simTest$cPoolsClean))
  expect_true(inherits(simTest$cPoolsClean, "data.table"))

  expect_equal(nrow(simTest$cPoolsClean), 3 * 251)
  expect_true("SK_6_Trembling aspen_M" %in% simTest$cPoolsClean$gcID)
  expect_true("SK_9_Trembling aspen_P" %in% simTest$cPoolsClean$gcID)
  expect_true("SK_9_Jack pine_P"       %in% simTest$cPoolsClean$gcID)

})

test_that("Module: SK curves: V2: without curve smoothing", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-SK-V2-raw"

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
    params = list(CBM_vol2biomass = list(.useCache = FALSE, .plot = FALSE, smooth = FALSE)),

    cbmAdmin = read.csv(file.path(spadesTestPaths$testdata, "cbmAdmin.csv")),

    userGcLocations = rbind(
      data.frame(admin_abbrev = "SK", eco_id = 9, species = "Trembling aspen", prodClass = "P")
    ),
    userGcMeta = read.csv(file.path(spadesTestPaths$testdata, "SK_v2", "userGcMeta.csv")),
    userGcM3   = read.csv(file.path(spadesTestPaths$testdata, "SK_v2", "userGcM3.csv"))
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")

  # Check output 'gcIncrements'
  rawConvert <- data.table::fread(file.path(spadesTestPaths$testdata, "SK_v2", "convertM3biom_SK_9_Trembling aspen_P.csv"))
  expect_equal(simTest$gcIncrements[, merch_inc],   c(0, rawConvert[, totMerch]))
  expect_equal(simTest$gcIncrements[, foliage_inc], c(0, rawConvert[, fol]))
  expect_equal(simTest$gcIncrements[, other_inc],   c(0, rawConvert[, other]))

  # For reference: test data creation
  # rawConvert <- CBMutils::convertM3biom(
  #   meta     = data.table::data.table(gcids = 1, spatial_unit_id = 28, species = "Trembling aspen"),
  #   gCvalues = data.table::data.table(gcids = 1, simTestInit$userGcM3[curveID == 7,]),
  #   spsMatch = data.table::data.table(species = "Trembling aspen", canfi_species = 1201, genus = "POPU"),
  #   ecozones = data.table::data.table(simTestInit$cbmAdmin),
  #   params3  = simTestInit$table3,
  #   params4  = simTestInit$table4,
  #   params5  = simTestInit$table5,
  #   params6  = simTestInit$table6,
  #   params7  = simTestInit$table7
  # )
  # rawConvert <- rbind(c(0, 0, 0), rawConvert)
  # rawConvert <- diff(rawConvert * 0.5)
  # write.csv(rawConvert, file.path(spadesTestPaths$testdata, "SK_v2", "convertM3biom_SK_9_Trembling aspen_P.csv"),
  #           row.names = FALSE, quote = FALSE)

})



