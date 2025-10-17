defineModule(sim, list(
  name = "CBM_vol2biomass_SK",
  description = paste("A module to prepare the user-provided growth and yield information for use",
                      "in the family of models spadesCBM - CBM-CFS3-like simulation of forest",
                      "carbon in the platform SpaDES. This module takes in user-provided m3/ha",
                      "and meta data for teh growth curves and returns annual increments for",
                      "the aboveground live c-pools."),
  keywords = "",
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_vol2biomass_SK = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "CBM_vol2biomass_SK.Rmd")),
  reqdPkgs = list(
    "PredictiveEcology/CBMutils@development (>=2.0.3.0007)",
    "ggforce", "ggplot2", "ggpubr", "googledrive", "mgcv", "quickPlot", "robustbase", "data.table", "patchwork"
  ),
  parameters = rbind(
    defineParameter(
      "outputFigurePath", "character", NA, NA, NA,
      paste("Filepath to a directory where output figures will be saved.",
            "The default is a directory named 'CBM_vol2biomass_figures' within the simulation outputs directory.")
    ),
    defineParameter(
      ".plotInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".useCache", "logical", TRUE, NA, NA,
      paste(
        "Should this entire module be run with caching activated?",
        "This is generally intended for data-type modules, where stochasticity",
        "and time are not relevant"
      )
    )
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "curveID", objectClass = "character",
      desc = "Column(s) uniquely defining each growth curve in `userGcSPU` and `userGcMeta`. Defaults to 'curveID'."),
    expectsInput(
      objectName = "userGcSPU", objectClass = "data.frame",
      desc = "Growth curve locations with columns 'spatial_unit_id' and all columns named by `curveID`"),
    expectsInput(
      objectName = "userGcMeta", objectClass = "data.frame",
      desc = "Growth curve metadata with columns 'curveID' and all columns named by `curveID`",
      sourceURL = "https://drive.google.com/file/d/1ugECJVNkglSSQFVqnk5ayG6q38l6AWe9"),
    expectsInput(
      objectName = "userGcMetaURL", objectClass = "character",
      desc = "URL for userGcMeta"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volumes with columns 'curveID', `Age`, and `MerchVolume`.",
      sourceURL = "https://drive.google.com/file/d/13s7fo5Ue5ji0aGYRQcJi-_wIb2-4bgVN"),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"),
    expectsInput(
      objectName = "cbmAdminURL", objectClass = "character",
      desc = "URL for cbmAdmin"),
    expectsInput(
      objectName = "table3", objectClass = "data.frame",
      desc = "Stem wood biomass model parameters for merchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv"),
    expectsInput(
      objectName = "table3URL", objectClass = "character",
      desc = "URL for table 3"),
    expectsInput(
      objectName = "table4", objectClass = "data.frame", desc = "Stem wood biomass model parameters for nonmerchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv"),
    expectsInput(
      objectName = "table4URL", objectClass = "character",
      desc = "URL for table 4"),
    expectsInput(
      objectName = "table5", objectClass = "data.frame",
      desc = "Stem wood biomass model parameters for sapling-sized trees from Boudewyn et al. 2007.",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv"),
    expectsInput(
      objectName = "table5URL", objectClass = "character",
      desc = "URL for table 5"),
    expectsInput(
      objectName = "table6", objectClass = "data.frame",
      desc = "Proportion model parameters from Boudewyn et al. 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv"),
    expectsInput(
      objectName = "table6URL", objectClass = "character",
      desc = "URL for table 6"),
    expectsInput(
      objectName = "table7", objectClass = "data.frame",
      desc = "Caps on proportion models from Boudewyn et al. 2007.",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv"),
    expectsInput(
      objectName = "table7URL", objectClass = "character",
      desc = "URL for table 7")
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "volCurves", objectClass = "plot",
      desc = "Plot of all the growth curve provided by the user"),
    createsOutput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata with key 'gcids'"),
    createsOutput(
      objectName = "cPoolsClean", objectClass = "data.table",
      desc = "Tonnes of carbon/ha both cumulative and increments,
      for each growth curve id (in this data.table id and gcids are
      the same), by age and ecozone"),
    createsOutput(
      objectName = "growth_increments", objectClass = "data.table",
      desc = "Carbon increment matrix by age for each gcids")
  )
))

doEvent.CBM_vol2biomass_SK <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- Init(sim)

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  # Check input
  if ("gcids" %in% sim$curveID)           stop("'curveID' cannot contain \"gcids\"")
  if ("gcids" %in% names(sim$userGcMeta)) stop("'userGcMeta' cannot contain \"gcids\"")
  if ("gcids" %in% names(sim$userGcM3))   stop("'userGcM3' cannot contain \"gcids\"")

  reqCols <- list(
    userGcSPU  = c(sim$curveID, "spatial_unit_id"),
    userGcMeta = c("curveID", sim$curveID, "species"),
    userGcM3   = c("curveID", "Age", "MerchVolume")
  )

  if (!all(reqCols$userGcSPU %in% names(sim$userGcSPU))) stop(
    "userGcSPU must have columns: ", paste(shQuote(reqCols$userGcSPU), collapse = ", "))
  if (!all(reqCols$gcMeta %in% names(sim$gcMeta))) stop(
    "gcMeta must have columns: ", paste(shQuote(reqCols$gcMeta), collapse = ", "))
  if (!all(reqCols$userGcM3 %in% names(sim$userGcM3))) stop(
    "userGcM3 must have columns: ", paste(shQuote(reqCols$userGcM3), collapse = ", "))

  if (!all(sim$userGcM3$curveID %in% sim$userGcMeta$curveID)) {
    stop("There is a missmatch in the 'curveID' columns of userGcMeta and the userGcM3")
  }
  for (col in sim$curveID){
    if (!all(sim$userGcSPU[[col]] %in% sim$userGcMeta[[col]])) {
      stop("There is a missmatch in the ", sQuote(col), " columns of userGcSPU and userGcMeta")
    }
  }

  sim$userGcSPU  <- data.table::as.data.table(sim$userGcSPU)
  sim$userGcMeta <- data.table::as.data.table(sim$userGcMeta)
  sim$userGcM3   <- data.table::as.data.table(sim$userGcM3)

  ## SK: always include gc ID 55
  if (any(c(27, 28) %in% sim$userGcSPU$spatial_unit_id) && 55 %in% sim$userGcMeta$curveID){
    sim$userGcSPU <- unique(data.table::rbindlist(list(
      sim$userGcSPU,
      data.frame(spatial_unit_id = 28, curveID = 55)
    ), fill = TRUE))
  }

  ## user provides userGcM3: incoming cumulative m3/ha.
  ## table needs 3 columns: gcids, Age, MerchVolume
  # Here we check that ages increment by 1 each timestep,
  # if it does not, it will attempt to resample the table to make it so.
  ageJumps <- sim$userGcM3[, list(jumps = unique(diff(as.numeric(Age)))), by = curveID]
  idsWithJumpGT1 <- ageJumps[jumps > 1][["curveID"]]
  if (length(idsWithJumpGT1) > 0) {
    missingAboveMin <- sim$userGcM3[, approx(Age, MerchVolume, xout = setdiff(seq(0, max(Age)), Age)),
                                    by = curveID]
    setnames(missingAboveMin, c("x", "y"), c("Age", "MerchVolume"))
    sim$userGcM3 <- rbindlist(list(sim$userGcM3, na.omit(missingAboveMin)))
    setorderv(sim$userGcM3, c("curveID", "Age"))

    # Assertion
    ageJumps <- sim$userGcM3[, list(jumps = unique(diff(as.numeric(Age)))), by = curveID]
    idsWithJumpGT1 <- ageJumps[jumps > 1][["curveID"]]
    if (length(idsWithJumpGT1) > 0)
      stop("There are still yield curves that are not annually resolved")
  }

  # Creates/sets the vol2biomass outputs subfolder (inside the general outputs folder)
  figPath <- file.path(outputPath(sim), "CBM_vol2biomass_figures")
  sim$volCurves <- ggplot(data = sim$userGcM3, aes(x = Age, y = MerchVolume, group = "curveID", colour = factor("curveID"))) +
    geom_line() + theme_bw()
  SpaDES.core::Plots(sim$volCurves,
                     filename = "volCurves",
                     path = figPath,
                     ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                     types = "png")
  message("User: please look at the curve you provided via sim$volCurves or the volCurves.png file in the outputs folder")


  # START reducing Biomass model parameter tables --------------------------------------------

  thisAdmin <- sim$cbmAdmin[sim$cbmAdmin$SpatialUnitID %in% na.omit(sim$userGcSPU$spatial_unit_id), ]

  # subsetting Boudewyn tables to the ecozones/admin boundaries of the study area.
  # Some ecozones/boundaries are not in these tables, in these cases, the function replaces them in
  # thisAdmin to the closest equivalent present in the Boudewyn tables.
  stable3 <- boudewynSubsetTables(sim$table3, thisAdmin, thisAdmin$EcoBoundaryID)
  stable4 <- boudewynSubsetTables(sim$table4, thisAdmin, thisAdmin$EcoBoundaryID)
  stable5 <- boudewynSubsetTables(sim$table5, thisAdmin, thisAdmin$EcoBoundaryID)
  stable6 <- boudewynSubsetTables(sim$table6, thisAdmin, thisAdmin$EcoBoundaryID)
  stable7 <- boudewynSubsetTables(sim$table7, thisAdmin, thisAdmin$EcoBoundaryID)

  # END reducing Biomass model parameter tables -----------------------------------------------

  # START Reading in user provided meta data for growth curves --------------------------------------------
  # This could be a complete data frame with the same columns as gcMetaEg.csv OR is could be only curve
  # id and species.

  ## Check that all required columns are available, and if not, add them:
  if (any(!c("canfi_species", "sw_hw", "genus") %in% names(sim$userGcMeta))){

    sppMatchTable <- CBMutils::sppMatch(
      sim$userGcMeta$species, return = c("CanfiCode", "NFI", "Broadleaf"))[, .(
        canfi_species = CanfiCode,
        sw_hw         = data.table::fifelse(Broadleaf, "hw", "sw"),
        genus         = sapply(strsplit(NFI, "_"), `[[`, 1)
      )]

    sim$userGcMeta <- cbind(
      sim$userGcMeta[, .SD, .SDcols = setdiff(names(sim$userGcMeta), names(sppMatchTable))],
      sppMatchTable)
    rm(sppMatchTable)
  }


  # END Reading in user provided meta data for growth curves -----------------------------------------------

  # START processing curves from m3/ha to tonnes of C/ha then to annual increments
  # per above ground biomass pools -------------------------------------------

  # Create a new unique key defining each growth curve and spatial_unit_id
  sim$userGcSPU <- cbind(
    gcids = factor(
      CBMutils::gcidsCreate(sim$userGcSPU[, .SD, .SDcols = c("spatial_unit_id", sim$curveID)])
    ),
    sim$userGcSPU)
  data.table::setkey(sim$userGcSPU, gcids)

  sim$gcMeta <- merge(sim$userGcSPU, sim$userGcMeta, by = sim$curveID)
  data.table::setcolorder(sim$gcMeta, which(names(sim$gcMeta) == "gcids"))
  data.table::setkey(sim$gcMeta, gcids)

  if (!"ecozones" %in% names(sim$gcMeta)){
    sim$gcMeta <- merge(
      sim$gcMeta,
      sim$cbmAdmin[, .(spatial_unit_id = SpatialUnitID, ecozones = EcoBoundaryID)],
      by = "spatial_unit_id")
  }

  gcM3 <- merge(sim$gcMeta, sim$userGcM3, by = "curveID", allow.cartesian = TRUE)[
    , .(gcids, Age, MerchVolume)]
  data.table::setkey(gcM3, gcids, Age)

  # 1. Calculate the translation (result is cPools or "cumulative AGcarbon pools")

  # Matching is 1st on species, then on gcids which gives us location (admin,
  # spatial unit and ecozone)
  fullSpecies <- unique(sim$gcMeta$species)

  cPools <- cumPoolsCreate(fullSpecies, sim$gcMeta, gcM3,
                             stable3, stable4, stable5, stable6, stable7, thisAdmin
                             ) |> Cache()

  # 2. Make sure the provided curves are annual
  ## if not, we need to extrapolate to make them annual
  minAgeId <- cPools[,.(minAge = max(0, min(age) - 1)), by = "gcids"]
  fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = "gcids"]
  # these are going to be 0s
  carbonVars <- data.table(gcids = unique(fill0s$gcids),
                           totMerch = 0,
                           fol = 0,
                           other = 0 )
  fiveOf7cols <- fill0s[carbonVars, on = "gcids"]
  otherVars <- cPools[,.(id = unique(id), ecozone = unique(ecozone)), by = "gcids"]
  add0s <- fiveOf7cols[otherVars, on = "gcids"]
  cPoolsRaw <- rbindlist(list(cPools,add0s), use.names = TRUE)
  set(cPoolsRaw, NULL, "age", as.numeric(cPoolsRaw$age))
  setorderv(cPoolsRaw, c("gcids", "age"))

  # 3. Fixing of non-smooth curves
  message(crayon::red("User: please inspect figures of the raw and smoothed translation of your growth curves in: ",
                    figPath))

  # 3.1 SK-specific fixes with birch curves:
  ## SK is a great example of poor performance of the Boudewyn et al 2007
  ## models. The "translation" does not work well with white birch (probably
  ## because there was not enough data in SK in the model-building data). So,
  ## the resulting curves are for fol and other are nonsensical. This can be
  ## seen by visually inspecting the curves going into the translations (run
  ## m3ToBiomPlots commented above). Here, the user, decided that after all the
  ## catches in place in the cSmoothPools failed, a hard fix was needed. The
  ## fol and other columns in gcids 37 and 58, will be replace by the fol and
  ## other of gcids 55.
  ## The user will have to decide which curves to replace and with what in their own study areas.
  if (any(cPoolsRaw$gcids == "27_37")) {
    cPoolsRaw[gcids == "27_37", fol   := cPoolsRaw[gcids == "28_55", fol]]
    cPoolsRaw[gcids == "27_37", other := cPoolsRaw[gcids == "28_55", other]]
  }
  if (any(cPoolsRaw$gcids == "28_58")) {
    cPoolsRaw[gcids == "28_58", fol   := cPoolsRaw[gcids == "28_55", fol]]
    cPoolsRaw[gcids == "28_58", other := cPoolsRaw[gcids == "28_55", other]]
  }
  if (any(cPoolsRaw$gcids == "27_58")) {
    cPoolsRaw[gcids == "27_58", fol   := cPoolsRaw[gcids == "28_55", fol]]
    cPoolsRaw[gcids == "27_58", other := cPoolsRaw[gcids == "28_55", other]]
  }
  if (any(cPoolsRaw$gcids == "27_38")) {
    cPoolsRaw[gcids == "27_38", fol   := cPoolsRaw[gcids == "28_55", fol]]
    cPoolsRaw[gcids == "27_38", other := cPoolsRaw[gcids == "28_55", other]]
  }
  if (any(cPoolsRaw$gcids == "27_39")) {
    cPoolsRaw[gcids == "27_39", fol   := cPoolsRaw[gcids == "28_55", fol]]
    cPoolsRaw[gcids == "27_39", other := cPoolsRaw[gcids == "28_55", other]]
  }
  if (any(cPoolsRaw$gcids == "28_60")) {
    cPoolsRaw[gcids == "28_60", fol   := cPoolsRaw[gcids == "28_55", fol]]
    cPoolsRaw[gcids == "28_60", other := cPoolsRaw[gcids == "28_55", other]]
  }

  # Smooth curves
  cPoolsClean <- cumPoolsSmooth(cPoolsRaw
                                  ) |> Cache()

  #Note: this will produce a warning if one of the curve smoothing efforts doesn't converge
  cPoolsSmoothPlot <- m3ToBiomPlots(inc = cPoolsClean,
                                    title = "Cumulative merch/fol/other by gcid")
  for (i in seq_along(cPoolsSmoothPlot)){
  SpaDES.core::Plots(cPoolsSmoothPlot[[i]],
                     filename = paste0("cPools_smoothed_postChapmanRichards_", i, ".png"),
                     path = figPath,
                     ggsaveArgs = list(width = 10, height = 5, units = "in", dpi = 300),
                     types = "png")
  }

  ## keeping the new curves - at this point they are still cumulative
  colNames <- c("totMerch", "fol", "other")
  set(cPoolsClean, NULL, colNames, NULL)
  colNamesNew <- grep("totMerch|fol|other", colnames(cPoolsClean), value = TRUE)
  setnames(cPoolsClean, old = colNamesNew, new = colNames)

  # 4. Calculating Increments
  incCols <- c("incMerch", "incFol", "incOther")
  cPoolsClean[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
                by = eval("gcids")]
  colsToUse33 <- c("age", "gcids", incCols)
  rawIncPlots <- m3ToBiomPlots(inc = cPoolsClean[, ..colsToUse33],
                               title = "Increments")
  for (i in seq_along(rawIncPlots)){
  SpaDES.core::Plots(rawIncPlots[[i]],
                     filename = paste0("increments_", i, ".png"),
                     path = figPath,
                     ggsaveArgs = list(width = 10, height = 5, units = "in", dpi = 300),
                     types = "png")
}

  sim$cPoolsClean <- cPoolsClean

  # 4. add sw/hw flag
  colsToUseForestType <- c("sw_hw", "gcids")
  forestType <- unique(sim$gcMeta[, ..colsToUseForestType])

  #       # cbmTables$forest_type
  #       # id           name
  #       # 1  1       Softwood
  #       # 2  2      Mixedwood
  #       # 3  3       Hardwood
  #       # 4  9 Not Applicable

  setkeyv(forestType, "gcids")
  cPoolsClean <- merge(cPoolsClean, forestType, by = "gcids",
                                     all.x = TRUE, all.y = FALSE)

  # 5. finalize sim$growth_increments table
  outCols <- c("id", "ecozone", "totMerch", "fol", "other")
  cPoolsClean[, (outCols) := NULL]
  keepCols <- c("gcids", "age", "merch_inc", "foliage_inc", "other_inc", "sw_hw")
  incCols <- c("merch_inc", "foliage_inc", "other_inc")
  setnames(cPoolsClean,names(cPoolsClean),
           keepCols)
  increments <- cPoolsClean[, (incCols) := list(
    merch_inc, foliage_inc, other_inc
  )]
  setorderv(increments, c("gcids", "age"))

  # Assertions
  if (isTRUE(P(sim)$doAssertions)) {
    # All should have same min age
    if (length(unique(increments[, min(age), by = "sw_hw"]$V1)) != 1)
      stop("All ages should start at the same age for each curveID")
    if (length(unique(increments[, max(age), by = "sw_hw"]$V1)) != 1)
      stop("All ages should end at the same age for each curveID")
  }

  ## replace increments that are NA with 0s
  increments[is.na(increments), ] <- 0
  sim$growth_increments <- increments

  # END process growth curves -------------------------------------------------------------------------------
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # Growth and yield
  if (!suppliedElsewhere("curveID", sim)) {
    sim$curveID <- "curveID"
  }

  if (!suppliedElsewhere("userGcMeta", sim)) {
    if (!suppliedElsewhere("userGcMetaURL", sim)) {
      sim$userGcMetaURL <- extractURL("userGcMeta")
    }

    sim$userGcMeta <- prepInputs(url = sim$userGcMetaURL,
                                 targetFile = "gcMetaEg.csv",
                                 destinationPath = inputPath(sim),
                                 fun = fread,
                                 purge = 7
    )
    data.table::setnames(sim$userGcMeta, "gcids", "curveID")
    data.table::setkey(sim$userGcMeta, curveID)

    sim$userGcMeta[, sw_hw := data.table::fifelse(forest_type_id == 1, "sw", "hw")]
  }

  if (!suppliedElsewhere("userGcM3", sim)){

    if (suppliedElsewhere("userGcM3URL", sim)){

      sim$userGcM3 <- prepInputs(url = sim$userGcM3URL,
                                 destinationPath = inputPath(sim),
                                 fun = "data.table::fread")

    }else{

      message("User has not supplied growth curves ('userGcM3' or 'userGcM3URL'). ",
              "Defaults for Saskatchewan will be used.")

      sim$userGcM3 <- prepInputs(url = extractURL("userGcM3"),
                                 destinationPath = inputPath(sim),
                                 targetFile = "userGcM3.csv",
                                 fun = "data.table::fread")
      data.table::setnames(sim$userGcM3, names(sim$userGcM3), c("curveID", "Age", "MerchVolume"))
      data.table::setkeyv(sim$userGcM3, c("curveID", "Age"))

    }
  }

  # cbmAdmin: this is needed to match species and parameters. Boudewyn et al 2007
  # abbreviation and cbm spatial units and ecoBoudnary id is provided with the
  # adminName to avoid confusion.
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    if (!suppliedElsewhere("cbmAdminURL", sim)) {
      sim$cbmAdminURL <- extractURL("cbmAdmin")
    }
    sim$cbmAdmin <- prepInputs(url = sim$cbmAdminURL,
                               targetFile = "cbmAdmin.csv",
                               destinationPath = inputPath(sim),
                               fun = fread)
  }

  ## tables from Boudewyn -- all downloaded from the NFIS site.
  ## however, NFIS changes the tables and seems to forget parameter columns at times.
  if (!suppliedElsewhere("table3", sim)) {
    if (!suppliedElsewhere("table3URL", sim)) {
      sim$table3URL <- extractURL("table3")
    }
    sim$table3 <- prepInputs(url = sim$table3URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }

  if (!suppliedElsewhere("table4", sim)) {
    if (!suppliedElsewhere("table4URL", sim)) {
      sim$table4URL <- extractURL("table4")
    }
    sim$table4 <- prepInputs(url = sim$table4URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }


  if (!suppliedElsewhere("table5", sim)) {
    if (!suppliedElsewhere("table5URL", sim)) {
      sim$table5URL <- extractURL("table5")
    }
    sim$table5 <- prepInputs(url = sim$table5URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }


  if (!suppliedElsewhere("table6", sim)) {
    if (!suppliedElsewhere("table6URL", sim)) {
      sim$table6URL <- extractURL("table6")
    }
    sim$table6 <- prepInputs(url = sim$table6URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }

  if (!suppliedElsewhere("table7", sim)) {
    if (!suppliedElsewhere("table7URL", sim)) {
      sim$table7URL <- extractURL("table7")
    }
    sim$table7 <- prepInputs(url = sim$table7URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
  }


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
