defineModule(sim, list(
  name = "CBM_vol2biomass",
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
  version = list(CBM_vol2biomass = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "CBM_vol2biomass.Rmd")),
  reqdPkgs = list(
    "PredictiveEcology/CBMutils@development (>=2.5.2)",
    "ggforce", "ggplot2", "ggpubr", "mgcv", "quickPlot", "robustbase", "data.table", "patchwork"
  ),
  parameters = rbind(
    defineParameter("smooth",    "logical", TRUE, NA, NA, "Smooth curves with the Chapman Richards equation"),
    defineParameter(".plotPath", "character", NA, NA, NA, "Path to directory for output figures"),
    defineParameter(".plot",     "logical", TRUE, NA, NA, "Plot input and translated curves"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "userGcLocations", objectClass = "data.frame",
      desc = "Growth curve locations with columns 'admin_abbrev', 'eco_id', and one or more columns in 'userGcMeta'"),
    expectsInput(
      objectName = "userGcMeta", objectClass = "data.frame",
      desc = "Growth curve metadata table with key 'curveID' and other curve identifiers"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volume table with columns 'curveID', 'Age', and 'MerchVolume'"),
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
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata"),
    createsOutput(
      objectName = "gcIncrements", objectClass = "data.table",
      desc = "Growth curve carbon increments in MgC/ha/year"),
    createsOutput(
      objectName = "cPoolsClean", objectClass = "data.table",
      desc = "Tonnes of carbon/ha both cumulative and increments,
      for each growth curve id (in this data.table id and gcids are
      the same), by age and ecozone")
  )
))

doEvent.CBM_vol2biomass <- function(sim, eventTime, eventType) {
  switch(
    eventType,

    init = {

      sim <- scheduleEvent(sim, start(sim), "CBM_vol2biomass", "vol2biomass", eventPriority = 5)
    },

    vol2biomass = {

      sim <- ReadInputs(sim)
      sim <- Vol2Biomass(sim)
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

ReadInputs <- function(sim) {

  # Check input
  if ("gcids" %in% names(sim$userGcMeta)) stop("'userGcMeta' cannot contain \"gcids\"")
  if ("gcids" %in% names(sim$userGcM3))   stop("'userGcM3' cannot contain \"gcids\"")

  reqCols <- list(
    userGcLocations = c("admin_abbrev", "eco_id"),
    userGcMeta      = c("curveID", "species"),
    userGcM3        = c("curveID", "Age", "MerchVolume")
  )

  for (tableName in names(reqCols)){
    if (!data.table::is.data.table(sim[[tableName]])){
      sim[[tableName]] <- data.table::as.data.table(sim[[tableName]])
    }
  }

  if (!all(reqCols$userGcLocations %in% names(sim$userGcLocations))) stop(
    "userGcLocations must have columns: ", paste(shQuote(reqCols$userGcLocations), collapse = ", "))
  if (!all(reqCols$gcMeta %in% names(sim$gcMeta))) stop(
    "gcMeta must have columns: ", paste(shQuote(reqCols$gcMeta), collapse = ", "))
  if (!all(reqCols$userGcM3 %in% names(sim$userGcM3))) stop(
    "userGcM3 must have columns: ", paste(shQuote(reqCols$userGcM3), collapse = ", "))

  if (!all(sim$userGcMeta$curveID %in% sim$userGcM3$curveID)) {
    stop("There is a missmatch in the 'curveID' columns of userGcMeta and the userGcM3")
  }

  # Initiate gcMeta
  adminID <- c("admin_abbrev", "eco_id")
  curveID <- setdiff(names(sim$userGcLocations), c("admin_name", adminID))
  if (length(curveID) == 0) stop("userGcLocations requires one or more userGcMeta columns")

  sim$gcMeta <- unique(sim$userGcLocations)
  sim$gcMeta$gcids <- factor(CBMutils::gcidsCreate(sim$gcMeta[, .SD, .SDcols = c(adminID, curveID)]))

  sim$gcMeta <- merge(sim$gcMeta, sim$userGcMeta, by = curveID, all.x = TRUE)
  data.table::setkey(sim$gcMeta, gcids)
  data.table::setcolorder(sim$gcMeta, c("gcids", "admin_name", adminID, curveID), skip_absent = TRUE)

  if (any(is.na(sim$gcMeta$curveID))){
    gcMissing <- sim$gcMeta[is.na(curveID), .SD, .SDcols = names(sim$userGcLocations)]
    stop("userGcLocations do not match a curve in userGcMeta:\n", paste(
      sapply(1:nrow(gcMissing), function(i) paste(
        sapply(names(gcMissing), function(c) paste0(c, ": ", gcMissing[i,][[c]])),
        collapse = "; ")),
      collapse = "\n"))
  }

  ## Check that all required columns are available, and if not, add them:
  if (any(!c("canfi_species", "sw_hw", "genus") %in% names(sim$gcMeta))){

    sppMatchTable <- CBMutils::sppMatch(
      sim$gcMeta$species, return = c("CanfiCode", "Genus", "Broadleaf"))[, .(
        canfi_species = CanfiCode,
        sw_hw         = data.table::fifelse(Broadleaf, "hw", "sw"),
        genus         = Genus
      )]

    sim$gcMeta <- cbind(
      sim$gcMeta[, .SD, .SDcols = setdiff(names(sim$gcMeta), names(sppMatchTable))],
      sppMatchTable)
    rm(sppMatchTable)
  }

  return(invisible(sim))

}

Vol2Biomass <- function(sim){

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
  if (P(sim)$.plot){

    volCurves <- ggplot(data = sim$userGcM3, aes(x = Age, y = MerchVolume, group = curveID, colour = factor(curveID))) +
      geom_line() + labs(colour = "curveID") + theme_bw()

    SpaDES.core::Plots(volCurves,
                       filename = "volCurves",
                       path = P(sim)$.plotPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")

    message("User: please review plots of input curves: ", P(sim)$.plotPath)
  }


  # START reducing Biomass model parameter tables --------------------------------------------

  ## TEMPORARY: Current Boudewyn tables label Yukon as YT instead of YK. This hard coded fix ensures they are properly labeled as YK
  sim$table3$juris_id[sim$table3$juris_id == "YT"] <- "YK"
  sim$table4$juris_id[sim$table4$juris_id == "YT"] <- "YK"
  sim$table6$juris_id[sim$table6$juris_id == "YT"] <- "YK"
  sim$table7$juris_id[sim$table7$juris_id == "YT"] <- "YK"

  # subsetting Boudewyn tables to the ecozones/admin boundaries of the study area.
  # Some ecozones/boundaries are not in these tables, in these cases, the function replaces them in
  # thisAdmin to the closest equivalent present in the Boudewyn tables.
  thisAdmin <- unique(sim$gcMeta[, .(juris_id = admin_abbrev, ecozone = eco_id)])
  stable3 <- boudewynSubsetTables(sim$table3, thisAdmin)
  stable4 <- boudewynSubsetTables(sim$table4, thisAdmin)
  stable5 <- boudewynSubsetTables(sim$table5, thisAdmin)
  stable6 <- boudewynSubsetTables(sim$table6, thisAdmin)
  stable7 <- boudewynSubsetTables(sim$table7, thisAdmin)


  # START processing curves -------------------------------------------

  # Process curves from m3/ha to tonnes of C/ha then to annual increments
  # per above ground biomass pools

  gcM3 <- merge(sim$gcMeta, sim$userGcM3, by = "curveID", allow.cartesian = TRUE)[
    , .(gcids, Age, MerchVolume)]
  data.table::setkey(gcM3, gcids, Age)

  # 1. Calculate the translation (result is cPools or "cumulative AGcarbon pools")

  # Matching is 1st on species, then on gcids which gives us location (admin,
  # spatial unit and ecozone)
  fullSpecies <- unique(sim$gcMeta$species)

  gcMeta <- data.table::copy(sim$gcMeta)
  data.table::setnames(gcMeta, "admin_abbrev", "juris_id")
  data.table::setnames(gcMeta, "eco_id", "ecozones")

  ## Temporary: this is required by cumPoolsCreate
  if (!"spatial_unit_id" %in% names(gcMeta)){
    gcMeta <- merge(
      gcMeta,  data.table::as.data.table(sim$cbmAdmin)[
        , .(juris_id = abreviation, ecozones = EcoBoundaryID, spatial_unit_id = SpatialUnitID)],
      by = c("juris_id", "ecozones"))
    data.table::setkey(gcMeta, gcids)
    data.table::setcolorder(gcMeta)
  }

  cPools <- cumPoolsCreate(
    fullSpecies, gcMeta, gcM3,
    thisAdmin = data.table::as.data.table(sim$cbmAdmin),
    stable3, stable4, stable5, stable6, stable7
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

  # 3. Smooth curves
  if (P(sim)$smooth){

    cPoolsClean <- cumPoolsSmooth(cPoolsRaw) |> Cache()

  }else{

    cPoolsClean <- cPoolsRaw
    cPoolsClean[, totMerch_New := totMerch]
    cPoolsClean[, fol_New      := fol]
    cPoolsClean[, other_New    := other]
  }

  #Note: this will produce a warning if one of the curve smoothing efforts doesn't converge
  if (P(sim)$.plot){

    cPoolsSmoothPlot <- m3ToBiomPlots(inc = cPoolsClean,
                                      title = "Cumulative merch/fol/other by gcid")

    for (i in seq_along(cPoolsSmoothPlot)){
      SpaDES.core::Plots(cPoolsSmoothPlot[[i]],
                         filename = paste0("cPools_smoothed_postChapmanRichards_", i, ".png"),
                         path = P(sim)$.plotPath,
                         ggsaveArgs = list(width = 10, height = 5, units = "in", dpi = 300),
                         types = "png")
    }

    message(crayon::red(
      "User: please review plots of raw and smoothed translations of growth curves: ",
      P(sim)$.plotPath))
  }

  ## keeping the new curves - at this point they are still cumulative
  colNames <- c("totMerch", "fol", "other")
  set(cPoolsClean, NULL, colNames, NULL)
  colNamesNew <- grep("totMerch|fol|other", colnames(cPoolsClean), value = TRUE)
  setnames(cPoolsClean, old = colNamesNew, new = colNames)

  # 4. Calculating Increments
  incCols <- c("incMerch", "incFol", "incOther")
  cPoolsClean <- copy(cPoolsClean) #TEMP FIX: making a copy of this table deals with the shallow table warning that happens in the line below.
  ## This warning likely originates from how the table is dealt with in cumPoolsSmooth or cumPoolsCreate.
  cPoolsClean[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
                by = eval("gcids")]

  sim$cPoolsClean <- cPoolsClean

  if (P(sim)$.plot){

    colsToUse33 <- c("age", "gcids", incCols)
    rawIncPlots <- m3ToBiomPlots(inc = sim$cPoolsClean[, ..colsToUse33],
                                 title = "Increments")
    for (i in seq_along(rawIncPlots)){
      SpaDES.core::Plots(rawIncPlots[[i]],
                         filename = paste0("increments_", i, ".png"),
                         path = P(sim)$.plotPath,
                         ggsaveArgs = list(width = 10, height = 5, units = "in", dpi = 300),
                         types = "png")
    }

    message(crayon::red("User: please review plots of carbon increments: ", P(sim)$.plotPath))
  }

  # 4. finalize increments table
  increments <- cPoolsClean[, .(
    gcids, age,
    merch_inc   = incMerch,
    foliage_inc = incFol,
    other_inc   = incOther
  )]
  data.table::setkey(increments, gcids, age)

  ## replace increments that are NA with 0s
  increments[is.na(merch_inc),   merch_inc   := 0]
  increments[is.na(foliage_inc), foliage_inc := 0]
  increments[is.na(other_inc),   other_inc   := 0]

  sim$gcIncrements <- increments

  # Assertions
  if (isTRUE(P(sim)$doAssertions)) {
    # All should have same min age
    if (length(unique(increments[, min(age), by = "gcids"]$V1)) != 1)
      stop("All ages should start at the same age for each curveID")
    if (length(unique(increments[, max(age), by = "gcids"]$V1)) != 1)
      stop("All ages should end at the same age for each curveID")
  }

  # END process growth curves -------------------------------------------------------------------------------
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # Set output plot path
  if (is.na(P(sim)$.plotPath)) P(sim)$.plotPath <- file.path(outputPath(sim), "CBM_vol2biomass_figures")

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
