#' Perceived causes of building collapse in Nigeria
#'
#' A worked example dataset for the RII package: a simulated survey of 150
#' construction professionals across seven Nigerian states, rating ten
#' commonly cited causes of building collapse on a 5-point Likert scale
#' (1 = Not significant, 5 = Extremely significant). Rating patterns are
#' loosely modelled on findings typically reported in Nigerian
#' building-collapse studies, so the ranking this produces (poor
#' supervision, substandard materials and poor workmanship at the top;
#' overloading and lack of maintenance culture lower down) is realistic,
#' but the data itself is simulated, not collected from real respondents.
#'
#' @format A data frame with 150 rows and 14 variables:
#' \describe{
#'   \item{RespondentID}{Integer respondent identifier.}
#'   \item{Profession}{Factor: Architect, Civil Engineer, Builder or
#'     Quantity Surveyor.}
#'   \item{State}{Factor: the Nigerian state the respondent practises in
#'     (Lagos, Oyo, Ekiti, Kwara, FCT-Abuja, Rivers or Edo).}
#'   \item{YearsExperience}{Integer years of professional experience.}
#'   \item{SubstandardMaterials}{Use of substandard building materials
#'     (1-5).}
#'   \item{PoorSupervision}{Poor supervision by professionals on site
#'     (1-5).}
#'   \item{InadequateSoilInvestigation}{Inadequate soil/geotechnical
#'     investigation before construction (1-5).}
#'   \item{CorruptionApprovalProcess}{Corruption/bribery in the building
#'     plan approval process (1-5).}
#'   \item{NonComplianceBuildingCode}{Non-compliance with the national
#'     building code (1-5).}
#'   \item{UnqualifiedArtisans}{Use of unqualified/untrained artisans
#'     (1-5).}
#'   \item{StructuralDesignErrors}{Errors in structural design (1-5).}
#'   \item{Overloading}{Overloading of the building beyond its design
#'     capacity (1-5).}
#'   \item{PoorWorkmanship}{General poor workmanship (1-5).}
#'   \item{LackOfMaintenance}{Lack of a building maintenance culture
#'     (1-5).}
#' }
#'
#' @source Simulated by SoftData Consult for teaching purposes, with
#'   rating tendencies informed by the general pattern of findings in the
#'   Nigerian building-collapse literature.
#'
#' @examples
#' data(building_collapse_nigeria)
#' tbl <- rii_table(building_collapse_nigeria[, 5:14])
#' tbl
"building_collapse_nigeria"

#' Barriers to climate-smart agriculture adoption among Nigerian farmers
#'
#' A second worked example dataset, deliberately from outside construction:
#' a simulated survey of 175 smallholder farmers across five Nigerian
#' states, rating ten barriers to adopting climate-smart agriculture (CSA)
#' practices on a **4-point** scale (1 = Not a barrier, 4 = Very severe
#' barrier) — included to show that RII, and this package, work the same
#' way for any Likert-scale ranking task and any scale range, not just a
#' 5-point construction-industry survey. Rating patterns are loosely
#' modelled on the general pattern of findings in the Nigerian CSA-adoption
#' literature (access to credit, extension services and input costs tend to
#' rate as the most severe barriers), but the data itself is simulated, not
#' collected from real respondents.
#'
#' @format A data frame with 175 rows and 15 variables:
#' \describe{
#'   \item{FarmerID}{Integer respondent identifier.}
#'   \item{State}{Factor: Ekiti, Kwara, Oyo, Kogi or Niger.}
#'   \item{Gender}{Factor: Male or Female.}
#'   \item{FarmSizeHa}{Numeric farm size in hectares.}
#'   \item{YearsFarming}{Integer years of farming experience.}
#'   \item{LackOfAccessToCredit}{Lack of access to agricultural credit/
#'     loans (1-4).}
#'   \item{LimitedExtensionServices}{Limited access to agricultural
#'     extension services (1-4).}
#'   \item{HighCostOfImprovedInputs}{High cost of improved seeds/inputs
#'     (1-4).}
#'   \item{LackOfTechnicalKnowledge}{Lack of technical knowledge of CSA
#'     practices (1-4).}
#'   \item{LandTenureInsecurity}{Insecure land tenure (1-4).}
#'   \item{UnpredictableWeatherPatterns}{Unpredictable/changing weather
#'     patterns (1-4).}
#'   \item{LackOfMarketAccess}{Poor access to output markets (1-4).}
#'   \item{InadequateIrrigationInfra}{Inadequate irrigation infrastructure
#'     (1-4).}
#'   \item{LowLiteracyLevels}{Low literacy levels among farmers (1-4).}
#'   \item{LackOfGovernmentSupport}{Insufficient government support/
#'     policy (1-4).}
#' }
#'
#' @source Simulated by SoftData Consult for teaching purposes, with
#'   rating tendencies informed by the general pattern of findings in the
#'   Nigerian climate-smart-agriculture adoption literature.
#'
#' @examples
#' data(csa_adoption_barriers_nigeria)
#' tbl <- rii_table(csa_adoption_barriers_nigeria[, 6:15], scale_max = 4)
#' tbl
"csa_adoption_barriers_nigeria"

#' RII: Relative Importance Index for Likert-Scale Survey Data
#'
#' The RII package computes the Relative Importance Index (RII), along
#' with means, standard deviations and ranks, from Likert-scale survey
#' data — the calculation environmental, construction and social science
#' researchers usually do by hand in a spreadsheet.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{rii_table}}}{Ranked RII table from raw
#'     respondent-level data (the usual case).}
#'   \item{\code{\link{rii_table_freq}}}{Ranked RII table from a
#'     ready-made frequency table (counts per scale point per item).}
#'   \item{\code{\link{rii_raw}}, \code{\link{rii_freq}}}{Single-item
#'     building blocks used by the table functions above.}
#'   \item{\code{\link{classify_rii}}}{Classify RII values into the
#'     five importance bands used in the literature.}
#'   \item{\code{plot.rii_table}}{Ranked horizontal bar chart, via
#'     \code{plot(tbl)}.}
#' }
#'
#' @section Example data:
#' \code{\link{building_collapse_nigeria}} — perceived causes of building
#' collapse in Nigeria, rated by 150 construction professionals.
#'
#' @keywords internal
"_PACKAGE"
