# package startup-message (data source displayed)

#' Title
#'
#' @param lib 
#' @param pkg 
#' @importFrom utils packageVersion
#'
#' @examples

.onAttach <- function(lib, pkg)
{
  packageStartupMessage('************************************************************')
  packageStartupMessage(paste('*                     swissdd',utils::packageVersion("swissdd"),'                       *'))
  packageStartupMessage('*                  developed by politan.ch                 *')
  packageStartupMessage('*                                                          *')
  packageStartupMessage('*                     Data sources:                        *')
  packageStartupMessage('*                Federal Statistical Office                *')
  packageStartupMessage('*                 https://www.bfs.admin.ch/                *')
  packageStartupMessage('*                                                          *')
  packageStartupMessage('*                      Swissvotes                          *')
  packageStartupMessage('*                  https://swissvotes.ch/                  *')
  packageStartupMessage('*                                                          *')
  packageStartupMessage('************************************************************')
}

# suppress notes

utils::globalVariables(c("id", "resultat","res","geoid",
                         "kantId",
                         "canton_id","canton_name",
                         "district_id","district_name",
                         "mun_id","mun_name",
                         "langKey",
                         "yes",
                         "anr",
                         "name",
                         # "kantonname",
                         "results2",
                         "geoLevelnummer",
                         "geoLevelname",
                         "ktdata_zh",
                         "jaStimmenInProzent",
                         "cor","cov","correlation", 
                         "text",
                         "measure", "vogeId", "geometry",
                         "bezkId", "stimmbeteiligungInProzent",
                         "text",#,"ktid"
                         "packageVersion"
                         ))



