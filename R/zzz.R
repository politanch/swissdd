# package startup-message (data source displayed)

.onAttach <- function(lib, pkg)
{
  packageStartupMessage('**************************************************************')
  packageStartupMessage('*                          swissdd 0.0.9                     *')
  packageStartupMessage('*                     provided by politan.ch                 *')
  packageStartupMessage('*                                                            *')
  packageStartupMessage('*                                                            *')
  packageStartupMessage('*           Data source: Federal Statistical Office          *')
  packageStartupMessage('*                   https://www.bfs.admin.ch/                *')
  packageStartupMessage('*                                                            *')
  packageStartupMessage('**************************************************************')
}

# suppress notes

utils::globalVariables(c("id", "resultat",
                         "res","geoid","langKey",
                         "yes","kantonname",
                         "results2","geoLevelnummer",
                         "geoname","district_id",
                         "geoLevelname","text","ktid"))



