# package startup-message (data source displayed)

.onAttach <- function(lib, pkg)
{
  packageStartupMessage('**************************************************************')
  packageStartupMessage('*                          swissdd 1.0.1                     *')
  packageStartupMessage('*                     provided by politan.ch                 *')
  packageStartupMessage('*                                                            *')
  packageStartupMessage('*                                                            *')
  packageStartupMessage('*           Data source: Federal Statistical Office          *')
  packageStartupMessage('*                   https://www.bfs.admin.ch/                *')
  packageStartupMessage('*                                                            *')
  packageStartupMessage('**************************************************************')
}

# suppress notes

utils::globalVariables(c("id", "resultat","res","geoid",
                         "canton_id","canton_name",
                         "district_id","district_name",
                         "mun_id","mun_name",
                         "langKey",
                         "yes",
                         # "kantonname",
                         "results2",
                         "geoLevelnummer",
                         # "geoname",
                         "jaStimmenInProzent",
                         "cor","cov","correlation",
                         # "geoLevelname",
                         "text"#,"ktid"
                         ))



