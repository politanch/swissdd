# available_votedates <- function(list){
#
#
# urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
#
# grepl("[0-9]{2}",urls$result$resources$download_url)
#
#
# sub("\\D{8}","",urls$result$resources$download_url)
#
# }
#
#
#
#
# gsub("[0-9]{2,}","\\1",urls$result$resources$download_url)
#
#
# substring(urls$result$resources$download_url, regexpr("[0-9]{8}", urls$result$resources$download_url))
#
# gsub("[^0-9]{4,}", "", urls$result$resources$download_url)
#
#
# sub(".* ", "", x)
