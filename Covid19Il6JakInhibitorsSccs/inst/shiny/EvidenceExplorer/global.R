if (!exists("shinySettings")) {
  if (file.exists("data")) {
    shinySettings <- list(dataFolder = "data", blind = TRUE)
  } else {
    shinySettings <- list(dataFolder = "S:/Covid19Il6JakInhibitorsSccs/shinyDataFolder", blind = TRUE)
  }
}
dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
load(file.path(dataFolder, "shinyData.RData"))

tos <- unique(sccsResult[, c("exposureId", "outcomeId")])
tos <- tos[tos$outcomeId %in% outcomeOfInterest$outcomeId, ]

