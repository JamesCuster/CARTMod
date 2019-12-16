library(shiny)
library(RSQLite)
library(dplyr)
library(DT)
library(CARTMod)

# Connect to database
dbPath <- system.file("shinyExamples", "irisApp", "iris.sqlite",
                      package = "CARTMod")
irisdb <- dbConnect(dbDriver("SQLite"), dbPath)

# Create reactive to store db tables
reactiveData <- reactiveValues()


# Function that loads db tables and stores in irisReactive
loadDatabase <- function(db, tables = c("iris", "flowers", "modified")) {
  if ("iris" %in% tables) {
    reactiveData$iris <- tbl(db, "iris") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  if ("flowers" %in% tables) {
    reactiveData$flowers <- tbl(db, "flowers") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  if ("modified" %in% tables) {
    modified <<- tbl(db, "modified") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
}

loadDatabase(irisdb)


# iris --------------------------------------------------------------------
irisInputs <- data.frame(
  ids = c("irisID", names(iris), "smell"),
  labels = c("irisID", gsub("\\.", " ", names(iris)), "Smell"),
  type = c("skip",
           "textInput",
           "textInput",
           "textInput",
           "textInput",
           "selectizeInput",
           "selectizeInput"),
  choicesTable = c(NA, NA, NA, NA, NA, "flowers", "static"),
  choicesValues = c(NA, NA, NA, NA, NA, "flowerID", NA),
  choicesLabels = c(NA, NA, NA, NA, NA, "flowerName", NA),
  stringsAsFactors = FALSE
)

irisStaticChoices <- list(
  smell = c("Poor", "Fair", "Good"),
  smellFilter = c("Poor", "Fair", "Good")
)


# irisFilters <- data.frame(
#   ids = c("speciesFilter", "smellFilter"),
#   labels = c("Species", "Smell"),
#   type = c("selectizeInput", "selectInput"),
#   choicesTable = c("flowers", "static"),
#   choicesValues = c("flowerID", NA),
#   choicesLabels = c("flowerName", NA),
#   stringsAsFactors = FALSE
# )


# Flowers -----------------------------------------------------------------
flowerInputs <- data.frame(
  ids = c("flowerID", "flowerName", "flowerName2"),
  labels = c("flowerID", "Flower Name", "Flower Name 2"),
  type = c("skip", "textInput", "textInput"),
  stringsAsFactors = FALSE
)
