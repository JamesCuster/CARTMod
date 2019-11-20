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
  ids = c("irisID", names(iris)),
  labels = c("irisID", gsub("\\.", " ", names(iris))),
  type = c("skip",
           "textInput",
           "textInput",
           "textInput",
           "textInput",
           "selectizeInput"),
  choicesTable = c(NA, NA, NA, NA, NA, "flowers"),
  choicesValues = c(NA, NA, NA, NA, NA, "flowerID"),
  choicesLabels = c(NA, NA, NA, NA, NA, "flowerName"),
  stringsAsFactors = FALSE
)

irisFilters <- data.frame(
  ids = c("speciesFilter"),
  labels = c("Species"),
  type = c("selectizeInput"),
  choicesTable = c("flowers"),
  choicesValues = c("flowerID"),
  choicesLabels = c("flowerName"),
  stringsAsFactors = FALSE
)


# Flowers -----------------------------------------------------------------
flowerInputs <- data.frame(
  ids = c("flowerID", "flowerName"),
  labels = c("flowerID", "Flower Name"),
  type = c("skip", "textInput"),
  stringsAsFactors = FALSE
)
