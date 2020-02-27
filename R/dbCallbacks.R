#' Insert data into database
#'
#' \code{insertCallback} takes shiny input values supplied by the user, collects
#' them into a \code{data.frame} and then appends them into a database
#'
#' @inheritParams addEdit
#'
#' @export
#'
insertCallback <-
  function(inputData, db, dbTable, session = shiny::getDefaultReactiveDomain()) {
    input <- session$input
    ids <- inputData$ids
  # Creates data.frame of field values for new entry
  new <- lapply(ids,
                function(x) {
                  if (class(input[[x]]) == "Date") {
                    if (length(input[[x]]) == 0) {
                      NA
                    }
                    else {
                      as.character(input[[x]])
                    }
                  }
                  else if (is.null(input[[x]]) ||
                           length(input[[x]]) == 0 ||
                           input[[x]] == "") {
                    NA
                  }
                  else {
                    trimws(input[[x]])
                  }
                })
  new <- stats::setNames(new, ids)
  new <- as.data.frame(new)

  # inserts new entry into database
  DBI::dbWriteTable(db, dbTable, new, append = TRUE)
}



#' Update row in database
#'
#' \code{insertCallback} takes shiny input values supplied by the user, collects
#' them into a \code{data.frame} and then appends them into a database
#'
#' @inheritParams addEdit
#'
#' @export
#'
updateCallback <- function(inputData, db, dbTable, reactiveData, dtRow,
                           session = shiny::getDefaultReactiveDomain()) {
  input <- session$input
  ids <- inputData$ids
  idVar <- inputData[1, 1]
  # Creates data.frame of updated field values
  new <- lapply(ids,
                function(x) {
                  if (x == idVar) {
                    dtRow()
                  }
                  else if (class(input[[x]]) == "Date") {
                    if (length(input[[x]]) == 0) {
                      NA
                    }
                    else {
                      as.character(input[[x]])
                    }
                  }
                  else if (is.null(input[[x]]) || length(input[[x]]) == 0 || input[[x]] == "") {
                    NA
                  }
                  else {
                    input[[x]]
                  }
                })
  # Remove '.' from variable names
  ids2 <- gsub("\\.", "", ids)
  new <- stats::setNames(new, ids2)
  new <- as.data.frame(new, stringsAsFactors = FALSE)

  # creates update statement with named matching for values
  upStatement <-
    paste0(
      "update ",
      dbTable,
      " set ",
      paste0("'", ids[!ids == idVar], "'= $", ids2[!ids2 == idVar], collapse = ", "),
      " where ",
      idVar,
      "= $",
      idVar
    )

  up <- DBI::dbSendQuery(db, upStatement)
  # fills in upStatement with values from new data.frame
  DBI::dbBind(up, new)
  DBI::dbClearResult(up)
}
