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
