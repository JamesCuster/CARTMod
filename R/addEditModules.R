#' Add database entry button: UI function
#'
#' This function and \code{\link{addModule}} are used in conjunction to add the UI and
#' server elements necessary to add a row to a given table in a database
#'
#' @param id character name for the namespace of the module
#'
#' @return A shiny \code{\link[shiny]{actionButton}} which opens a modal to allow user to
#'   input data to be added to a database
#'
#' @seealso \code{\link{addModule}}
#'
#' @examples
#' library(shiny)
#' ui <- fluidPage(
#'   addModuleUI("data")
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
addModuleUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    shiny::actionButton(inputId = ns("add"), label = "Add")
  )
}



#' Add database entry button: server function
#'
#' This function and \link{addModuleUI} are used in conjection to add the UI and
#' server elements necessary to add a row to a given table in a database
#'
#' @param input,output,session These parameters are handled by \code{\link[shiny]{callModule}}
#'   and can be ignored.
#' @param modalTitle Character string for title to be displayed at the top of
#'   the modal.
#' @param inputData a \code{data.frame} containing columns \code{ids, labels,
#'   type} which correspond to the field names of the table in the database, lables to display for the input
#'   in the shiny UI, and the type of shiny input to be used.
#' @param db a \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as returned by
#'   \code{\link[DBI]{dbConnect}}. In other words, the object the database connection
#'   is saved to.
#' @param dbTable The database table the new data will be added to.
#'
#' @return Shiny \code{\link[shiny]{observeEvent}}'s which control actions when the
#' add button is pressed, as well as the save button in the modal.
#'
#' @seealso \code{\link{addModuleUI}}
#'
#' @export
addModule <- function(input, output, session, modalTitle, inputData, db, dbTable) {
  # controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    shiny::showModal(
      shiny::modalDialog(
        title = modalTitle,
        modalInputs(
          inputData$ids,
          inputData$labels,
          inputData$type,
          session = session
        ),
        footer =
          list(
            shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("insert"), "Save")
          )
      )
    )
  })

  # Controls what happens when Save is pressed
  shiny::observeEvent(input$insert, {
    insertCallback(input, output, session, inputData$ids, db, dbTable)
    shiny::removeModal()
  })
}



#' Hello
#'
#' @export
modalInputs <- function(ids, labels, type, values, df, choices, session) {
  fields <- list()
  for (i in seq_along(ids)) {
    if (type[i] == "skip") {
      fields[[ids[i]]] <- NULL
    }
    else if (type[i] == "textInput") {
      value <- ifelse(missing(values) || is.na(values[ids[i]]), "", values[ids[i]])
      fields[[ids[i]]] <- textInput(inputId = session$ns(ids[i]),
                                    label = labels[i],
                                    value = value,
                                    width = 400)
    }
    else if (type[i] == "selectizeInput") {
      value <- ifelse(missing(values) || is.na(values[ids[i]]), "", values[ids[i]])
      fields[[ids[i]]] <- selectizeInput(inputId = session$ns(ids[i]),
                                         label = labels[i],
                                         choices = c("", choices[[ids[[i]]]]),
                                         selected = value,
                                         width = 400)
    }
    else if (type[i] == "selectInput") {
      value <- ifelse(missing(values) || is.na(values[ids[i]]), "", values[ids[i]])
      fields[[ids[i]]] <- selectInput(inputId = session$ns(ids[i]),
                                      label = labels[i],
                                      choices = c("", choices[[ids[[i]]]]),
                                      width = 400)
    }
    else if (type[i] == "textAreaInput") {
      value <- ifelse(missing(values) || is.na(values[ids[i]]), "", values[ids[i]])
      fields[[ids[i]]] <- textAreaInput(inputId = session$ns(ids[i]),
                                        label = labels[i],
                                        value = value,
                                        width = "400px",
                                        height = "102px")
    }
    else if (type[i] == "dateInput") {
      value <- ifelse(missing(values) || is.na(values[ids[i]]), "", values[ids[i]])
      fields[[ids[i]]] <- dateInput(inputId = session$ns(ids[i]),
                                    label = labels[i],
                                    value = value,
                                    width = 400)
    }
    else if (type[i] == "actionButton") {
      fields[[ids[i]]] <- actionButton(inputId = session$ns(ids[i]),
                                       label = labels[i],
                                       style = "margin-left: 20px; margin-top: 24px; height: 34px;")
    }
  }
  fields
}
