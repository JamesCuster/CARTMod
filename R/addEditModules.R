#' Add database entry button: UI function
#'
#' This function and \code{\link{addModule}} are used in conjunction to add the
#' UI and server elements necessary to add a row to a given table in a database
#'
#' @param id character name for the namespace of the module
#'
#' @return A shiny \code{\link[shiny]{actionButton}} which opens a modal to
#'   allow user to input data to be added to a database
#'
#' @seealso \code{\link{addModule}}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     addModuleUI("data")
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
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
#' @param input,output,session These parameters are handled by
#'   \code{\link[shiny]{callModule}} and can be ignored.
#' @param modalTitle Character string for title to be displayed at the top of
#'   the modal.
#' @param inputData a \code{data.frame} containing columns \code{ids, labels,
#'   type} which correspond to the field names of the table in the database,
#'   lables for the input in the shiny UI, and the type of shiny input to be
#'   used.
#' @param db a \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI]{dbConnect}}. In other words, the object the
#'   database connection is saved to.
#' @param dbTable The database table the new data will be added to.
#'
#' @return Shiny \code{\link[shiny]{observeEvent}}'s which control actions when
#'   the add button is pressed, as well as the save button in the modal.
#'
#' @seealso \code{\link{addModuleUI}}
#'
#' @export
addModule <- function(input, output, session, modalTitle, inputData, db, dbTable, reactiveData) {
  # controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    # Checks inputData for select input types, if present, gathers the choices
    if (any(grepl("select", inputData$type))) {
      choices <- choicesReactive(inputData, reactiveData)
    }

    # Creates modal
    shiny::showModal(
      shiny::modalDialog(
        title = modalTitle,
        modalInputs(
          session = session,
          inputData = inputData,
          choices = choices
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



#' Create list of shiny inputs for modal
#'
#' Takes a \code{data.frame} containing information about shiny inputs and
#' additional optional parameters to create list of shiny inputs to be displayed
#' in a modal
#'
#' @param session The \code{session} object passed to function given to
#'   shinyServer. This should usually be \code{session = session}.
#' @inheritParams addModule
#' @param values Optional argument to be used when the inputs are being
#'   populated from an observation in the database. (NEED MORE DOCUMENTATION
#'   HERE ONCE THE EDIT FUNCTIONALITY IS BUILT OUT)
#' @param choices Optional argument to provide the choices for
#'   \code{\link[shiny]{selectInput}} and
#'   \code{\link[shiny:selectInput]{selectizeInput}} inputs.
#'
#' @export
modalInputs <- function(session, inputData, values, choices) {
  fields <-
    apply(
      inputData, 1,
      function(x, values) {
        value <- ifelse(missing(values) || is.na(values[x["ids"]]),
                        "", values[x["ids"]])
        if (x["type"] == "skip") {
          NULL
        }
        else if (x["type"] == "textInput") {
          shiny::textInput(inputId  = session$ns(x["ids"]),
                    label = x["labels"],
                    value = value,
                    width = 400)
        }
        else if (x["type"] == "selectizeInput") {
          shiny::selectizeInput(inputId  = session$ns(x["ids"]),
                         label = x["labels"],
                         choices = c("", choices[[x["ids"]]]),
                         selected = value,
                         width = 400)
        }
        else if (x["type"] == "selectInput") {
          shiny::selectInput(inputId  = session$ns(x["ids"]),
                      label = x["labels"],
                      choices = c("", choices[[x["ids"]]]),
                      width = 400)
        }
        else if (x["type"] == "textAreaInput") {
          shiny::textAreaInput(inputId  = session$ns(x["ids"]),
                        label = x["labels"],
                        value = value,
                        width = "400px",
                        height = "102px")
        }
        else if (x["type"] == "dateInput") {
          shiny::dateInput(inputId  = session$ns(x["ids"]),
                    label = x["labels"],
                    value = value,
                    width = 400)
        }
        else if (x["type"] == "actionButton") {
          shiny::actionButton(inputId  = session$ns(x["ids"]),
                       label = x["labels"],
                       style = "margin-left: 20px;
                                margin-top: 24px;
                                height: 34px;")
        }
      }
    )
  fields
}

