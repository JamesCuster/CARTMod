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
#' @param modalUI Function that creates the modal UI.
#' @param inputData a \code{data.frame} containing columns \code{ids, labels,
#'   type, choicesTable, choicesValues, choicesLabels} which correspond to
#'   \describe{
#'     \item{\code{ids}}{The field names in the database. These ids will also be
#'     used as the inputId for shiny inputs.}
#'
#'     \item{\code{labels}}{These are character values which will be used as the
#'     label for shiny inputs and be displayed above the input in the UI.}
#'
#'     \item{\code{type}}{The type of shiny input to be used.}
#'
#'     \item{\code{choicesTable, choicesValues, choicesLabels}}{Only used if the
#'     desired input has predefined choices
#'     (\code{\link[shiny:selectInput]{selectInput/selectizeInput})}. If neither
#'     of these inputs are used, then the values should be \code{NA}. If one of
#'     these inputs is used then:
#'       \describe{
#'         \item{\code{choicesTable}}{Is the table in the database where the
#'         choices for the given input are.}
#'         \item{\code{choicesValues}}{Is the column in the table which stores
#'         the numeric identifier for the choice}
#'         \item{\code{choicesLabels}}{Is the column in the table which stores
#'         the character name for the choices}
#'       }
#'     }
#'   }
#' @param reactiveData Reactive which stores all of the tables from the database
#'   as seperate \code{data.frames}
#' @param staticChoices \code{list} where choices for static selectize
#'   inputs are defined.
#' @param checkDuplicate character vector specifing which fields to check for
#'   possible duplication. Default value is null, and no fields are checked for
#'   duplicates
#' @param db a \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI]{dbConnect}}. In other words, the object the
#'   database connection is saved to.
#' @param dbTable The database table the new data will be added to.
#' @param reactiveData Reactive which stores all of the tables from the database
#'   as seperate \code{data.frames}
#' @param checkDuplicate Character vector of columns names (corresponds to input
#'   ids) that should be checked in the database for duplication. Default value
#'   is \code{NULL} meaning no variables are checked for duplication
#' @param additionalInputs Additional shiny inputs to be dislayed in the
#'   inputModal provided as a list. Note: inputs used here will not be appened
#'   with the module name prefix as the other inputs are
#' @param staticChoices List object which contains the choices for
#'   \code{\link[shiny:selectInput]{selectInput/selectizeInput})} which are not
#'   dependent on information in the database
#'
#' @return Shiny \code{\link[shiny]{observeEvent}}'s which control actions when
#'   the add button is pressed, as well as the save button in the modal.
#'
#' @seealso \code{\link{addModuleUI}}
#'
#' @export
addModule <- function(input, output, session, modalTitle, modalUI, inputData,
                      reactiveData, staticChoices = NULL, checkDuplicate = NULL,
                      db, dbTable) {
  # Currently modulUI is a function provided by user that creates the UI for the
  # modal. I will want to modify this so that it can be a function, or a list of
  # UI components. I don't know if this will actually be possible since it may
  # cause some namespace naming issues.

  # call modalModule
  shiny::callModule(
    CARTMod::modalModule,
    id = "modal",
    inputData,
    reactiveData,
    checkDuplicate,
    db,
    dbTable,
    modalUI,
    staticChoices
  )

  # Controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    CARTMod::modalModuleUI(session$ns("modal"), modalTitle = modalTitle)
  })
}


#' Function to check if entry is duplicate
#'
#' This function checks the column names provided in \code{checkDuplicate} and
#' checks the table provided in the \code{dbTable} argument if the new entry is
#' a possible duplicate of a row that already exist in the database
#'
#' @inheritParams addModule
#'
#' @export
checkDuplicateFunction <- function(input, output, session,
                                   checkDuplicate, dbTable, reactiveData) {
  possibleDuplicate <- lapply(checkDuplicate, function(x) {
    value <- tolower(input[[x]])
    fieldValues <- tolower(reactiveData[[dbTable]][[x]])
    if (value %in% fieldValues) {
      reactiveData[[dbTable]][which(value == fieldValues), ]
    }
  })
  possibleDuplicate <- do.call(rbind, possibleDuplicate)
  # remove any rows that were grabbed twice
  possibleDuplicate[!duplicated(possibleDuplicate), ]
}


#' Create Modal: UI function
#'
#' This function and \code{\link{modalModule}} are used in conjunction to create
#' the UI and server elements necessary to control the modal
#'
#' @param id character name for the namespace of the module
#' @inheritParams addModule
#'
#' @export
modalModuleUI <- function(id, modalTitle) {
  ns <- shiny::NS(id)

  # Generate and display modal
  shiny::showModal(
    shiny::modalDialog(
      title = modalTitle,
      shiny::uiOutput(ns("modalUI")),
      footer =
        list(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("insert"), "Save")
        )
    )
  )
}


#' Create Modal: server function
#'
#' This function and \code{\link{modalModuleUI}} are used in conjunction to create
#' the UI and server elements necessary to control the modal
#'
#' @inheritParams addModule
#'
#' @export
modalModule <- function(input, output, session, inputData, reactiveData, checkDuplicate, db, dbTable, modalUI, staticChoices) {
  # Get select(ize) choices and build modalUI
  choices <- choicesReactive(inputData, reactiveData, staticChoices)
  output$modalUI <- shiny::renderUI(modalUI(session$ns, choices = choices))

  # Controls what happens when Save is pressed
  shiny::observeEvent(input$insert, {
    # If checkDuplicates is not null, then check database for duplicates
    if (!is.null(checkDuplicate)) {
      duplicateFound <- checkDuplicateFunction(input, output, session, checkDuplicate, reactiveData, inputData, db, dbTable)
    }
    if (is.null(checkDuplicate) || !duplicateFound) {
      insertCallback(input, output, session, inputData$ids, db, dbTable)
      shiny::removeModal()
    }
  })

  # Observers to control duplicate modal action buttons
  shiny::observeEvent(input$continueAdd, {
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
          value <- if (value == "") as.Date(NA) else value
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
