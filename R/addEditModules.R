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
#' @param db a \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI]{dbConnect}}. In other words, the object the
#'   database connection is saved to.
#' @param dbTable The database table the new data will be added to.
#' @param reactiveData Reactive which stores all of the tables from the database
#'   as seperate \code{data.frames}
#' @param checkDuplicate Character vector of columns names (corresponds to input
#'   ids) that should be checked in the database for duplication. Default value
#'   is \code{NULL} meaning no variables are checked for duplication
#'
#' @return Shiny \code{\link[shiny]{observeEvent}}'s which control actions when
#'   the add button is pressed, as well as the save button in the modal.
#'
#' @seealso \code{\link{addModuleUI}}
#'
#' @export
addModule <- function(input, output, session,
                      modalTitle, inputData, db, dbTable, reactiveData,
                      checkDuplicate = NULL) {
  # controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    # Checks inputData for select input types, if present, gathers the choices
    if (any(grepl("select", inputData$type))) {
      choices <- choicesReactive(inputData, reactiveData)
    }

    # Creates modal for inputs
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
    # If checkDuplicates is not null, then check database for duplicates
    if (!is.null(checkDuplicate)) {
      possibleDuplicates <-
        checkDuplicateFunction(input, output, session, checkDuplicate,
                               dbTable, reactiveData)
      # IF duplicates are found create datatable to display them
      if (!is.null(possibleDuplicates)) {
        # create datatable of duplicates
        output$duplicate <- DT::renderDataTable(
          DT::datatable(possibleDuplicates, options = list(dom = "t")),
          server = TRUE
        )
        # display duplicate datatable in modal
        shiny::showModal(
          shiny::modalDialog(
            title = "Possible Duplicate Entry",
            shiny::tags$h5("Is this the entry you are trying to input?"),
            DT::dataTableOutput(session$ns("duplicate")),
            shiny::tags$h5("If yes, the entry already exists in the database.
                           Please cancel addition."),
            shiny::tags$h5("If no, proceed with addition."),
            footer =
              shiny::div(
                shiny::actionButton(session$ns("continueAdd"), "Continue"),
                shiny::actionButton(session$ns("cancelAdd"), "Cancel")
              )
          )
        )
      }
      # If no duplicates are found then proceed with addition
      else {
        insertCallback(input, output, session, inputData$ids, db, dbTable)
        shiny::removeModal()
      }
    }
  })

  # Observers to control duplicate modal action buttons
  shiny::observeEvent(input$cancelAdd, {
    shiny::removeModal()
  })

  shiny::observeEvent(input$continueAdd, {
    insertCallback(input, output, session, inputData$ids, db, dbTable)
    shiny::removeModal()
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
  do.call(rbind, possibleDuplicate)
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
