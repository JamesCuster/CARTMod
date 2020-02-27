#' Add database entry button: UI function
#'
#' This function and \code{\link{addEdit}} are used in conjunction to add the
#' UI and server elements necessary to add a row to a given table in a database
#'
#' @param id character name for the namespace of the module
#'
#' @return A shiny \code{\link[shiny]{actionButton}} which opens a modal to
#'   allow user to input data to be added to a database
#'
#' @seealso \code{\link{addEdit}}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     addEditUI("data")
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
addEditUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    shinyjs::useShinyjs(),
    shiny::actionButton(inputId = ns("add"), label = "Add"),
    shiny::actionButton(inputId = ns("edit"), label = "Edit")
  )
}



#' Add database entry button: server function
#'
#' This function and \link{addEditUI} are used in conjection to add the UI and
#' server elements necessary to add a row to a given table in a database
#'
#' @param input,output,session These parameters are handled by
#'   \code{\link[shiny]{callModule}} and can be ignored.
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
#' @param addTitle Title to display at the top of the add Modal
#' @param editTitle Title to display at the top of the edit Modal
#' @param dtRow Reactive which stores the row selected in the datatable
#'
#' @return Shiny \code{\link[shiny]{observeEvent}}'s which control actions when
#'   the add button is pressed, as well as the save button in the modal.
#'
#' @seealso \code{\link{addEditUI}}
#'
#' @export
addEdit <- function(input, output, session, addTitle, editTitle, modalUI, inputData,
                    reactiveData, staticChoices = NULL, checkDuplicate = NULL,
                    db, dbTable, dtRow) {
  # define the choices and values reactive
  choices <- choicesReactive(inputData, reactiveData, staticChoices)
  values <- shiny::reactive({
    reactiveData[[dbTable]][reactiveData[[dbTable]][, 1] == dtRow(), ]
  })

  # Currently modulUI is a function provided by user that creates the UI for the
  # modal. I will want to modify this so that it can be a function, or a list of
  # UI components. I don't know if this will actually be possible since it may
  # cause some namespace naming issues.

  # call addModal
  shiny::callModule(
    addModal,
    id = "addModal",
    inputData,
    reactiveData,
    checkDuplicate,
    db,
    dbTable,
    modalUI,
    staticChoices,
    choices = choices
  )

  # Controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    addModalUI(session$ns("addModal"), addTitle = addTitle)
  })



  # Call editModal
  # enable/disable edit button if datatable row is selected
  shiny::observe({
    shinyjs::toggleState("edit", condition = !(is.null(dtRow()) || dtRow() == ""))
  })

  # call modalModule
  shiny::callModule(
    editModal,
    id = "editModal",
    inputData,
    reactiveData,
    checkDuplicate,
    db,
    dbTable,
    modalUI,
    staticChoices,
    dtRow = dtRow,
    choices = choices,
    values = values
  )

  shiny::observeEvent(input$edit, {
    editModalUI(session$ns("editModal"), editTitle = editTitle)
  })
}


#' Create Add Modal: UI function
#'
#' This function and \code{\link{addModal}} are used in conjunction to create
#' the UI and server elements necessary to control the modal
#'
#' @param id character name for the namespace of the module
#' @inheritParams addEdit
#'
#' @export
addModalUI <- function(id, addTitle) {
  ns <- shiny::NS(id)

  # Generate and display modal
  shiny::showModal(
    shiny::modalDialog(
      title = addTitle,
      shiny::uiOutput(ns("modalUI")),
      footer =
        list(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("insert"), "Save")
        )
    )
  )
}


#' Create Add Modal: server function
#'
#' This function and \code{\link{addModalUI}} are used in conjunction to
#' create the UI and server elements necessary to control the modal
#'
#' @inheritParams addEdit
#' @param ... Additional parameters to mass to modalUI function
#'
#' @export
addModal <- function(input, output, session, inputData, reactiveData,
                        checkDuplicate, db, dbTable, modalUI, staticChoices, ...) {
  # Get select(ize) choices and build modalUI
  # choices <- choicesReactive(inputData, reactiveData, staticChoices)
  output$modalUI <- shiny::renderUI(callModalUI(modalUI, ...))

  # Controls what happens when Save is pressed
  shiny::observeEvent(input$insert, {
    # If checkDuplicates is not null, then check database for duplicates
    if (!is.null(checkDuplicate)) {
      duplicateFound <-
        checkDuplicateFunction(checkDuplicate, reactiveData, inputData, db,
                               dbTable)
    }
    if (is.null(checkDuplicate) || !duplicateFound) {
      insertCallback(inputData, db, dbTable)
      shiny::removeModal()
    }
  })

  # Observers to control duplicate modal action buttons
  shiny::observeEvent(input$continueAdd, {
    insertCallback(inputData, db, dbTable)
    shiny::removeModal()
  })
}



#' Create Edit Modal: UI function
#'
#' This function and \code{\link{editModal}} are used in conjunction to create
#' the UI and server elements necessary to control the modal
#'
#' @param id character name for the namespace of the module
#' @inheritParams addEdit
#'
#' @export
editModalUI <- function(id, editTitle) {
  ns <- shiny::NS(id)

  # Generate and display modal
  shiny::showModal(
    shiny::modalDialog(
      title = editTitle,
      shiny::uiOutput(ns("modalUI")),
      footer =
        list(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("update"), "Update")
        )
    )
  )
}



#' Create Add Modal: server function
#'
#' This function and \code{\link{addModalUI}} are used in conjunction to
#' create the UI and server elements necessary to control the modal
#'
#' @inheritParams addEdit
#' @param ... Additional parameters to mass to modalUI function
#'
#' @export
editModal <- function(input, output, session, inputData, reactiveData,
                      checkDuplicate, db, dbTable, modalUI, staticChoices, dtRow, ...) {
  # Get select(ize) choices and input values of selected row then build modalUI
  # choices <- choicesReactive(inputData, reactiveData, staticChoices)
  # values <- shiny::reactive({
  #   selectedRow <- dtRow()
  #   reactiveData[[dbTable]][selectedRow, ]
  # })
  output$modalUI <- shiny::renderUI(callModalUI(modalUI, ...))

  # Controls what happens when Update is pressed
  shiny::observeEvent(input$update, {
    updateCallback(inputData, db, dbTable, reactiveData, dtRow)
    shiny::removeModal()
  })
}



#' Call modalUI function
#'
#' This function wraps around the the modalUI argument to call the function
#' withing the modal server functions
#'
#' @inheritParams addEdit
#' @param ... Additional parameters to mass to modalUI function
#'
#'list(
#'   selectizeInput(ns("Species"), "Species", choices = reactiveData$flowers$flowerName),
#'   selectizeInput(ns("smell"), "Smell", choices = irisStaticChoices$smell)
#')
callModalUI <- function(modalUI, ..., session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  if (!is.function(modalUI)) {
    stop("modalUI argument must be a function.")
  }
  if (!("ns" %in% methods::formalArgs(modalUI))) {
    stop("ns must be an argument for the modalUI function.")
  }
  modalUI(ns = ns, ...)
}




#' Check for duplicated entries before addition
#'
#' This function takes a character vector of fields to check in the database for
#' a possible duplication before an addition occurs
#'
#' @inheritParams addEdit
#'
#' @export
checkDuplicateFunction <-
  function(checkDuplicate, reactiveData, inputData, db, dbTable,
           session = shiny::getDefaultReactiveDomain()) {

  input <- session$input
  output <- session$output
  # Check for duplicates in the columns provided in checkDuplicate
  possibleDuplicate <- lapply(checkDuplicate, function(x) {
    value <- tolower(input[[x]])
    fieldValues <- tolower(reactiveData[[dbTable]][[x]])
    if (value %in% fieldValues) {
      reactiveData[[dbTable]][which(value == fieldValues), ]
    }
  })
  # Gather possible duplicates and remove any rows that were grabbed twice
  possibleDuplicate <- do.call(rbind, possibleDuplicate)
  possibleDuplicate <- possibleDuplicate[!duplicated(possibleDuplicate), ]


  # If duplicates are found, display modal and return TRUE, if not return FALSE
  if (!is.null(possibleDuplicate)) {
    # create datatable of duplicates
    output$duplicate <- DT::renderDataTable(
      DT::datatable(possibleDuplicate, options = list(dom = "t")),
      server = TRUE
    )

    # display duplicate datatable in modal
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        title = "Possible Duplicate Entry",
        shiny::tags$h5("Is this the entry you are trying to input?"),
        DT::dataTableOutput(session$ns("duplicate")),
        shiny::tags$h5("If yes, the entry already exists in the database.
                        Please cancel addition."),
        shiny::tags$h5("If no, proceed with addition."),
        footer =
          shiny::div(
            # shiny::actionButton(session$ns("cancelAdd"), "Cancel"),
            shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("continueAdd"), "Continue")
          )
      )
    )
    duplicatesFound <- TRUE
  }
  else {
    duplicatesFound <- FALSE
  }
  return(duplicatesFound)
}

#' Create list of shiny inputs for modal
#'
#' Takes a \code{data.frame} containing information about shiny inputs and
#' additional optional parameters to create list of shiny inputs to be displayed
#' in a modal
#'
#' @inheritParams addEdit
#' @param ns namespace function passed from calling environment
#' @param values Optional argument to be used when the inputs are being
#'   populated from an observation in the database. (NEED MORE DOCUMENTATION
#'   HERE ONCE THE EDIT FUNCTIONALITY IS BUILT OUT)
#' @param choices Optional argument to provide the choices for
#'   \code{\link[shiny]{selectInput}} and
#'   \code{\link[shiny:selectInput]{selectizeInput}} inputs.
#'
#' @export
modalInputs <- function(ns = ns, inputData, values, choices,
                        session = shiny::getDefaultReactiveDomain()) {
  inputData <- inputData[inputData$type != "skip", ]
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
          shiny::textInput(inputId  = ns(x["ids"]),
                           label = x["labels"],
                           value = value,
                           width = 400)
        }
        else if (x["type"] == "selectizeInput") {
          shiny::selectizeInput(inputId  = ns(x["ids"]),
                                label = x["labels"],
                                choices = c("", choices[[x["ids"]]]),
                                selected = value,
                                width = 400)
        }
        else if (x["type"] == "selectInput") {
          shiny::selectInput(inputId  = ns(x["ids"]),
                             label = x["labels"],
                             choices = c("", choices[[x["ids"]]]),
                             width = 400)
        }
        else if (x["type"] == "textAreaInput") {
          shiny::textAreaInput(inputId  = ns(x["ids"]),
                               label = x["labels"],
                               value = value,
                               width = "400px",
                               height = "102px")
        }
        else if (x["type"] == "dateInput") {
          value <- if (value == "") as.Date(NA) else value
          shiny::dateInput(inputId  = ns(x["ids"]),
                           label = x["labels"],
                           value = value,
                           width = 400)
        }
        else if (x["type"] == "actionButton") {
          shiny::actionButton(inputId  = ns(x["ids"]),
                              label = x["labels"],
                              style = "margin-left: 20px;
                                margin-top: 24px;
                                height: 34px;")
        }
      }
    )
  fields
}
