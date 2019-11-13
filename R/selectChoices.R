#' Create reactive for select/selectize input choices
#'
#' @inheritParams addModule
#'
#' @export
choicesReactive <- function(inputData, reactiveData) {
  browser()
  choicesReact <- shiny::reactive({
    choices <-
      apply(
        inputData, 1,
        function(x) {
          browser()
          if (grepl("select", x["type"])) {
            valueLabel(
              df = reactiveData[[x["choicesTable"]]],
              value = x["choicesValues"],
              label = x["choicesLabels"])
          } else {
            return(NA)
          }
        }
      )
    choices <- stats::setNames(choices, inputData$ids)
    return(choices)
  })
  return(choicesReact())
}


#' Define label/value pairs for select/selectize inputs
#'
#' @param df A \code{data.frame} where the choices for the select/selectize
#'   input are.
#' @param value The column in \code{df} where the numeric ID value for a choice
#'   is stored.
#' @param label The column in the \code{df} where the charater label/name for a
#'   choice is stored.
#'
#' @export
valueLabel <- function(df, value, label) {
  browser()
  x <- stats::setNames(
    as.character(df[[value]]),
    df[[label]]
  )
  x <- x[sort(names(x))]
  return(x)
}
