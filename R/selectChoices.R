#' Create reactive for select/selectize input choices
#'
#' @inheritParams addModule
#'
#' @export
choicesReactive <- function(inputData, reactiveData, staticChoices = NULL) {
  choicesReact <- shiny::reactive({
    choices <-
      lapply(
        inputData$ids,
        function(x) {
          if (grepl("select", inputData[inputData$ids == x, "type"])) {
            if (inputData[inputData$ids == x, "choicesTable"] == "static") {
              staticChoices[[x]]
            }
            else {
              valueLabel(
                df = reactiveData[[inputData[inputData$ids == x, "choicesTable"]]],
                value = inputData[inputData$ids == x, "choicesValues"],
                label = inputData[inputData$ids == x, "choicesLabels"])
            }
          } else {
            return(NA)
          }
        }
      )
    choices <- stats::setNames(choices, inputData$ids)
    return(choices)
  })
  return(choicesReact)
}

#' Define label/value pairs for select/selectize inputs
#'
#' @param df A \code{data.frame} where the choices for the select/selectize
#'   input are. (note: \code{data.frame} used in this argument should live in
#'   the reactiveData object passed from the
#'   \code{\link[CARTMod]{choicesReactive}} function)
#' @param value The column in \code{df} where the numeric ID value for a choice
#'   is stored.
#' @param label The column in \code{df} where the charater label/name for a
#'   choice is stored.
#'
#' @export
valueLabel <- function(df, value, label) {
  x <- stats::setNames(
    as.character(df[[value]]),
    df[[label]]
  )
  x <- x[sort(names(x))]
  return(x)
}
