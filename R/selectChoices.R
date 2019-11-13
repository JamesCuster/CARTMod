#' Create reactive for select/selectize input choices
#'
#' @inheritParams addModule
#'
#' @export
choicesReactive <- function(inputData, reactiveData) {
  choicesReact <- shiny::reactive({
    choices <-
      apply(
        inputData, 1,
        function(x) {
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
#' @param df a data.frame
#' @param value value labels
#' @param label labels
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
