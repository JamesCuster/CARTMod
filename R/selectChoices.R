#' Create reactive for select/selectize input choices
#'
#' @export
choicesReactive <- function(inputData) {
  choicesReact <- reactive({
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
    choices <- setNames(choices, inputData$ids)
    return(choices)
  })
  return(choicesReact())
}


#' Define label/value pairs for select/selectize inputs
#'
#' @export
valueLabel <- function(df, value, label) {
  x <- setNames(
    as.character(df[[value]]),
    df[[label]]
  )
  x <- x[sort(names(x))]
  return(x)
}
