library(random.cdisc.data)
library(teal)

data <- teal_data()
data <- within(data, {
  ADSL <- radsl(cached = TRUE)
})
# set datanames
datanames <- c("ADSL")
datanames(data) <- datanames

## Reusable Configuration For Modules
ADSL <- data[["ADSL"]]

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("Excercise 1", style = "font-size: 30px;"),
)

footer <- tags$p(
  "This teal app is brought to you by PhUSE SDE Basel."
)

app <- init(
  data = data,
  modules = modules(
    example_module()
  ),
  title = build_app_title("Basic Teal Demo App"),
  header = header,
  footer = footer
)

if (Sys.getenv("QUARTO_ROOT") == "") {
  shinyApp(app$ui, app$server)
}
