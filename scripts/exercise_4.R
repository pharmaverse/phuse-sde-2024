library(random.cdisc.data)
library(teal)
library(teal.reporter)
library(teal.transform)
library(teal.widgets)
library(tern)
library(dplyr)

modules <- list(
  module(
    label = "Adhoc module",
    server = function(id, data, reporter, filter_panel_api){
      moduleServer(id, function(input, output, session){
        s_summary <- function(x) {
          if (is.numeric(x)) {
            in_rows(
              "n" = rcell(sum(!is.na(x)), format = "xx"),
              "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)), format = "xx.xx (xx.xx)"),
              "IQR" = rcell(IQR(x, na.rm = TRUE), format = "xx.xx"),
              "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
            )
          } else if (is.factor(x)) {
            vs <- as.list(table(x))
            do.call(in_rows, lapply(vs, rcell, format = "xx"))
          } else {
            stop("type not supported")
          }
        }

        observe({
          ADSL <- get_var(data(), "ADSL")
          req(ADSL)
          updateSelectInput(
            inputId = "param",
            choices = variable_choices(ADSL),
            selected = c("AGE"),
          )
        })

        table_q <- reactive({
          data() |>
            within(
              {
                s_summary <- my_summary
                summary_lyt <- basic_table() %>%
                  split_cols_by(var = "ARM") %>%
                  analyze(param, afun = s_summary)
                summary_tbl <- build_table(summary_lyt, ADSL)
                summary_tbl
              },
              my_summary = s_summary,
              param = input$param
            )
        })

        output$table = renderUI({
          renderPrint(table_q()[["summary_tbl"]])
        })

        # ----------
        # reproducibility
        # ----------
        observeEvent(input$src, {
          showModal(
            ui = modalDialog(
              title = "Reproducible R code",
              tags$pre(
                get_code(table_q())
              )
            ),
            session = session
          )
        })

        # ----------
        # reporter
        # ----------
        simple_reporter_srv(
          "simple_reporter",
          reporter = reporter,
          card_fun = function(card = TealReportCard$new(), comment) {
            card$set_name("Patient demographics")
            card$append_text(toString(table_q()[["summary_tbl"]]), "verbatim")
            card$append_fs(filter_panel_api$get_filter_state())
            card$append_encodings(list(param = input$param))
            if (!comment == "") {
              card$append_text("Comment", "header3")
              card$append_text(comment)
            }
            card$append_src(get_code(table_q()))
            card
          }
        )

      })
    },
    ui = function(id) {
      ns <- NS(id)

      standard_layout(
        output = div(
          fluidRow(column(
            width = 12,
            br(), hr(),
            uiOutput(ns("table"))
          ))
        ),
        encoding = div(
          simple_reporter_ui(ns("simple_reporter")),
          br(),
          tags$label('Encodings', class = 'text-primary'),
          helpText('Analysis Data:', tags$code('ADSL')),
          selectInput(
            inputId = ns('param'),
            label = 'Demographic Parameter',
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          hr(),
          actionButton(
            inputId = ns("src"),
            label = "Show R code",
            width = "100%"
          )
        )
      )
    },
    datanames = c("ADSL")
  )
)


data <- teal_data()
data <- within(data, {
  ADSL <- radsl(cached = TRUE)
})
datanames(data) <- c("ADSL")

app <- init(
  data = data,
  modules = modules
)

if (Sys.getenv("QUARTO_ROOT") == "") {
  shinyApp(app$ui, app$server)
}
