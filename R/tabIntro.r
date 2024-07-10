tabIntroUI <- function(id) {
  ns <- NS(id)
  h2("Back-PredicteR")
  box(
    width = NULL,
    title = "Introduction",
    helpText("Back-PredicteR is an application which helps you to back-predict signals (response) to concentrations (results) when the relation between the response and the result is known.", br(), 
    "This is particularly useful in laboratories when determining the concentration of unknown samples through assays such as absorbance or fluorescence measurements.", br(),
    "With Back-PredicteR, you can easily establish a calibration curve by measuring the response of your assay for samples of known concentration (calibration data) and fitting a regression model.", br(), 
    "Once the calibration curve is established, you can use it to back-predict the concentration of unknown samples based on their signal."),
    tags$ol(
      tags$li("Load data or load a test data sample"), 
      tags$li("Choose your model(s)"), 
      tags$li("Visualize your results"),
      tags$li("Download your results")
    ),
    br(),
    fluidRow(column(12, align = "center", img(src = 'diagram.png', width = '600'))),
    h6("Contact:"),
    a("Thomas de Marchin", href = "mailto:thomas.demarchin@pharmalex.com")
  )
}