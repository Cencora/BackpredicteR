tabAboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(12,
           box(
             width = NULL,
             title="About",
             a(h6("Need more? pharmalex.com"), href="https://www.pharmalex.com", target="_blank"),
             a(h6("Support"), href="mailto:stat-plxstat@pharmalex.com"),
             h6(paste0("Release: ", app_version, " (", last_update, ")")),
             h6("Introduction HPLC diagram by DataBase Center for Life Science (DBCLS), distributed under a CC BY 4.0 license.")
           ),
           box(
             width = NULL,
             collapsed = TRUE,
             title = "System Information",
             verbatimTextOutput(ns("system"))
           )
           
    )
  )
}

tabAboutServer <- function(id, sharedData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$system <- renderPrint({
        sessionInfo()
      })
      
    }
  )
}