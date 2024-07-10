###########################################################################
# Project           : Back-predicteR
# Program name      : 
# Developed in      : R version 3.2.4 Revised (2016-03-16 r70336)
# Purpose           : Perform back-calculation of response to result with help of calibration data
# Inputs            : from interface
# Outputs           : report.pdf
# Revision History  :
#   Version   Date            Author                     Revision 
#   -------   ---------       ----------                 -------------------------
#   1.0       15-JUL-2016     Thomas de Marchin          Creation
#
# Reference number  : No                                        
#
# Validation Level  : No
#
# Declaration of Confidentiality (choose the applicable sentence):
#   - I, as an author, certify that this program is free from any client or 
#     client project information whatever the type of information and that 
#     this program can be used without risk of confidentiality issue.
#
###########################################################################


ui <- function(request) { 
  
  dashboardPage(
    
    title = title,
    
    header = dashboardHeader(
      title = dashboardBrand(
        title = HTML(title), 
        color = "primary",
        href = "https://www.pharmalex.com/pharmalex-services/custom-software-development/",
        image = "logo_app2.png"
      ),
      downloadButton("get_excel_file", "Download results")
    ),
    
    sidebar = dashboardSidebar(
      skin = "light",
      sidebarMenu(
        id = "tabs",
        menuItem("Introduction", tabName = "tabIntro", icon = icon("book-open")),
        menuItem("Data", tabName = "tabData", icon = icon("file-medical")),
        menuItem("Analysis", tabName = "tabAnalysis", icon = icon("calculator")),
        menuItem("About/Tool description", tabName = "tabAbout", icon = icon("question"))
      ),
      div(class = "logo", tags$a(
        href="https://www.pharmalex.com", 
        target = "_blank",
        tags$img(src = "logoPharmalex.png", 
                 height = "35px")
      ))
    ),
    
    body = dashboardBody(
      useShinyjs(),
      
      tagList(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          includeHTML("www/google-analytics.html")
        )),
      
      use_theme(PLXtheme),   
      
      tabItems(
        
        # intro tab
        tabItem(tabName = "tabIntro", tabIntroUI("tabIntro")),
        
        # data tab
        tabItem(tabName = "tabData", 
                column(12,
                       box(
                         width = NULL,
                         title = "Load data",
                         fileInput('file_excel', "Upload a file", accept=c('.xlsx')),
                         checkboxInput("test_data", tags$span(style="font-size: 12px;", "or load test data")),
                         h6("Download a", a("template", href="Backpredicter_template.xlsx"), "or an", a("example.", href="Backpredicter_example_M1_to_M5.xlsx"))
                       ),
                       box(
                         width = NULL,
                         title = "Review your data",
                         id = "boxViewData",
                                          wellPanel(
                                            fluidRow(
                                              column(width=6, align="center", h5('Calibration data:'), 
                                                     verbatimTextOutput("view_calibration_data"), 
                                                     actionButton("show_all_calibration_data", "View all calibration data")),
                                              column(width=6, align="center", h5('Data to back predict:'), 
                                                     verbatimTextOutput("view_validation_data"), 
                                                     actionButton("show_all_validation_data", "View all validation data"))
                                              ))
                       ),
                       box(
                         width = NULL,
                         title = "Specify the units for plotting",
                         fluidRow(column(width=6, align="center", textInput("xunit", "X unit", value = "X unit")),
                                  column(width=6, align="center", textInput("yunit", "Y unit", value = "Y unit")))
                       )
                )),   
        
        # analysis tab
        tabItem(tabName = "tabAnalysis", 
                
                box(
                  width = NULL,
                  title = "Select your model(s)",
                  checkboxGroupInput("selected_models", NULL, choice_models, selected=NULL, inline=FALSE, width=NULL)
                ),
                
                box(
                  width = NULL,
                  title = "Results",
                  column(12,
                         align = "center",
                         conditionalPanel("input.selected_models.indexOf('1')!=-1",
                                          br(),
                                          h4(strong('Linear regression:'), align="center"),
                                          plotlyOutput("plot_model_R1", height="600"),
                                          actionButton("show_results_R1", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('2')!=-1",
                                          br(),
                                          h4(strong('Weighted (1/X) linear regression:'), align="center"),
                                          plotlyOutput("plot_model_R2", height="600"),
                                          actionButton("show_results_R2", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('3')!=-1",
                                          br(),
                                          h4(strong('Weighted (1/X^2) linear regression:'), align="center"),
                                          plotlyOutput("plot_model_R3", height="600"),
                                          actionButton("show_results_R3", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('4')!=-1",
                                          br(),
                                          h4(strong('Linear regression after (base 10) LOGARITHM transformation of both concentration and response:'), align="center"),
                                          plotlyOutput("plot_model_R4", height="600"),
                                          actionButton("show_results_R4", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('5')!=-1",
                                          br(),
                                          h4(strong('Linear regression after SQUARE ROOT transformation of both concentration and response:'), align="center"),
                                          plotlyOutput("plot_model_R5", height="500"),
                                          actionButton("show_results_R5", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('6')!=-1",
                                          br(),
                                          h4(strong('Quadratic regression:'), align="center"),
                                          plotlyOutput("plot_model_R6", height="500"),
                                          actionButton("show_results_R6", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('7')!=-1",
                                          br(),
                                          h4(strong('Weighted (1/X) quadratic regression:'), align="center"),
                                          plotlyOutput("plot_model_R7", height="500"),
                                          actionButton("show_results_R7", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('8')!=-1",
                                          br(),
                                          h4(strong('Weighted (1/X^2) quadratic regression:'), align="center"),
                                          plotlyOutput("plot_model_R8", height="500"),
                                          actionButton("show_results_R8", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('9')!=-1",
                                          br(),
                                          h4(strong('Four parameters logistic Regression:'), align="center"),
                                          plotlyOutput("plot_model_R9", height="500"),
                                          actionButton("show_results_R9", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('10')!=-1",
                                          br(),
                                          h4(strong('Weighted (POM) Four parameters logistic Regression:'), align="center"),
                                          plotlyOutput("plot_model_R10", height="500"),
                                          actionButton("show_results_R10", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('11')!=-1",
                                          br(),
                                          h4(strong('Five parameters logistic Regression:'), align="center"),
                                          plotlyOutput("plot_model_R11", height="500"),
                                          actionButton("show_results_R11", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('12')!=-1",
                                          br(),
                                          h4(strong('Weighted (POM) Five parameters logistic Regression:'), align="center"),
                                          plotlyOutput("plot_model_R12", height="500"),
                                          actionButton("show_results_R12", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('13')!=-1",
                                          br(),
                                          h4(strong('Power Regression:'), align="center"),
                                          plotlyOutput("plot_model_R13", height="500"),
                                          actionButton("show_results_R13", "View back predictions")),
                         
                         conditionalPanel("input.selected_models.indexOf('14')!=-1",
                                          br(),
                                          h4(strong('Weighted (POM) Power Regression:'), align="center"),
                                          plotlyOutput("plot_model_R14", height="500"),
                                          actionButton("show_results_R14", "View back predictions")))
                )),
        
        # system info tab
        tabItem(tabName = "tabAbout", tabAboutUI("tabAbout"))
      )
    ),
    
    footer = dashboardFooter(
      left = a("pharmalex.com", href = "https://www.pharmalex.com", target = "_blank"),
      right = paste0("©", format(Sys.Date(), "%Y"), " PHARMALEX GMBH. ALL RIGHTS RESERVED.")
    )
    
  )
}
