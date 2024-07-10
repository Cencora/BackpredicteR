###########################################################################
# Project           : Back-predicteR
# Program name      : 
# Developed in      : R version 4.2.2
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


shinyServer(function(session, input, output) {
  
  # tabAbout
  tabAboutServer("tabAbout", sharedData)
  
  #DEBUG
  #output$aa <- renderPrint({input$file_excel$datapath})
  
  ### Import data, rename columns and order it
  data <- reactive({fct_data_import(input$file_excel$datapath)})
  
  ### Show the first "n" observations
  output$view_validation_data <- renderPrint({
    validate(need(data() != "", "Please load a data set or select 'Load test data'")) 
    head(data()$data_val, n=5) })
  
  output$view_calibration_data <- renderPrint({
    validate(need(data() != "", "Please load a data set or select 'Load test data'")) 
    head(data()$data_cal, n=5)})
  
  ### Show all imported data
  output$view_all_calibration_data <- renderDataTable({DT::datatable(data()$data_cal, options=list(paging=FALSE, searching=FALSE))})
  output$view_all_validation_data <- renderDataTable({DT::datatable(data()$data_val, options=list(paging=FALSE, searching=FALSE))})
  
  ### Show back-prediction in the app
  output$view_results_R1 <- renderDataTable({DT::datatable(regression_models()$model_R1$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R2 <- renderDataTable({DT::datatable(regression_models()$model_R2$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R3 <- renderDataTable({DT::datatable(regression_models()$model_R3$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R4 <- renderDataTable({DT::datatable(regression_models()$model_R4$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R5 <- renderDataTable({DT::datatable(regression_models()$model_R5$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R6 <- renderDataTable({DT::datatable(regression_models()$model_R6$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R7 <- renderDataTable({DT::datatable(regression_models()$model_R7$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R8 <- renderDataTable({DT::datatable(regression_models()$model_R8$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R9 <- renderDataTable({DT::datatable(regression_models()$model_R9$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R10 <- renderDataTable({DT::datatable(regression_models()$model_R10$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R11 <- renderDataTable({DT::datatable(regression_models()$model_R11$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R12 <- renderDataTable({DT::datatable(regression_models()$model_R12$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R13 <- renderDataTable({DT::datatable(regression_models()$model_R13$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  output$view_results_R14 <- renderDataTable({DT::datatable(regression_models()$model_R14$data_back_prediction, options=list(paging=FALSE, searching=FALSE))})
  
  # modal windows to view all data
  observeEvent(input$show_all_calibration_data, {
    showModal(modalDialog(
      title = 'Imported calibration data',
      DT::dataTableOutput("view_all_calibration_data"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_all_validation_data, {
    showModal(modalDialog(
      title = 'Imported validation data',
      DT::dataTableOutput("view_all_validation_data"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  # modal windows for back predictions
  observeEvent(input$show_results_R1, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R1"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R2, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R2"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R3, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R3"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R4, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R4"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R5, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R5"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R6, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R6"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R7, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R7"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R8, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R8"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R9, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R9"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R10, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R10"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R11, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R11"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R12, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R12"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R13, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R13"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  observeEvent(input$show_results_R14, {
    showModal(modalDialog(
      title = 'Back predictions',
      DT::dataTableOutput("view_results_R14"),
      easyClose = T,
      footer = modalButton('Close')
    ))})
  
  # Hide/show boxes
  observeEvent(input$test_data, {
    if(input$test_data == 0){
      shinyjs::show(id = "boxViewData")
    } else {
      shinyjs::hide(id = "boxViewData")
    }
  })
  
  ###Perform regressions
  regression_models <- reactive({fct_regression_models(data(), 
                                                       selected_models=input$selected_models, 
                                                       beta=95, 
                                                       test_data=input$test_data)})
 
  
  output$plot_model_R1 <- renderPlotly({
    validate(need("1" %in% input$selected_models, ""))
    fct_plot_model_1(regression_models()$model_R1, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R2 <- renderPlotly({
    validate(need("2" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R2, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R3 <- renderPlotly({
    validate(need("3" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R3, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R4 <- renderPlotly({
    validate(need("4" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R4, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R5 <- renderPlotly({
    validate(need("5" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R5, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R6 <- renderPlotly({
    validate(need("6" %in% input$selected_models, ""))
    fct_plot_model_1(regression_models()$model_R6, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R7 <- renderPlotly({
    validate(need("7" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R7, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R8 <- renderPlotly({
    validate(need("8" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R8, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R9 <- renderPlotly({
    validate(need("9" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R9, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R10 <- renderPlotly({
    validate(need("10" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R10, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R11 <- renderPlotly({
    validate(need("11" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R11, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R12 <- renderPlotly({
    validate(need("12" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R12, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R13 <- renderPlotly({
    validate(need("13" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R13, xunit=input$xunit, yunit=input$yunit)})
  
  output$plot_model_R14 <- renderPlotly({
    validate(need("14" %in% input$selected_models, "")) 
    fct_plot_model_1(regression_models()$model_R14, xunit=input$xunit, yunit=input$yunit)})

  
  output$get_excel_file <- downloadHandler(
    filename = function() {
      if(input$test_data==1){file<-"Backprediction.xlsx"}else{file<-paste0("Backprediction_", sub("\\.[[:alnum:]]+$", "",input$file_excel$name), ".xlsx")}
      return(file)},
    content = function(file){
      fct_create_excel_file(file, regression_models(), input=input)
      file.rename("Back_prediction.xlsx",file)
    }
  )
  
  #Disable the select models buttons if no data
  #shinyjs::disable("selected_models")
  observe({if(input$test_data==1 | is.null(data())==FALSE){shinyjs::enable("selected_models")}})
  observe({if(input$test_data==0 & is.null(data())==TRUE){shinyjs::disable("selected_models")}})
  
  #Disable the report button if no model selected
  #shinyjs::disable("get_excel_file")
  observe({if(is.null(input$selected_models)==FALSE){shinyjs::enable("get_excel_file")}})
  observe({if(is.null(input$selected_models)==TRUE){shinyjs::disable("get_excel_file")}})
  
  ### Other
  output$view_selected_models <- renderPrint({input$selected_models})
  
  
})



