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

### Read all functions
source("global.R")
sapply(list.files(path = "R", full.names = T), source)

input <- list()
#input$file_excel$datapath <- "data/test_data_R1-5.xlsx"
#input$file_excel$datapath <- "data/test_data_R9-12.xlsx"
input$file_excel$datapath <- "data/test_data_R13-14.xlsx"
#input$file_excel$datapath <- "C:/Users/thomas.demarchin/Desktop/Back_1.xlsx" 
input$selected_models <- c("13","12","1")
input$beta <- 95
input$test_data <- 0
input$xunit <- "µM"
input$yunit <- "V"

#Let's go
data <- fct_data_import(input$file_excel$datapath)

regression_models <- fct_regression_models(data, selected_models=input$selected_models, beta=input$beta, test_data=input$test_data)



# plot_model_R1 <- fct_plot_model_1(regression_models$model_R10, input)
# print(plot_model_R1)
plot_model_R4 <- fct_plot_model_2(regression_models$model_R13, xunit=input$xunit, yunit=input$yunit)
print(plot_model_R4)
# # plot_model_R6 <- fct_plot_model_1(regression_models$model_R6, input=input)
# # print(plot_model_R6)
# 
# plot_model_R10 <- fct_plot_model_1(regression_models$model_R10)
# print(plot_model_R10)
# 
# plot_model_R9 <- fct_plot_model_1(regression_models$model_R9)
# print(plot_model_R9)

# plot_model_R11 <- fct_plot_model_1(regression_models$model_R11)
# print(plot_model_R11)

 fct_create_excel_file("Back_prediction.xlsx", regression_models, input)

