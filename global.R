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

rm(list = ls())

# load useful packages
library(dplyr)
library(shiny)
library(ggplot2)
library(openxlsx)
library(plotly)
library(DT)
library(shinyBS)
library(drc)
library(shinyjs)
library(nlreg)
library(bs4Dash)
library(fresh)

choice_models <- c("1: Linear regression"=1,
                   "2: Weighted (1/X) linear regression"=2,
                   "3: Weighted (1/X^2) linear regression"=3,
                   "4: Linear regression after (base 10) LOGARITHM transformation of both concentration and response"=4,
                   "5: Linear regression after SQUARE ROOT transformation of both concentration and response"=5,
                   "6: Quadratic regression"=6,
                   "7: Weighted (1/X) Quadratic regression"=7,
                   "8: Weighted (1/X^2) Quadratic regression"=8,
                   "9: Four parameters logistic regression"=9,
                   "10: Weighted (POM) Four parameters logistic regression"=10,
                   "11: Five parameters logistic regression"=11,
                   "12: Weighted (POM) Five parameters logistic regression"=12,
                   "13: Power regression"=13,
                   "14: Weighted (POM) Power regression"=14)

title <- "Back-PredicteR"
app_version <- "1.1"
last_update <- "April 2023"

# theme
PLXtheme <- create_theme(
  bs4dash_sidebar_light(
    bg = "#FFF"
  ),
  bs4dash_layout(
    sidebar_width = "300px"
  ),
  bs4dash_status(
    primary = "#009aa8"
  ),
  bs4dash_color(
    gray_900 = "#233c4c"
  )
)