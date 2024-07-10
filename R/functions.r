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

fct_plot_model_1 <- function(data_plot, xunit, yunit){
  
  min_response <- min(data_plot$data_cal$response)
  
  p <- ggplot() + 
    geom_point(aes(x=concentration, y=response), alpha=0.7, data=data_plot$data_cal) + 
    geom_line(aes(y=fit, x=concentration), linetype="dotted", data=data_plot$prediction_cal) +
    geom_point(aes(x=0, y=response), col="red", shape=4, data=data_plot$data_back_prediction) +
    geom_segment(aes(y=response, yend=response, x=0, xend=back_prediction), alpha=0.3, linetype="dotted", col="red", data=data_plot$data_back_prediction) +
    geom_segment(y=min_response, aes(yend=response, x=back_prediction, xend=back_prediction), alpha=0.3, linetype="dotted", col="red", data=data_plot$data_back_prediction) +
    ylab(paste0("response (", yunit, ")\n")) + xlab(paste0("concentration (", xunit, ")\n")) +
    geom_point(aes(x=back_prediction), y=min_response, col="red", shape=25, size=1, data=data_plot$data_back_prediction)
 
   p <- ggplotly(p) %>% 
    config(p = ., displaylogo = FALSE) %>% 
    layout(margin = list(l = 100)) 
   
  return(p)
}



fct_plot_model_2 <- function(data_plot, xunit, yunit){
  
  min_response <- min(data_plot$data_cal$response)
  
  p <- ggplot() + geom_point(aes(x=concentration, y=response), alpha=0.7, data=data_plot$data_cal)
  p <- p + geom_line(aes(y=fit, x=concentration), linetype="dotted", data=data_plot$prediction_cal)
  p <- p + geom_point(aes(x=0, y=response), col="red", shape=4, data=data_plot$data_back_prediction) 
  p <- p + geom_segment(aes(y=response, yend=response, x=0, xend=back_prediction), alpha=1, linetype="dotted", col="red", data=data_plot$data_back_prediction)
  p <- p + geom_segment(y=min_response, aes(yend=response, x=back_prediction, xend=back_prediction), alpha=1, linetype="dotted", col="red", data=data_plot$data_back_prediction)
  p <- p + ylab(paste0("response (", yunit, ")\n")) + xlab(paste0("concentration (", xunit, ")\n"))
  p <- p + geom_point(aes(x=back_prediction), y=min_response, col="red", shape=25, size=1, data=data_plot$data_back_prediction) 
  p <- p + geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1), label=data_plot$equation_plot, parse=TRUE)
  return(p)
}


fct_create_excel_file <- function(file, regression_models, input){
  
  tempfile_list <- NULL
  wb <- createWorkbook("Arlenda")
  
  for(i in 1:length(regression_models)){
    sheetname <- as.character(regression_models[[i]]["name"])
    data <- data.frame(regression_models[[i]]["data_back_prediction"])
    colnames(data) <- c("sampleID", paste0("response (", input$xunit, ")"), paste0("back prediction (", input$yunit, ")"))
    data_cal <- data.frame(regression_models[[i]]["data_cal"])
    colnames(data_cal) <- c("sampleID", paste0("concentration (", input$xunit, ")"), paste0("response (", input$yunit, ")"))
    
    addWorksheet(wb, sheet=sheetname)
    
    #logo
    insertImage(wb, sheet=sheetname, "www/logoPharmalexBlanc.png", width=4, height=0.5, startRow=1, startCol=1, units="in", dpi=300)
    
    #data
    writeDataTable(wb, sheet=sheetname, startRow=7, data_cal, withFilter=FALSE)
    writeDataTable(wb, sheet=sheetname, startRow=c(nrow(data_cal)+9), data, withFilter=FALSE)
    setColWidths(wb, sheet=sheetname, cols=1:5, widths="auto")
    
    #Add the plot to the Excel sheet
    tempfile <- paste0(tempfile(), ".jpg")
    tempfile_list <- c(tempfile_list, tempfile)
    jpeg(tempfile, width=14, height=10, units="cm", res=200) 
    print(fct_plot_model_2(regression_models[[i]], xunit=input$xunit, yunit=input$yunit))
    dev.off()
    #insertPlot(wb, sheet=sheetname, fileType="jpg", width=14, height=10, startRow=7, startCol=5, units="cm", dpi=200)
    insertImage(wb, sheet=sheetname, tempfile, width=14, height=10, startRow=7, startCol=5, units="cm", dpi=200)
  } 
  
  saveWorkbook(wb, file, overwrite=TRUE)
  file.remove(tempfile_list)
}

fct_data_import <- function(datapath){
  
  if(is.null(datapath)){return(NULL)}
  
  data <- na.omit(read.xlsx(datapath, sheet = "Calibration", startRow=4, colNames=FALSE))
  colnames(data) <- c("sampleID", "concentration", "response")
  data_cal <- data
  
  data <- na.omit(read.xlsx(datapath, sheet = "Data to back predict", startRow=4, colNames=FALSE))
  colnames(data) <- c("sampleID", "response")
  data_val <- data
  
  
  return(list(data_cal=data_cal, data_val=data_val))}

##############################
### Self-starting function ###
##############################

#Self-starting function for Power regression model
powermodel=function(x,a,b,c){a+b*x^c}

powermodelInit=function(mCall,LHS,data){
  xy=sortedXyData(mCall[["x"]],LHS,data)
  lmFit1=lm(xy[,"y"]~1) #for "intercept", a
  lmFit2=lm(log(xy[,"y"])~log(xy[,"x"])) #for b and c
  coefs1=coef(lmFit1)
  coefs2=coef(lmFit2)
  a=coefs1
  b=exp(coefs2[1])
  c=coefs2[2]
  value=c(a,b,c)
  names(value)=mCall[c("a","b","c")]
  value
}
SSpower=selfStart(powermodel,powermodelInit,c("a","b","c"))


###################
### Regressions ###
###################

fct_regression_models <- function(data, selected_models, beta, test_data){
  regression_models <- list()
  
  ### Model R1: Linear regression
  if("1" %in% selected_models){
    fct_lm1 <- function(data, beta){ 
      
      lm_R1 <- lm(formula=response~concentration, data=data$data_cal) 
      p_alpha <- unname(lm_R1$coefficients[1])
      p_beta <- unname(lm_R1$coefficients[2])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      prediction_cal <- as.data.frame(predict(lm_R1, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- (data$data_val$response-p_alpha)/p_beta
      
      eq <- substitute(italic(y) == a + b * italic(x), list(a = format(p_alpha, digits = 2), b = format(p_beta, digits = 2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Linear reg.", error=c(0), data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R1-5.xlsx")}
    regression_models$model_R1 <- tryCatch(
      {fct_lm1(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 1, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  
  ### Model R2: Weighted (1/X) linear regression
  if("2" %in% selected_models){
    fct_lm2 <- function(data, beta){ 
      
      lm_R2 <- lm(formula=response~concentration, weights=1/concentration, data=data$data_cal) 
      p_alpha <- unname(lm_R2$coefficients[1])
      p_beta <- unname(lm_R2$coefficients[2])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      prediction_cal <- as.data.frame(predict(lm_R2, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- (data$data_val$response-p_alpha)/p_beta
      
      eq <- substitute(italic(y) == a + b * italic(x), list(a = format(p_alpha, digits = 2), b = format(p_beta, digits = 2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (X^-1) linear reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R1-5.xlsx")}
    regression_models$model_R2 <- tryCatch(
      {fct_lm2(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 2, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R3: Weighted (1/X^2) linear regression
  if("3" %in% selected_models){
    fct_lm3 <- function(data, beta){ 
      
      lm_R3 <- lm(formula=response~concentration, weights=1/concentration^2, data=data$data_cal) 
      p_alpha <- unname(lm_R3$coefficients[1])
      p_beta <- unname(lm_R3$coefficients[2])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      prediction_cal <- as.data.frame(predict(lm_R3, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- (data$data_val$response-p_alpha)/p_beta
      
      eq <- substitute(italic(y) == a + b * italic(x), list(a = format(p_alpha, digits = 2), b = format(p_beta, digits = 2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (X^-2) linear reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R1-5.xlsx")}
    regression_models$model_R3 <- tryCatch(
      {fct_lm3(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 3, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R4: Linear regression after (base 10) LOGARITHM transformation of both concentration and response
  if("4" %in% selected_models){
    fct_lm4 <- function(data, beta){ 
      
      lm_R4 <- lm(formula=log10(response)~log10(concentration), data=data$data_cal) 
      p_alpha <- unname(lm_R4$coefficients[1])
      p_beta <- unname(lm_R4$coefficients[2])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      prediction_cal <- as.data.frame(predict(lm_R4, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- 10^prediction_cal
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- 10^((log10(data$data_val$response)-p_alpha)/p_beta)
      
      eq <- substitute(italic(log(y)) == a + b * italic(log(x)), list(a=format(p_alpha, digits=2), b=format(p_beta, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Linear reg. (LOG10)", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R1-5.xlsx")}
    regression_models$model_R4 <- tryCatch(
      {fct_lm4(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 4, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R5: Linear regression after SQUARE ROOT transformation of both concentration and response
  if("5" %in% selected_models){
    fct_lm5 <- function(data, beta){ 
      
      lm_R5 <- lm(formula=sqrt(response)~sqrt(concentration), data=data$data_cal) 
      p_alpha <- unname(lm_R5$coefficients[1])
      p_beta <- unname(lm_R5$coefficients[2])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      prediction_cal <- as.data.frame(predict(lm_R5, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- prediction_cal^2
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- ((sqrt(data$data_val$response)-p_alpha)/p_beta)^2
      
      eq <- substitute(italic(sqrt(y)) == a + b * italic(sqrt(x)), list(a=format(p_alpha, digits=2), b=format(p_beta, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Linear reg. (SQUARE ROOT)", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R1-5.xlsx")}
    regression_models$model_R5 <- tryCatch(
      {fct_lm5(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 5, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R6: Quadratic regression
  if("6" %in% selected_models){
    fct_lm6 <- function(data, beta){ 
      
      lm_R6 <- lm(formula=response~concentration + I(concentration^2), data=data$data_cal) 
      p_alpha <- unname(lm_R6$coefficients[1])
      p_beta <- unname(lm_R6$coefficients[2])
      p_gamma <- unname(lm_R6$coefficients[3])
      
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(predict(lm_R6, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- cbind(prediction_cal, sequence)
      
      rho <- p_beta^2-(4*p_gamma*(p_alpha-data$data_val$response))
      x1 <- (-p_beta+sqrt(rho))/(2*p_gamma)
      x2 <- (-p_beta-sqrt(rho))/(2*p_gamma)
      # back_prediction_val<-ifelse(abs(x1-data$concentration)<abs(x2-data$concentration), x1, x2)
      back_prediction<-x1
      
      eq <- substitute(italic(y) == a + b * italic(x) + g * italic(x^2), list(a=format(p_alpha, digits=2), b=format(p_beta, digits=2), g=format(p_gamma, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Quadratic reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha, p_gamma=p_gamma), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R6-8.xlsx")}
    regression_models$model_R6 <- tryCatch(
      {fct_lm6(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 6, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R7: Weighted (1/X) Quadratic regression
  if("7" %in% selected_models){
    fct_lm7 <- function(data, beta){ 
      
      lm_R7 <- lm(formula=response~concentration + I(concentration^2), weights=1/concentration, data=data$data_cal) 
      p_alpha <- unname(lm_R7$coefficients[1])
      p_beta <- unname(lm_R7$coefficients[2])
      p_gamma <- unname(lm_R7$coefficients[3])
      
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(predict(lm_R7, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- cbind(prediction_cal, sequence)
      
      rho <- p_beta^2-(4*p_gamma*(p_alpha-data$data_val$response))
      x1 <- (-p_beta+sqrt(rho))/(2*p_gamma)
      x2 <- (-p_beta-sqrt(rho))/(2*p_gamma)
      
      back_prediction<-x1
      
      eq <- substitute(italic(y) == a + b * italic(x) + g * italic(x^2), list(a=format(p_alpha, digits=2), b=format(p_beta, digits=2), g=format(p_gamma, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (X^-1) Quadratic reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha, p_gamma=p_gamma), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R6-8.xlsx")}
    regression_models$model_R7 <- tryCatch(
      {fct_lm7(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 7, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R8: Weighted (1/X^2) Quadratic regression
  if("8" %in% selected_models){
    fct_lm8 <- function(data, beta){ 
      
      lm_R8 <- lm(formula=response~concentration + I(concentration^2), weights=1/concentration^2, data=data$data_cal) 
      p_alpha <- unname(lm_R8$coefficients[1])
      p_beta <- unname(lm_R8$coefficients[2])
      p_gamma <- unname(lm_R8$coefficients[3])
      
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(predict(lm_R8, interval="prediction", level=(beta/100), newdata=sequence))
      prediction_cal <- cbind(prediction_cal, sequence)
      
      rho <- p_beta^2-(4*p_gamma*(p_alpha-data$data_val$response))
      x1 <- (-p_beta+sqrt(rho))/(2*p_gamma)
      x2 <- (-p_beta-sqrt(rho))/(2*p_gamma)
      
      back_prediction<-x1
      
      eq <- substitute(italic(y) == a + b * italic(x) + g * italic(x^2), list(a=format(p_alpha, digits=2), b=format(p_beta, digits=2), g=format(p_gamma, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (X^-2) Quadratic reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(p_beta=p_beta, p_alpha=p_alpha), equation_plot=equation_plot))
    }
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R6-8.xlsx")}
    regression_models$model_R8 <- tryCatch(
      {fct_lm8(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 8, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R9: Four parameters logistic Regression
  if("9" %in% selected_models){
    fct_nlm9 <- function(data, beta){ 
      
      nlm_R9 <- drm(response~concentration, fct=LL.4(names=c("Slope", "Lower", "Upper", "ED50")),data=data$data_cal)
      
      slope <- unname(nlm_R9$coefficients[1])
      lower <- unname(nlm_R9$coefficients[2])
      upper <- unname(nlm_R9$coefficients[3])
      ED50 <- unname(nlm_R9$coefficients[4])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(predict(nlm_R9, interval="prediction", level=(beta/100), newdata=sequence))
      colnames(prediction_cal)[1] <- "fit"
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- exp(log(((upper-lower)/(data$data_val$response-lower))-1)/slope + log(ED50))
      
      eq <- substitute(italic(y) == l + frac(u-l,1+exp(s * log(x)-log(ed50))), list(l=format(lower, digits=2), u=format(upper, digits=2), s=format(slope, digits=2), ed50=format(ED50, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="4pl reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(slope=slope, lower=lower, upper=upper, ED50=ED50), equation_plot=equation_plot))
    }
    
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R9-12.xlsx")}
    regression_models$model_R9 <- tryCatch(
      {fct_nlm9(data, beta)},
      error=function(e) {
        print(e)
        shinyjs::info(paste0("Something went wrong with model 9, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R10: Weighted (POM) Four parameters logistic Regression
  if("10" %in% selected_models){
    fct_nlm10 <- function(data, beta){
      
      nlm_R10_start <- drm(response~concentration, fct=LL.4(names=c("Slope", "Lower", "Upper", "ED50")),data=data$data_cal)
      start<- c(Slope=unname(coef(nlm_R10_start)[1]), Lower=unname(coef(nlm_R10_start)[2]), Upper=unname(coef(nlm_R10_start)[3]), ED50=unname(coef(nlm_R10_start)[4]))
      start <- c(start, g=1)
      
      #nls_R10 <- nls(response~Lower+((Upper-Lower)/(1+exp(Slope*(log(concentration)-log(ED50))))), start=start, data=data$data_cal)
      #nlreg_R10 <- nlreg(response~Lower+((Upper-Lower)/(1+exp(Slope*(log(concentration)-log(ED50))))), start=start, data=data$data_cal)
      nlm_R10 <- nlreg(response~Lower+((Upper-Lower)/(1+exp(Slope*(log(concentration)-log(ED50))))), start=start, data=data$data_cal,
                       weights= ~ (Lower+((Upper-Lower)/(1+exp(Slope*(log(concentration)-log(ED50))))))^g)
      
      slope <- unname(coef(nlm_R10)[1])
      lower <- unname(coef(nlm_R10)[2])
      upper <- unname(coef(nlm_R10)[3])
      ED50 <- unname(coef(nlm_R10)[4])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(lower+((upper-lower)/(1+exp(slope*(log(sequence$concentration)-log(ED50))))))
      colnames(prediction_cal)[1] <- "fit"
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- exp(log(((upper-lower)/(data$data_val$response-lower))-1)/slope + log(ED50))
      
      eq <- substitute(italic(y) == l + frac(u-l,1+exp(s * log(x)-log(ed50))), list(l=format(lower, digits=2), u=format(upper, digits=2), s=format(slope, digits=2), ed50=format(ED50, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (POM) 4pl reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(slope=slope, lower=lower, upper=upper, ED50=ED50), equation_plot=equation_plot))
    }
    
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R9-12.xlsx")}
    regression_models$model_R10 <- tryCatch(
      {fct_nlm10(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 10, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R11: Five parameters logistic Regression
  if("11" %in% selected_models){
    fct_nlm11 <- function(data, beta){ 
      
      nlm_R11 <- drm(response~concentration, fct=LL.5(names=c("Slope", "Lower", "Upper", "ED50", "Assymetry")),data=data$data_cal)
      
      slope <- unname(nlm_R11$coefficients[1])
      lower <- unname(nlm_R11$coefficients[2])
      upper <- unname(nlm_R11$coefficients[3])
      ED50 <- unname(nlm_R11$coefficients[4])
      assymetry <- unname(nlm_R11$coefficients[5])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(predict(nlm_R11, interval="prediction", level=(beta/100), newdata=sequence))
      colnames(prediction_cal)[1] <- "fit"
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- exp(log(((upper-lower)/(data$data_val$response-lower))^(1/assymetry)-1)/slope + log(ED50))
      
      eq <- substitute(italic(y) == l + frac(u-l,(1+exp(s * log(x)-log(ed50)))^a), list(l=format(lower, digits=2), u=format(upper, digits=2), s=format(slope, digits=2), ed50=format(ED50, digits=2), a=format(assymetry, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="5pl reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(slope=slope, lower=lower, upper=upper, ED50=ED50, assymetry=assymetry), equation_plot=equation_plot))
    }
    
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R9-12.xlsx")}
    regression_models$model_R11 <- tryCatch(
      {fct_nlm11(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 11, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  
  
  ### Model R12: Weighted (POM) Five parameters logistic Regression
  if("12" %in% selected_models){
    fct_nlm12 <- function(data, beta){
      
      nlm_R12_start <- drm(response~concentration, fct=LL.5(names=c("Slope", "Lower", "Upper", "ED50", "Assymetry")),data=data$data_cal)
      start<- c(Slope=unname(coef(nlm_R12_start)[1]), Lower=unname(coef(nlm_R12_start)[2]), Upper=unname(coef(nlm_R12_start)[3]), ED50=unname(coef(nlm_R12_start)[4]), Assymetry=unname(coef(nlm_R12_start)[5]))
      start <- c(start, g=1)
      
      nlm_R12 <- nlreg(response~Lower+((Upper-Lower)/(1+exp(Slope*(log(concentration)-log(ED50))))^Assymetry), start=start, data=data$data_cal,
                       weights= ~ (Lower+((Upper-Lower)/(1+exp(Slope*(log(concentration)-log(ED50)))))^Assymetry)^g)
      
      slope <- unname(coef(nlm_R12)[1])
      lower <- unname(coef(nlm_R12)[2])
      upper <- unname(coef(nlm_R12)[3])
      ED50 <- unname(coef(nlm_R12)[4])
      assymetry <- unname(coef(nlm_R12)[5])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(lower+((upper-lower)/(1+exp(slope*(log(sequence$concentration)-log(ED50))))^assymetry))
      colnames(prediction_cal)[1] <- "fit"
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- exp(log(((upper-lower)/(data$data_val$response-lower))^(1/assymetry)-1)/slope + log(ED50))
      
      eq <- substitute(italic(y) == l + frac(u-l,(1+exp(s * log(x)-log(ed50)))^a), list(l=format(lower, digits=2), u=format(upper, digits=2), s=format(slope, digits=2), ed50=format(ED50, digits=2), a=format(assymetry, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (POM) 5pl reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(slope=slope, lower=lower, upper=upper, ED50=ED50), equation_plot=equation_plot))
    }
    
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R9-12.xlsx")}
    regression_models$model_R12 <- tryCatch(
      {fct_nlm12(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 12, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  ### Model R13: Power Regression
  if("13" %in% selected_models){
    fct_nlm13 <- function(data, beta){ 
      
      nlm_R13 <- nls(response ~ SSpower(concentration, pA, pB, pY), data=data$data_cal)
      
      pA <- unname(summary(nlm_R13)$coef[1,1])
      pB <- unname(summary(nlm_R13)$coef[2,1])
      pC <- unname(summary(nlm_R13)$coef[3,1])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(predict(nlm_R13, interval="prediction", level=(beta/100), newdata=sequence))
      colnames(prediction_cal)[1] <- "fit"
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- ((data$data_val$response-pA)/pB)^(1/pC)
      
      eq <- substitute(italic(y) == a + b * x^c, list(a=format(pA, digits=2), b=format(pB, digits=2), c=format(pC, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Power reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(pA=pA, pB=pB, pC=pC), equation_plot=equation_plot))
    }
    
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R13-14.xlsx")}
    regression_models$model_R13 <- tryCatch(
      {fct_nlm13(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 13, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  ### Model R14: Weighted (POM) Power Regression
  
  if("14" %in% selected_models){
    fct_nlm14 <- function(data, beta){ 
      
      nlm_R14_start <- nls(response ~ SSpower(concentration, pA, pB, pY), data=data$data_cal)
      start<- c(pA=unname(coef(nlm_R14_start)[1]), pB=unname(coef(nlm_R14_start)[2]), pC=unname(coef(nlm_R14_start)[3]))
      start <- c(start, g=1)
      
      nlm_R14 <- nlreg(response~pA+pB*concentration^pC, start=start, data=data$data_cal,
                       weights= ~ (pA+pB*concentration^pC)^g)
      
      pA <- unname(summary(nlm_R14)$coef[1,1])
      pB <- unname(summary(nlm_R14)$coef[2,1])
      pC <- unname(summary(nlm_R14)$coef[3,1])
      
      sequence <- data.frame(concentration=seq(from=min(data$data_cal$concentration), to=max(data$data_cal$concentration), length.out=1000))
      
      prediction_cal <- as.data.frame(pA+pB*sequence$concentration^pC)
      colnames(prediction_cal)[1] <- "fit"
      prediction_cal <- cbind(prediction_cal, sequence)
      
      back_prediction <- ((data$data_val$response-pA)/pB)^(1/pC)
      
      eq <- substitute(italic(y) == a + b * x^c, list(a=format(pA, digits=2), b=format(pB, digits=2), c=format(pC, digits=2)))
      equation_plot <- as.character(as.expression(eq))
      
      return(list(name="Weighted (POM) Power reg.", data_cal=data$data_cal, data_back_prediction=cbind(data$data_val, back_prediction), prediction_cal=prediction_cal, parameters=list(pA=pA, pB=pB, pC=pC), equation_plot=equation_plot))
    }
    
    if(test_data==TRUE){data <- fct_data_import("data/test_data_R13-14.xlsx")}
    regression_models$model_R14 <- tryCatch(
      {fct_nlm14(data, beta)},
      error=function(e) {
        shinyjs::info(paste0("Something went wrong with model 14, please desactivate the model or upload another dataset. This may help you to diagnose the problem: \n ",e))
      })
  }
  
  return(regression_models)
}
