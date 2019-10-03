#' A function to report variable importance after glm() or either glm or glmnet through caret
#'
#' This function reports standardized odds ratios and ranks variable by importance: The odds ratios of continuous variables are standardized to a two standard deviation change of the variable. The odds ratios for factor variables are left unchanged. This follows the procedure suggested by Andrew Gelman in "Scaling regression inputs by dividing by two standard deviations," Statistics in Medicine (2008), Vol. 27, pp. 2965-2873.
#' The function takes as inputs models created by glm or caret using glm
#' @param modelFit reguired: estimated/trained glm model
#' @keywords variable importance
#' @return A tibble with variable, var_imp, p_value, factor, OR_std, OR_sd_perc
#' @export
#' @examples
#' varimp.logistic(logit1)
#' varimp.logistic(logitFit)

varimp.logistic <- function(modelFit) {
  UseMethod("varimp.logistic", modelFit)
}

#' @export
#' @describeIn varimp.logistic Method for glm()
varimp.logistic.glm <- function(modelFit) {
  if(modelFit$method != "glm.fit") {
    stop("This function only works when you call glm() or either glm or glmnet through caret")
  }
  result <- coef(summary(modelFit))
  tmp_coeffs <- coef(modelFit)
  tmp_newvars <- names(tmp_coeffs)[-1]
  used.dataframe <- eval(modelFit$call$data)

  allvars_factor <-
    c(names(used.dataframe[sapply(used.dataframe, is.factor)]), names(used.dataframe[sapply(used.dataframe, is.character)]))
  allvars_factor2 <-
    paste(allvars_factor, collapse= "|" )

  factor_merge <- enframe(tmp_newvars[grepl(allvars_factor2,tmp_newvars)], name = "fac", value = "variable") %>%
    mutate(fac=1)

  logit_temp <- glm(modelFit$formula, data=used.dataframe, family=binomial, x=TRUE)
  sd_merge <- as_tibble(logit_temp$x) %>%
    summarize_all(funs(sd)) %>%
    gather(key = "variable", value = "sd")

  final_result <- as_tibble(result, rownames="variable") %>%
    dplyer::slice(-1) %>%
    left_join(factor_merge,by = "variable") %>%
    mutate(fac = replace_na(fac, 0),
           factor = ifelse(fac==1, "Yes", "No")) %>%
    left_join(sd_merge, by = "variable") %>%
    mutate(var_imp = ifelse(fac==1, exp(abs(Estimate)), exp(abs(Estimate*2*sd))),
           OR=exp(Estimate),
           OR_sd=ifelse(fac==1, NA, exp(Estimate*sd))) %>%
    arrange(-var_imp) %>%
    mutate(temp=ifelse(Estimate>=0, (OR-1)*100, - (1-OR)*100),
           OR_perc=paste0(formatC(temp, format = "f", digits = 1), "%")) %>%
    mutate(temp=ifelse(Estimate>=0, (OR_sd-1)*100, - (1-OR_sd)*100),
           OR_sd_perc=ifelse(fac==1, NA, paste0(formatC(temp, format = "f", digits = 1), "%"))
    ) %>%
    rename(p_value="Pr(>|z|)") %>%
    select(variable, var_imp, p_value, factor, OR, OR_perc, sd, OR_sd, OR_sd_perc) %>%
    mutate(p_value=round(p_value,3))

  options(scipen=999, digits =3)
  return(final_result)
}

#' @export
#' @describeIn varimp.logistic Method for either glm or glmnet in caret. This works regardless of whether the data is pre-processed with "scale"
varimp.logistic.train <- function(modelFit) {
  if( !(modelFit$method %in% c("glm","glmnet")) ) {
    stop("This function only works when you call glm() or either glm or glmnet through caret")
  }

  if(modelFit$method=="glm"){
    result <- coef(summary(modelFit$finalModel))
    tmp_coeffs <- coef(modelFit$finalModel)
    tmp_newvars <- names(tmp_coeffs)[-1]
    used.dataframe <- eval(modelFit$call$data)
  }

  if(modelFit$method=="glmnet"){
    tmp.fm <- getform.glmnet(modelFit)
    tmp.df <- modelFit$trainingData
    colnames(tmp.df)[1] <- as.character(tmp.fm)[2]
    tmp.lr <- glm(tmp.fm, data=tmp.df, family=binomial)
    tmp.coef <- coef(modelFit$finalModel,modelFit$finalModel$lambdaOpt)[names(coef(tmp.lr)),1]
    tmp.lr$coefficients <- tmp.coef
    oldmodelFit <- modelFit
    modelFit <- tmp.lr

    result <- coef(summary(modelFit))
    result[,4] <- NA
    tmp_coeffs <- coef(modelFit)
    tmp_newvars <- names(tmp_coeffs)[-1]
    used.dataframe <- eval(modelFit$call$data)
  }

  allvars_factor <-
    c(names(used.dataframe[sapply(used.dataframe, is.factor)]), names(used.dataframe[sapply(used.dataframe, is.character)]))
  allvars_factor2 <-
    paste(allvars_factor, collapse= "|" )

  factor_merge <- enframe(tmp_newvars[grepl(allvars_factor2,tmp_newvars)], name = "fac", value = "variable") %>%
    mutate(fac=1)

  logit_temp <- glm(modelFit$terms, data=used.dataframe, family=binomial, x=TRUE)
  sd_merge <- as_tibble(logit_temp$x) %>%
    summarize_all(funs(sd)) %>%
    gather(key = "variable", value = "sd")

  scaleFlag <- length(modelFit$preProcess$method$scale)>0

  final_result <- as_tibble(result, rownames="variable") %>%
    dplyer::slice(-1) %>%
    left_join(factor_merge,by = "variable") %>%
    mutate(fac = replace_na(fac, 0),
           factor=ifelse(fac==1, "Yes", "No")) %>%
    left_join(sd_merge, by="variable") %>%
    mutate(var_imp = case_when(
      scaleFlag == TRUE ~ ifelse(fac==1, exp(abs(Estimate/sd)), exp(abs(Estimate*2))),
      scaleFlag == FALSE ~ ifelse(fac==1, exp(abs(Estimate)), exp(abs(Estimate*2*sd)))),
      OR = case_when(
        scaleFlag == TRUE ~ exp(Estimate/sd),
        scaleFlag == FALSE ~ exp(Estimate)),
      OR_sd = case_when(
        scaleFlag == TRUE ~ ifelse(fac==1, NA, exp(Estimate)),
        scaleFlag == FALSE ~ ifelse(fac==1, NA, exp(Estimate*sd)))
    ) %>%
    arrange(-var_imp) %>%
    mutate(temp=ifelse(Estimate>=0, (OR-1)*100, - (1-OR)*100),
           OR_perc=paste0(formatC(temp, format = "f", digits = 1), "%")) %>%
    mutate(temp=ifelse(Estimate>=0, (OR_sd-1)*100, - (1-OR_sd)*100),
           OR_sd_perc=ifelse(fac==1, NA, paste0(formatC(temp, format = "f", digits = 1), "%"))
    ) %>%
    rename(p_value="Pr(>|z|)") %>%
    select(variable, var_imp, p_value, factor, OR, OR_perc, sd, OR_sd, OR_sd_perc) %>%
    mutate(p_value=round(p_value,3))
  options(scipen=999, digits =3)
  return(final_result)
}


#' A function to create a new formula after glmnet in caret
#'
#' This function takes non-zero variables after glmnet and returns a
#' new formula. If any dummy created from a factor is non-zero, it will
#' incluce the factor variable
#' @param modelFit required: trained caret glmnet model
#' @param lambda optional: a lambda penalty (default is modelFit$bestTune$lambda)
#' @keywords glmnet LASSO
#' @export
#' @return A formula
#' @examples
#' getform.glmnet(glmnetFit)
#' @export

getform.glmnet <- function(modelFit, lambda = modelFit$bestTune$lambda) {
  if(modelFit$method != "glmnet") {
    stop("This function only works when you call glmnet through caret")
  }
  tmp_coeffs <- coef(modelFit$finalModel, lambda)
  tmp_newvars <- names(tmp_coeffs[, 1])[which(tmp_coeffs[, 1] != 0)][-1]
  used.dataframe <- eval(modelFit$call$data)

  allvars_factor <- enframe(attr(modelFit$terms, "dataClass"), name = "variable", value = "type") %>% filter(type !="numeric") %>% select(variable) %>% unlist %>% unname()

  allvars_not_factor <-
    enframe(attr(modelFit$terms, "dataClass"), name = "variable", value = "type") %>% filter(type =="numeric") %>% select(variable) %>% unlist %>% unname()

  new_factor <- NULL
  for (i in allvars_factor) {
    temp <- unique(str_extract(tmp_newvars, i))
    temp2 <- temp[!is.na(temp)]
    new_factor <- c(new_factor, temp2)
  }

  new_not_factor <- NULL
  for (i in allvars_not_factor) {
    temp <- unique(str_extract(tmp_newvars, i))
    temp2 <- temp[!is.na(temp)]
    new_not_factor <- c(new_not_factor, temp2)
  }

  indvars <- c(new_not_factor, new_factor)
  allvars <- attr(modelFit$terms,"term.labels")
  removedvars <- setdiff(allvars, indvars)
  removedvars_disp <- paste(removedvars, collapse=", ")
  message(paste("glmnet removed variables:", removedvars_disp, sep=" "))
  tmp_newvars3 <- paste(indvars, collapse = "+")
  tmp_depvar <- paste0(all.vars(eval(modelFit$call$form)[[2]]), "~")
  glmnet.fm <- as.formula(paste0(tmp_depvar, tmp_newvars3))
  return(glmnet.fm)
}

#' A function to create a plot after varimp.logistic()
#'
#' This function takes the dataframe created by varimp.logistic() and returns a
#' plot. It will also pass on the dataset, so it can be used in the middle or
#' the end of a pipe.
#' @param .data a dataframe created by varimp.logistic() (can be piped)
#' @keywords plot variable importance
#' @export
#' @return a plot and the unmodified dataframe created by varimp.logistic()
#' @examples
#' varimp.logistic(modelFit) %>% filter(var_imp > 1) %>% plotimp.logistic()
#' @export

plotimp.logistic <- function(.data) {
  plotdata <- .data %>%
    mutate(variable=fct_reorder(variable, var_imp))

  print(ggplot(data=plotdata) + geom_col(aes(x = variable, y = var_imp, fill = factor)) +
          scale_y_continuous(expand = expand_scale(add = c(-1,.1)))+coord_flip())
  return(.data)
}

