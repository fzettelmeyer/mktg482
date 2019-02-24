#' A function to report variable importance after glm()
#'
#' This function reports standardized odds ratios and ranks variable by importance
#' It takes as inputs models created by glm or caret using glm
#' @param modelFit reguired: estimated/trained glm model
#' @keywords variable importance
#' @return A tibble with variable, var_imp, p_value, factor, OR_std, OR_std_perc
#' @export
#' @examples
#' imp.glm(logit1)
#' imp.glm(logitFit)

imp.glm <- function(modelFit) {
  UseMethod("imp.glm", modelFit)
}

#' @export
#' @describeIn imp.glm Method for glm()
imp.glm.glm <- function(modelFit) {
  if(modelFit$method != "glm.fit") {
    stop("This function only works when you call glm() or glm through caret")
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
    slice(-1) %>%
    left_join(factor_merge,by = "variable") %>%
    mutate(fac = replace_na(fac, 0),
           factor=ifelse(fac==1, "Yes", "No")) %>%
    left_join(sd_merge, by="variable") %>%
    mutate(var_imp=exp(abs(Estimate*sd)),
           OR_std=ifelse(factor=="Yes",exp(Estimate), exp(Estimate*sd))) %>%
    arrange(-var_imp) %>%
    mutate(temp=ifelse(Estimate>=0, (OR_std-1)*100, - (1-OR_std)*100),
           OR_std_perc=paste0(formatC(temp, format = "f", digits = 1), "%")) %>%
    rename(p_value="Pr(>|z|)") %>%
    select(variable, var_imp, p_value, factor, OR_std, OR_std_perc) %>%
    mutate(p_value=round(p_value,3))

  options(scipen=999, digits =3)
  return(final_result)
}

#' @export
#' @describeIn imp.glm Method for glm in caret. This works regardless of whether the data is pre-processed with "scale"
imp.glm.train <- function(modelFit) {
  if(modelFit$method != "glm") {
    stop("This function only works when you call glm() or glm through caret")
  }
  result <- coef(summary(modelFit$finalModel))
  tmp_coeffs <- coef(modelFit$finalModel)
  tmp_newvars <- names(tmp_coeffs)[-1]
  used.dataframe <- eval(modelFit$call$data)

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
    slice(-1) %>%
    left_join(factor_merge,by = "variable") %>%
    mutate(fac = replace_na(fac, 0),
           factor=ifelse(fac==1, "Yes", "No")) %>%
    left_join(sd_merge, by="variable") %>%
    # mutate(var_imp=exp(abs(Estimate)),
    #        OR_std=ifelse(factor=="Yes",exp(Estimate/sd), exp(Estimate))) %>%
    mutate(var_imp = case_when(
      scaleFlag == TRUE ~ exp(abs(Estimate)),
      scaleFlag == FALSE ~ exp(abs(Estimate*sd))),
      OR_std = case_when(
        scaleFlag == TRUE ~ ifelse(factor=="Yes",exp(Estimate/sd), exp(Estimate)),
        scaleFlag == FALSE ~ ifelse(factor=="Yes",exp(Estimate), exp(Estimate*sd))
      )
    ) %>%
    arrange(-var_imp) %>%
    mutate(temp=ifelse(Estimate>=0, (OR_std-1)*100, - (1-OR_std)*100),
           OR_std_perc=paste0(formatC(temp, format = "f", digits = 1), "%")) %>%
    rename(p_value="Pr(>|z|)") %>%
    select(variable, var_imp, p_value, factor, OR_std, OR_std_perc) %>%
    mutate(p_value=round(p_value,3))
  options(scipen=999, digits =3)
  return(final_result)
}

#' A function to to create a new formula after glmnet in caret
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
  tmp_newvars3 <- paste(indvars, collapse = "+")
  tmp_depvar <- paste0(all.vars(eval(modelFit$call$form)[[2]]), "~")
  glmnet.fm <- as.formula(paste0(tmp_depvar, tmp_newvars3))
  return(glmnet.fm)
}
