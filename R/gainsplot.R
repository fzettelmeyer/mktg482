#' A function to plot a gains curve
#'
#' This function allows you to compare model performace by comparing the gains curves of models.
#' @param score1, score 2, ... (required)
#' @param label (required)
#' @param bin (defaults to 10) 
#' @keywords gains, auc
#' @export
#' @examples
#' gainsplot(logit1$fitted.values, logit2$fitted.values, logit3$fitted.values, label.var = bbb$buyer)

gainsplot <- function(...,label.var, bin = 10) {
  pred.vars <- tibble(...)
  gains.data.build <- NULL
  auc.build <- NULL
  for (i in seq_along(pred.vars)) {
    pred.var <- pred.vars[[i]]
    pred <- ROCR::prediction(pred.var,factor(label.var))
    gain <- ROCR::performance(pred, "tpr", "rpp")
    gains.data <- tibble(Model = i, 
                         Percent.buyers=as.numeric(unlist(gain@y.values)),
                         Percent.customers=as.numeric(unlist(gain@x.values))) %>%
      mutate(Percent.buyers=Percent.buyers*100, 
             Percent.customers=Percent.customers*100)
    auc <- tibble(auc = round(unlist(performance(pred, measure = "auc")@y.values),
                              3))
    print(paste0("AUC of model ",i,": ", auc$auc[1]))
    auc.build <- bind_rows(auc.build, auc)
    gains.data.build <- bind_rows(gains.data.build, gains.data)
  }
  gains.data.build <- mutate(gains.data.build, Model = factor(Model))
  no.model.data <- tbl_df(data.frame(Percent.buyers=c(0,100),
                                     Percent.customers=c(0,100)))
  print(ggplot() + 
          geom_line(data=gains.data.build, 
                    aes(Percent.customers,Percent.buyers,color = Model)) +
          geom_line(data=no.model.data, 
                    aes(Percent.customers,Percent.buyers), linetype=3) +
          labs(x="Percent Customers",
               y="Percent Buyers"))
  return(auc.build)
}
