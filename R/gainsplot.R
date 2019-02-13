#' A function to plot a gains curve
#'
#' This function allows you to compare model performace by comparing the gains curves of models.
#' @param label.var (required)
#' @param score1, score 2, ... (at least 1 required)
#' @keywords gains, auc
#' @export
#' @examples
#' gainsplot(logit1$fitted.values, rf$fitted.values, nn$fitted.values, label.var = bbb$buyer)

gainsplot <- function(label.var,...) {
  arglist <- list(...)
  
  for (i in 1:length(arglist)) {
    if (class(arglist[[i]]) == "matrix") {
      if(min(dim(arglist[[i]])) >1 ){
        stop("One of the predictions is matrix with more than one column")
      }
    }
    arglist[[i]] <- as.vector(arglist[[i]])
  }
  
  pred.vars <- as_tibble(do.call(cbind, arglist))
  
  for (i in 3:length(match.call())) {
    names(pred.vars)[i - 2] <- deparse(match.call()[[i]])
  }
  
  
  gains.data.build <- NULL
  auc.build <- NULL
  for (i in seq_along(pred.vars)) {
    pred.var <- pred.vars[[i]]
    pred <- ROCR::prediction(pred.var, factor(label.var))
    gain <- ROCR::performance(pred, "tpr", "rpp")
    gains.data <- tibble(Model = colnames(pred.vars)[i],
                         Percent.buyers=as.numeric(unlist(gain@y.values)),
                         Percent.customers=as.numeric(unlist(gain@x.values))) %>%
      mutate(Percent.buyers=Percent.buyers*100,
             Percent.customers=Percent.customers*100)
    auc <- bind_cols(model = colnames(pred.vars)[i],
                     auc =  round(unlist(ROCR::performance(pred, measure = "auc")@y.values),3))
    auc.build <- bind_rows(auc.build, auc)
    gains.data.build <- bind_rows(gains.data.build, gains.data)
  }
  no.model.data <- tbl_df(data.frame(Percent.buyers=c(0,100),
                                     Percent.customers=c(0,100)))
  print(ggplot() +
          geom_line(data=gains.data.build,
                    aes(Percent.customers,Percent.buyers,color = Model)) +
          geom_line(data=no.model.data,
                    aes(Percent.customers,Percent.buyers), linetype=3) +
          labs(x="Percent Customers",
               y="Percent Buyers"))
  auc.build <- tbl_df(data.frame(auc.build))
  return(auc.build)
}
