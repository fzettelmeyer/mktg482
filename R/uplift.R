#' A function to compute the performance of an uplift estimator.
#'
#' Adapted from QiniTable in the tools4uplift package
#' by Florian Zettelmeyer to ensure that the n-tiles are calculated
#' based on the treatment observations, not the stacked observations..
#' @param data: a data frame containing the treatment, the outcome and the predictors.
#' @param treat: name of a binary (numeric) vector representing the treatment assignment (coded as 0/1)
#' @param outcome: name of a binary response (numeric) vector (coded as 0/1)
#' @param prediction: a score to sort the observations from highest to lowest uplift or propensity.
#' @param nb.group: (optional, default = 10)
#' @export
#' @examples
#' QiniTable(
#'  expdata_stacked.test,
#'  treat = "ad",
#'  outcome = "converted",
#'  prediction = "score_logitFit_treat",
#'  nb.group = 20
#')

QiniTable <- function(data, treat, outcome, prediction, nb.group = 10){

   # Computes the performance of an uplift estimator.
   # 11-10-2019: Adapted from QiniTable in the tools4uplift package
   # by Florian Zettelmeyer to ensure that the n-tiles are calculated
   # based on the treatment observations, not the stacked observations.
   #
   # Args:
   #   data: a data frame containing the treatment, the outcome and the predictors.
   #   treat: name of a binary (numeric) vector representing the treatment
   #          assignment (coded as 0/1).
   #   outcome: name of a binary response (numeric) vector (coded as 0/1).
   #   prediction: a predicted uplift to sorts the observations from highest
   #               to lowest uplift.
   #   ... and default parameters.
   #
   # Returns:
   #   The performance of an uplift estimator in a table.

   # Error handling
   if (nb.group < 2) {
      stop("The number of groups must be greater or equal to 2")
   }

   # First, we need to rank and sort the observations
   df <- data[data[[treat]] == 1, ]
   breaks <- c(-Inf, quantile(df[[prediction]], probs = seq(0, 1, by = 1/nb.group))[2:nb.group],Inf)
   data$group <- nb.group+1 - cut(data[[prediction]], breaks,
                                  labels = FALSE,include.lowest = TRUE)

   dataResults <- data.frame(matrix(rep(0), nb.group, 8))
   colnames(dataResults) <- c("cum_per", "T_Y1", "T_n", "C_Y1",
                              "C_n", "incremental_Y1", "inc_uplift", "uplift")

   # Incremental observed uplift
   for(i in 1:nb.group){
      subset <- data[data$group <= i, ]
      dataResults[i,1] <- i/nb.group
      dataResults[i,2] <- sum(subset[[treat]] == 1 & subset[[outcome]] == 1)
      dataResults[i,3] <- sum(subset[[treat]] == 1)
      dataResults[i,4] <- sum(subset[[treat]] == 0 & subset[[outcome]] == 1)
      dataResults[i,5] <- sum(subset[[treat]] == 0)
      dataResults[i,6] <- dataResults[i, 2] - dataResults[i, 4]*dataResults[i, 3]/dataResults[i, 5]
   }
   dataResults[,7] <- dataResults[,6]/dataResults[nb.group,3]*100


   # Observed uplift in each group
   for (i in 1:nb.group){
      subset <- data[data$group == i, ]
      dataResults[i,8] <- sum(subset[[treat]] == 1 & subset[[outcome]] == 1) / sum(subset[[treat]] == 1) -
         sum(subset[[treat]] == 0 & subset[[outcome]] == 1) / sum(subset[[treat]] == 0)
   }

   return(dataResults)
}


#' A function to plot a Qini Bar Plot for two models.
#'
#' This function allows you to compare model performace by comparing the Qini Bar Plot of two models.
#' @param table1, table2, (exactly)
#' @param modelnames = c("model1", "model2") (optional)
#' @export
#' @examples
#' QiniBarPlot(PerfTable_uplift, PerfTable_propensity, modelnames = c("Logit Uplift", "Logit Propensity"))

QiniBarPlot <- function(...,modelnames=NULL) {
   arglist <- list(...)

   # Assemble the dataframe
   combined_table=arglist[[1]][,c("cum_per","uplift")]
   if (length(arglist) > 1) {
      for (i in 2:length(arglist)) {
         temp2 = arglist[[i]][, c("uplift")]
         combined_table = cbind(combined_table, temp2)
      }
   }

   # Adjust names and add modelnames if they have been specified
   if (is.null(modelnames) == TRUE) {
      if (length(arglist) > 1) {
         for (i in 2:length(match.call())) {
            names(combined_table)[i] <- deparse(match.call()[[i]])
         }
      }
   } else {
      names(combined_table) <- c("cum_per", modelnames)
   }

   # Plot uplift graph
   plot <- combined_table %>%
      gather(key = "model", value = "uplift", -cum_per) %>%
      mutate(cum_per = cum_per * 100, uplift= uplift*100) %>%
      rename(Model=model) %>%
      ggplot() + geom_col(aes(x=cum_per, y = uplift, fill = Model), position="dodge") +
      xlab("Proportion of Population Targeted") + ylab("Uplift (%)")
   print(plot)
}


   #' A function to plot a Qini curve for two models.
   #'
   #' This function allows you to compare model performace by comparing the Qini curves of two models.
   #' @param table1, table2, (exactly)
   #' @param modelnames = c("model1", "model2") (optional)
   #' @export
   #' @examples
   #' QiniCurve2(PerfTable_uplift, PerfTable_propensity, modelnames = c("Logit Uplift", "Logit Propensity"))

QiniCurve <- function(...,modelnames=NULL) {
   arglist <- list(...)

   notargeting_line <- data.frame(x=0, xend=100,
                                  y=0, yend=arglist[[1]][nrow(arglist[[1]]),7])

   # Assemble the dataframe
   combined_table=arglist[[1]][,c("cum_per","inc_uplift")]
   if (length(arglist) > 1) {
      for (i in 2:length(arglist)) {
         temp2 = arglist[[i]][, c("inc_uplift")]
         combined_table = cbind(combined_table, temp2)
      }
   }

   # Adjust names and add modelnames if they have been specified
   if (is.null(modelnames) == TRUE) {
      if (length(arglist) > 1) {
         for (i in 2:length(match.call())) {
            names(combined_table)[i] <- deparse(match.call()[[i]])
         }
      }
   } else {
      names(combined_table) <- c("cum_per", modelnames)
   }

   # Plot uplift graph
   plot <- combined_table %>%
      rbind(c(0,0), .) %>%
      gather(key = "model", value = "inc_uplift", -cum_per) %>%
      mutate(cum_per = cum_per * 100) %>%
      rename(Model=model) %>%
      ggplot() + geom_line(aes(x=cum_per, y = inc_uplift, color = Model))+
      xlab("Proportion of Population Targeted") + ylab("Incremental Uplift (%)") +
      geom_segment(data =notargeting_line, aes(x=x, y=y, xend=xend, yend=yend), linetype="dotted")
   print(plot)
}
