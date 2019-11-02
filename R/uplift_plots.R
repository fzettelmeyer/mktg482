#' A function to plot a Qini Bar Plot for two models.
#'
#' This function allows you to compare model performace by comparing the Qini Bar Plot of two models.
#' @param table1, table2, (exactly)
#' @param modelnames = c("model1", "model2") (optional)
#' @export
#' @examples
#' QiniBarPlot2(PerfTable_uplift, PerfTable_propensity, modelnames = c("Logit Uplift", "Logit Propensity"))

QiniBarPlot2 <- function(table1, table2, modelnames = c("model1", "model2")){

   nb.table1 <- length(table1$cum_per)
   nb.table2 <- length(table2$cum_per)

   if (nb.table1!=nb.table2)
      stop("The two tables have a different number of rows/groups")

   combined_table <- cbind(table1[,c("cum_per","uplift")], tempname = table2[,c("uplift")])
   colnames(combined_table)[2] <- modelnames[[1]]
   colnames(combined_table)[3] <- modelnames[[2]]
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

QiniCurve2 <- function(table1, table2, modelnames = c("model1", "model2")){

   nb.table1 <- length(table1$cum_per)
   nb.table2 <- length(table2$cum_per)

   if (nb.table1!=nb.table2)
      stop("The two tables have a different number of rows/groups")

   notargeting_line <- data.frame(x=0, xend=100,
                                  y=0, yend=table1[nrow(table1),7])

   combined_table <- cbind(table1[,c("cum_per","inc_uplift")], tempname = table2[,c("inc_uplift")])
   colnames(combined_table)[2] <- modelnames[[1]]
   colnames(combined_table)[3] <- modelnames[[2]]

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
