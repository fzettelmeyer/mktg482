#' A function to determine which coefficients can be estimated by a partial factorial design.
#' @param full.design (required)
#' @param frac.design (required)
#' @keywords doe, multivariate testing
#' @export
#' @examples
#' library(AlgDesign)
#' my.exp.design <- list( Shape=c("Rectangle", "Square"), Color=c("Blue", "Red", "Yellow"), DiscountPct=c("10", "20", "30"))
#' my.full.design <- expand.grid(my.exp.design) #gen.factorial(my.exp.design, factors="all")
#' my.frac.design <- optFederov(data=my.full.design, nTrials=6)  # 6 is minumum so main effects only
#' get.estimable.coefficients(my.full.design, my.frac.design)

get.estimable.coefficients <- function(full.design, frac.design){
	#colnames(full.design) <- paste0(colnames(full.design),".")
	#colnames(frac.design$design) <- paste0(colnames(frac.design$design),".")

	fm <- formula(paste("y~",paste(colnames(full.design), collapse="*")))
	y <- 1:nrow(full.design)
	lm.full <- lm(fm, data=full.design)
	coef.full <- coef(lm.full)

	y <- 1:nrow(frac.design$design)
	lm.frac <- lm(fm, data=frac.design$design)
	coef.frac <- coef(lm.frac)[!is.na(coef(lm.frac))]

	return(list( Estimable=names(coef.frac)[-1],
				 Inestimable=setdiff(names(coef.full), names(coef.frac)) ))
}

#' A function to determine the sample size required to estimate a proportion given a desired confidence level and interval half width
#' @param halfwidth (required)
#' @param confidence (optional with default of .95)
#' @param proportion (optional with default of .5)
#' @keywords sample size calcuation
#' @export
#' @examples
#' ss.proportion(0.03)
#' ss.proportion(0.03, confidence=0.99)
#' ss.proportion(0.03, confidence=0.99, proportion=.1)

ss.proportion <- function(halfwidth, confidence=0.95, proportion=0.5){
  if(halfwidth <= 0){ return("Halfwidth must be greater than 0.") }
  if(confidence <= 0 | confidence >= 1){ return("Confidence must be between 0 and 1.") }
  if(proportion <= 0 | proportion >= 1){ return("Proportion must be between 0 and 1.") }
  z <- abs(qnorm((1 - confidence) / 2))
  pq <- proportion * (1-proportion)
  return(ceiling(z^2 * pq / halfwidth^2))
}

#' A function to determine the sample size required to estimate the difference of two proportions given a desired confidence level and interval half width
#' @param halfwidth (required)
#' @param confidence (optional with default of .95)
#' @param proportion1 (optional with default of .5)
#' @param proportion2 (optional with default of .5)
#' @keywords sample size calcuation
#' @export
#' @examples
#' ss.2proportion(0.03)
#' ss.2proportion(0.03, confidence=0.99)
#' ss.2proportion(0.03, confidence=0.99, proportion1=.1, proportion1=.2)

ss.2proportion <- function(halfwidth, confidence=0.95, proportion1=0.5, proportion2=0.5){
  if(halfwidth <= 0){ return("Halfwidth must be greater than 0.") }
  if(confidence <= 0 | confidence >= 1){ return("Confidence must be between 0 and 1.") }
  if(proportion1 <= 0 | proportion1 >= 1){ return("Proportions must be between 0 and 1.") }
  if(proportion2 <= 0 | proportion2 >= 1){ return("Proportions must be between 0 and 1.") }
  z <- abs(qnorm((1 - confidence) / 2))
  pq1 <- proportion1 * (1-proportion1)
  pq2 <- proportion2 * (1-proportion2)
  return(ceiling(z^2 * (pq1 + pq2) / halfwidth^2))
}
