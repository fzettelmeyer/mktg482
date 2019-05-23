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
