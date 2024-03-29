#' Takes multiply imputed datasets (as generated by the `extract_MIdata()` function)
#' and runs an analysis function on each of them.
#'
#' @param formula a two sided model formula (see \code{\link[stats]{formula}}).
#' @param family only for \code{glm}: a description of the distribution and link
#'               function to be used in the model. This can be a character string
#'               naming a family function, a family function or the result of a
#'               call to a family function. (For more details see below and
#'               \code{\link[stats]{family}}.)
#' @param data the output object of `extract_MIdata()` function.
#' @param pool logical. If TRUE, estimates from each imputed data set will be
#'             pooled together according to Rubin's rules. Default is TRUE.
#'
#' @returns A list containing
#' - list of estimated coefficients and standard error from each imputed data.
#' - pooled estimates based Rubin's rule if `pool = TRUE`.
#'
#' @details `rubin_rules` applies Rubin's rules (Rubin, 1987) for pooling together
#' the results from a multiple imputation procedure. The pooled point `Estimate` is
#' is the average across the point estimates from the complete-data analyses.
#' The `SE` is the square-root of the sum of two terms representing the within-variance
#' and the between-variance (see Little-Rubin (2002)). The function also returns
#'  95% confidence interval, based on the estimated pooled degrees of freedom
#' that can be used for inference based on the t-distribution.
#'
#' @export

miAnalyze = function(formula, family=NULL, data, pool=TRUE){
  if (!inherits(data, "remiod")) stop("Use only with 'remiod' objects.")

  models = data$models
  data = data$midata

  imps = max(data$Imp_)
  res = lapply(1:imps, function(i) {
    if (is.null(family)) {
      allvar = all.vars(formula)
      y = allvar[attr(terms(formula), "response")]
      if (models[y]=="clm" || ("ordered" %in% class(data[,y]))) {
        md = ordinal::clm(formula, data=subset(data, Imp_==i))
        out= cbind(Estimate=coef(md), SE= sqrt(diag(vcov(md))))
      }
      else msg("Please specify ")
    }else {
      md = glm(formula, data=subset(data, Imp_==i), family=family)
      out = stats::summary.glm(md)$coefficients
    }
    out
  }
  )
  if(pool){
    estm = do.call(cbind, lapply(res, function(resi) resi[,1]) )
    estv = do.call(cbind, lapply(res, function(resi) resi[,2]^2) )
    est = apply(estm, 1, mean)
    vw =  apply(estv, 1, mean)
    vb =  apply(estm, 1, var)

    m = ncol(estm)
    vt = vw + vb + vb/m
    df = (m - 1) * ((1 + vw/(vb + vb/m))^2)
    t = qt(.975, df)
    out = data.frame(Estimate=est, SE=sqrt(vt), CI.low=est - t*sqrt(vt), CI.up=est + t*sqrt(vt),
                     t = est/sqrt(vt), p.value=pt(est/sqrt(vt), df, lower.tail = FALSE))
  }
  return(list(Est.sep = res, Est.pool = out))
}
