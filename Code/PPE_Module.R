# Personal Protective Equipment Module
# Goals: give certain nodes attributes related to personal PPE (googles, masks, gloves)
# sources: https://www.thelancet.com/action/showPdf?pii=S0140-6736%2820%2931142-9
# ---- basic components
# faceCovering = Odds Ratio =  0.15
# eye protection = O.R. = 0.22
# distancing = O.R. = 0.18
# generate infection risk reduction factor = f*e*d
# ---- additional items
# compliance with each policy
# confidence intervals


#' Personal Protective Equipment Class
#'
#' A default class to implement Personal Protective Equipment - ppe - policies.
#' A graph of n nodes with a \code{$infProbReduction} property have based on ppe applied. 
#' When \code{$donext} \code{$infProbReduction} property is updated based on range in confidence intervals for each
#' method. 
#'
#' @field faceCovering estimated effect of covid infection with face covering use
#' @field eyeProtection estimated effect of covid infection with eye protection use
#' @field distancing estimated effect of covid infection with physical distancing
#' @field compliance percent of population complying with policy
#' @field faceCI Confidence interval for faceCovering odds ratio
#' @field eyeCI Confidence interval for eyeProtection odds ratio
#' @field distCI Confidence interval for distancing odds ratio
#'
#' @export default_ppe
default_ppe <- setRefClass(
  "ppe",
  fields = list(
    faceCovering="numeric",
    eyeProtection="numeric",
    distancing="numeric",
    compliance="numeric"
    # faceCI="numeric",
    # eyeCI="numeric",
    # distCI="numeric"
  ),
  methods = list(
    init = function(g) {
      "Set infection probability reduction given ppe"
      igraph::V(g)$infProbReduction <- c(rep((faceCovering*eyeProtection*distancing), igraph::vcount(g)*compliance), rep(1, igraph::vcount(g)-(igraph::vcount(g)*compliance))) %>%
        sample()
      return(g)
    },
    donext = function(g) {
      igraph::V(g)$infProbReduction <- c(rep((faceCovering*eyeProtection*distancing), igraph::vcount(g)*compliance), rep(1, igraph::vcount(g)-(igraph::vcount(g)*compliance))) %>%
        sample()
      return(g)
    }
  )
)
