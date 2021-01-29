#### vaccine module

# Goals: give certain nodes attributes related to vaccination
# sources: https://www.nytimes.com/2020/11/18/health/pfizer-covid-vaccine.html
# https://www.cdc.gov/csels/dsepd/ss1978/lesson3/section6.html
# ---- basic components
# assign initial attributes
# complete vaccination provides 95% reduction in cases among vaccinated
# generate infection risk reduction factor = f*e*d*v
# ---- additional items
# targeted vaccination
# partial vaccination
# how does vaccination interact with testing
# how does vaccination interact with transmission if infection occurrs
## currently assume no impact on transmission
# confidence intervals


#' Vaccination Class
#'
#' A default class to implement vaccination policies.
#' A graph of n nodes with a \code{$infProbReduction} property have based on the vaccine being applied. 
#' When \code{$donext} \code{$infProbReduction} property is updated based on range in confidence intervals for each
#' method. 
#'
#' @field vaxEff percent reduction in probability of covid infection if vaccinated
#' @field vaxEffCI Confidence interval for vaccine efficacy
#' @field propVax proportion of individuals initially vaccinated
#' @filed vaxRate how many individuals are vaccinated per day
#'
#' @export default_ppe
default_vax <- setRefClass(
  "vax",
  fields = list(
    vaxEff="numeric",
    propVax="numeric",
    # vaxEffCI="numeric",
    vaxRate="numeric"
  ),
  methods = list(
    init = function(g) {
      "Set reduction in probability of infection given vax"
      igraph::V(g)$vaxProtect <- c(rep(1-vaxEff, igraph::vcount(g)*propVax), rep(1, igraph::vcount(g)-(igraph::vcount(g)*propVax))) %>%
        sample()
      
      "Set infection probability reduction given vaccination"   
      if("infProbReduction" %in% igraph::vertex_attr_names(g)){
        igraph::V(g)$infProbReduction <- igraph::V(g)$infProbReduction*igraph::V(g)$vaxProtect
      } else {
        igraph::V(g)$infProbReduction <- igraph::V(g)$vaxProtect
      }
      return(g)
    },
    donext = function(g) {
      "vaccinate additional people"
      #filter for unvaccinated people
      unVax <- igraph::V(g)[vaxProtect == 1]
      
      if(vaxRate < 1){
        vaxRate <- rbinom(1,1,prob = vaxRate)
      } 
      
      if(vaxRate > igraph::vcount(g)){
        #print("so much vaccine!")
        #browser()
        igraph::V(g)[unVax]$vaxProtect <- rep(1-vaxEff,length(unVax))

      } else {
      
      igraph::V(g)[unVax]$vaxProtect <- c(rep(1-vaxEff,vaxRate),rep(1,vcount(g)-vaxRate)) %>% sample()  
      }
      #vaccinate some number of individuals - give them vaxProtect attribute
      
   
        vaxed <- igraph::V(g)[vaxProtect != 1]
          "Infection probability is reset in ppe module so must be re-established here"
        igraph::V(g)[vaxed]$infProbReduction <- igraph::V(g)[vaxed]$infProbReduction*igraph::V(g)[vaxed]$vaxProtect
      
      return(g)
    }
  )
)
