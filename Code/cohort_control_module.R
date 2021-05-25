# Cohort Control Model
# Goals: Govern how cohorting affects nodes. Will decide which group is in or out of the office.
# ---- daily components
# Shift length - how many days in a row will a cohort occupy the office 
# ---- hour components
# Shift length - how many hours is each shift
# Frequency - instances per week shifts occur 
# buffer - avoid having sessions on the same day
# mitigation_buffer - time between shifts that allows for virus to be removed
# ---- additional items
# groups per shift 
# understand which groups can be in the office together without increasing
# contacts

#' Cohort Control Class
#'
#' A default class to control cohort actions.
#' A graph of n nodes with a \code{$cohort} property. 
#' 
#' When \code{$donext} is called 1 is added to the \code{$inOfficeCounter} property and 
#' remote status is evaluated.
#' If \code{$reported} is false and \code{$inOfficeCounter} equal to or
#' greater than \code{leaveDuration} and \code{$infected} is 2, 
#' the \code{$infect} attribute is set to 2 or "recovered". 
#'
#' @field shift_length Number time steps (days) a cohort is in the office
#' @field starting_cohort The cohort that will be initially in the office
#'
#' @export cohort_controls
cohort_controls <- setRefClass(
  "corhortControls",
  fields = list(
    shift_length ="numeric",
    starting_cohort = "numeric"
  ),
  methods = list(
    init = function(g) {
      
      "Set remote status"
      igraph::V(g)$remote <- TRUE
      "Set in office counter"
      igraph::V(g)$inOfficeCounter <- 0
      
      "Initiate starting group in the office"
      
      "Set remote status"
      igraph::V(g)$remote <- ifelse(igraph::V(g)$cohort == starting_cohort,FALSE, igraph::V(g)$remote)
        
      "Set in office counter"
      igraph::V(g)$inOfficeCounter <- ifelse(igraph::V(g)$cohort == starting_cohort,1, igraph::V(g)$inOfficeCounter)
        
      return(g)
    },
    donext = function(g) {
      
      "check if in office group has reached shift length"
      if(max(igraph::V(g)$inOfficeCounter) == shift_length){
        
        "get cohorts"
        cohorts <- unique(igraph::V(g)$cohort)
        
        "get current in office cohort "
        inOfficeCohort <- unique(igraph::V(g)[igraph::V(g)$remote == TRUE]$cohort)
        
        if(length(inOfficeCohort)> 2){
          stop("multiple offices ")
        }
        
        "shift one index position down cohorts or start from the beginning"
        if((inOfficeCohort + 1) < length(cohorts)){
          current_inOffice_cohort <- inOfficeCohort + 1
        } else {
          current_inOffice_cohort <- starting_cohort
        }
        
        "change groups & reset counters, else continue"
        igraph::V(g)$remote <- ifelse(igraph::V(g)$cohort == current_inOffice_cohort,TRUE, FALSE)
        
        "reset office counter"
        igraph::V(g)$inOfficeCounter <- ifelse(igraph::V(g)$cohort == current_inOffice_cohort,1,0)
        
        
      } else {
        "increment office counter for in office group"
      }
      

      
      
      
      
      "Increments counter and computes whether a nodes state should be moved to recovered"
      "Not a duplicate of whats happening in the recovery module because conditional statements are different"
      igraph::V(g)[infected==3]$counter <- igraph::V(g)[infected==3]$counter + 1
      # Recover infected nodes
      ## Infected nodes have a probability of infected days/20 to recover
      leaveNodes <- igraph::V(g)[infected==3]
      propRecover <- leaveNodes$counter/max_recovery_time
      rollDice <- runif(length(leaveNodes))
      # Update recovered nodes
      igraph::V(g)[leaveNodes]$recovered <- rollDice < propRecover
      igraph::V(g)[recovered]$infected <- 2
      igraph::V(g)[recovered]$color <- "green"
      
      "set leave status based on whether or not the covid case was reported or if they have passed through the leave period"
      igraph::V(g)$leave <- ifelse(igraph::V(g)$reported == 1 | (igraph::V(g)$leaveCounter <= leaveDuration & igraph::V(g)$leaveCounter > 0) ,TRUE, FALSE)
      
      "add a day to the leaveCounter"
      igraph::V(g)$leaveCounter <- ifelse(igraph::V(g)$leave == TRUE, igraph::V(g)$leaveCounter + 1,igraph::V(g)$leaveCounter) 
      
      "set infection status to Leave"
      igraph::V(g)$infected <- ifelse(igraph::V(g)$leave == TRUE, 3,igraph::V(g)$infected)
      
      "Set leave color"
      igraph::V(g)$color <- ifelse(igraph::V(g)$leave == TRUE,"yellow", igraph::V(g)$color)
      
      return(g)
    }
  )
)
