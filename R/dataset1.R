#' Data Set 1 Chemical Mixture Simulated Data
#'
#' These synthetic data can be considered as the results of a prospective cohort epidemiologic study. The outcome cannot cause the exposures (as might occur in a cross-sectional study). Correlations between exposure variables can be thought of as caused by common sources or modes of exposure. The nuisance variable Z can be assumed to be a potential confounder and not a collider.
#'
#'Additional information: For purposes of Data Set #1, there is no loss to follow up, missing or censored data, mismeasurement of the variables (Y, Xi, Z), or many of the other potential biases. One may also assume that the seven exposure variables X1, X7 and Z are not intermediate variables and not colliders. There are no other confounders or effect measure modifiers. Random noise has been added to the outcome variable.
#'
#' @format A data frame with 500 rows and 10 variables:
#' \describe{
#'   \item{OBS}{subject identifier}
#'   \item{Y}{The outcome, a continuous variable}
#'   \item{X1}{a continous exposure variable}
#'   \item{X2}{a continous exposure variable}
#'   \item{X3}{a continous exposure variable}
#'   \item{X4}{a continous exposure variable}
#'   \item{X5}{a continous exposure variable}
#'   \item{X6}{a continous exposure variable}
#'   \item{X7}{a continous exposure variable}
#'   \item{Z}{a binary confounder}
#'   ...
#' }
#' @source \url{https://www.niehs.nih.gov/about/events/pastmtg/2015/statistical/index.cfm}
"dataset1"
