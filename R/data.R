#' FRAP analysis dataset
#'
#' The dataset provides 65 measures of fluorescence intensity equally spaced in time over a period of 100 seconds for 132 dendritic spines, for a total of 8580 observations.
#' Data presents a nested hierarchical structure: spines belongs to 54 neurons, grouped in 6 cultures.
#' The number of spines within neurons, and of neurons within cultures is not constant.
#' Experimental condition is applied to 28 neurons (CAP2-shRNA, 71 spines), whereas 26 are controls (SCR, 61 spines).
#' The dataset is a subset of the original version where spines with fluorescence intensity larger than 0.6 at time 0 have been excluded.
#' For a comprehensive description of the data and of the data cleaning steps, please refer to the paper below.
#'
#' @format ## `FRAPdata`
#' A data frame with 8580 rows and 6 columns:
#' \describe{
#'   \item{Culture.id}{Culture id, from 1 to 6}
#'   \item{genetic.id}{Experimental condition, SCR for control and SH for CAP2-shRNA}
#'   \item{neuron}{Neuron id and experimental condition, nested within cultures}
#'   \item{spine.ID}{Spine id, nested within neurons}
#'   \item{time}{Time, from 0 to 99.84}
#'   \item{y}{Fluorescence intensities}
#' }
#' @source Di Credico, G., Pelucchi, S., Pauli, F. et al. Nonlinear mixed-effects models to analyze actin dynamics in dendritic spines. Sci Rep 15, 5790 (2025). https://doi.org/10.1038/s41598-025-87154-w
"FRAPdata"
