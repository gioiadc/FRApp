#' FRAP analysis dataset
#'
#' The dataset provides 65 measures equally spaced in time over a period of 100 seconds for 132 dendritic spines, for a total of 8580 observations.
#' Data presents a nested hierarchical structure: spines belongs to 54 neurons, grouped in 6 cultures.
#' The number of spines within neurons, and of neurons within cultures is not constant.
#' Experimental condition is applied to 28 neurons (CAP2-shRNA, 71 spines), whereas 26 are controls (SCR, 61 spines).
#' For a comprehensive description of the data and of the data cleaning steps, please refer to the paper below.
#'
#' @format ## `FRAPdata`
#' A data frame with 8580 rows and 6 columns:
#' \describe{
#'   \item{Culture.id}{Culture id, from 1 to 6}
#'   \item{genetic.id}{Experimental condition, SCR for control and SH for CAP2-shRNA}
#'   \item{neuron}{Neuron id, nested within cultures}
#'   \item{spine.ID}{Spine id, nested within neurons}
#'   \item{time}{Time, from 0 to 99.84}
#'   \item{y}{Fluorescence intensities}
#' }
#' @source Nonlinear mixed-effects models to analyze actin dynamics in dendritic spines - Di Credico, Pelucchi, Pauli, Di Luca, Marcello, Edefonti
"FRAPdata"
