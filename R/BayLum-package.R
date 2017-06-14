#' @title Chronological Bayesian Models Integrating Optically Stimulated Luminescence Dating
#'
#' @description A collection of various R functions for Bayesian analysis of Luminescence data.
#' This includes, amongst others, data import, export, application of age models and palaeodose model.
#'
#' @name BayLum-package
#' @aliases BayLum-package BayLum
#' @docType package
#' @author Claire Christophe, Guillaume Guerin, Sebastian Kreutzer, Anne Philippe
#' @keywords package
#' @details This package is based on the functions \code{\link{Generate_DataFile}}, \code{\link{Generate_DataFile_MG}},
#' \code{\link{Age_Computation}} and \code{\link{AgeS_Computation}}.
#' The two first functions create a list containing all informations needed to analyse each Bin file in order to compute its age,
#' of Single-grain OSL measurements for the first function and Multi-grain OSL measurements for the second.
#' The another two functions use Bayesian analysis for OSL age estimation for one or various samples,
#' according differents models that are available in options
#' (different growth curves and different equivalent dose distributions around the palaeodose).
#'
#' It is possible to consider various Bin files per sample,
#' and to compute ages of samples in stratigraphic contraints and to integrate systematic errors.
#'
#' @import utils stats methods graphics grDevices coda Luminescence ArchaeoPhases rjags
NULL
