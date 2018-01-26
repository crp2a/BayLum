#' @title Chronological Bayesian Models Integrating Optically Stimulated Luminescence and 14C Dating
#'
#' @description A collection of various R functions for Bayesian analysis of luminescence data and 14C age estimates.
#' This includes, amongst others, data import, export, application of age and palaeodose models.
#'
#' @name BayLum-package
#' @aliases BayLum-package BayLum
#' @docType package
#' @author Claire Christophe, Anne Philippe, Sebastian Kreutzer, Guillaume Guérin
#' @keywords package
#' @details This package is based on the functions: [Generate_DataFile] and [Generate_DataFile_MG]
#' to import luminisecence data. These functions create a list containing all informations to compute age
#' of Single-grain OSL measurements for the first function and Multi-grain OSL measurements for the second.
#'
#' The functions: [Age_Computation] and [AgeS_Computation] use Bayesian analysis for OSL age
#' estimation for one or various samples according to differents models
#' (e.g. different dose-response curves and different equivalent dose distributions around the palaeodose).
#'
#' It is possible to consider various BIN-files per sample, to compute ages of samples in stratigraphic constraints
#' and to integrate systematic errors.
#'
#' It is possible to calibrate 14C age with the function [AgeC14_Computation].
#' We can also estimate chronology containing 14C age and OSL samples with the function [Age_OSLC14].
#'
#' @md
#' @import utils stats methods graphics grDevices coda
NULL
