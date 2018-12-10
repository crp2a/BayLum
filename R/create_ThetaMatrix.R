#' @title Create Theta Matrix
#'
#' @description Create the \eqn{\Theta} matrix with the shared uncertainties
#' that can used as input in, e.g., [AgeS_Computation] and [Age_OSLC14] which is used for the
#' covariance matrix \eqn{\Sigma} (Combès \& Philippe, 2017)
#'
#' @details The function intends to ease the creation of the \eqn{Theta} matrix, which cannot be
#' created with the base R function [stats::cov]. The relationship between the covariance matrix
#' \eqn{Sigma} and \eqn{Theta} is defined as
#'
#' \deqn{\Sigma_ij = A_i * A_j * \Theta_ij}
#'
#' For details see Combès \& Philippe, 2017 and Guérin et al. (under review).
#'
#' **Systematic uncertainties**
#'
#' The following table provides informaton on the named argument
#' that can be provided via the argument `sigma_s`
#'
#' \tabular{lll}{
#' ARGUMENT \tab DESCRIPTION \tab UNIT \cr
#' `s_K` \tab relative uncertainty K concentration \tab - \cr
#' `s_U` \tab relative uncertainty U concentration \tab - \cr
#' `s_Th` \tab relative uncertainty Th concentration \tab - \cr
#' `s_gammaDR` \tab relative uncertainty gamma-dose rate  \tab - \cr
#' `s_CAL` \tab relative uncertainty beta-source calibration \tab - \cr
#' `s_intDR` \tab absolute uncertainty internal dose rate \tab Gy/ka \cr
#' }
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France), based
#' on an 'MS Excel' sheet by Guillaume Guérin, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)
#'
#' @section Function version: 0.1.0
#'
#' @param input [character] or [data.frame] (*optional*): input data frame or file connection
#' to import a CSV-file with the needed information. If nothing is provided the function returns
#' an input template. The argument `output_file` can be used to write this input template to the file
#' system
#'
#' @param output_file [character] (optional): file path for the output CSV-file, the field separator
#' is hard set to `","`. Please use [utils::write.table] for more flexibility.
#'
#' @param sigma_s [numeric] (with default): named character with values for systematic uncertainties, the order
#' of the *named* vector is not important, but the naming! **Note**: some of the uncertainties have
#' a unit, please check details.
#'
#' @param ... further arguments that can be passed to [utils::read.table] (for the CSV-file import)
#'
#' @seealso [AgeS_Computation], [Age_OSLC14], [utils::read.table], [utils::write.table]
#'
#' @return A symetric \eqn{Theta} matrix or if `input` is missing, a [data.frame] with an input
#' template
#'
#' @references
#'
#' Combès, B., Philippe, A., 2017. Bayesian analysis of individual and systematic multiplicative errors
#' for estimating ages with stratigraphic constraints in optically stimulated luminescence dating.
#' Quaternary Geochronology 39, 24–34. \doi{10.1016/j.quageo.2017.02.003}
#'
#' @examples
#' ##(1) return template data.frame
#' create_ThetaMatrix()
#'
#' @keywords datagen IO
#'
#' @md
#' @export
create_ThetaMatrix <- function(
  input,
  output_file = NULL,
  sigma_s =  c(
    s_K = 0.010,
    s_U = 0.007,
    s_Th = 0.006,
    s_gammaDR = 0.05,
    s_CAL = 0.020,
    s_intDR = 0.030),
  ...
  ){

  # Verify input --------------------------------------------------------------------------------
  # basic input
  if(missing(input)){

    #set names
    df_colnames <-  c("SAMPLE_ID",
    "DR_BETA_K",
    "DR_BETA_U",
    "DR_BETA_TH",
    "DR_GAMMA",
    "DR_TOTAL",
    "DR_TOTAL_X")

    #set data.frame
    df <- as.data.frame(matrix(NA_real_, ncol = length(df_colnames)))
    colnames(df) <- df_colnames

    #return
    if(!is.null(output_file)){
      message("[create_ThetaMatrix()] 'input' missing, template data.frame returned and exported to ", output_file)
      write.table(x = df, file = output_file, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")

    }else{
      message("[create_ThetaMatrix()] 'input' missing, input template returned!")

    }

    ##return
    return(df)

  }else if(class(input) == "data.frame"){
     df <- input

  }else{
    if(!file.exists(file))
      stop(paste0("[create_ThetaMatrix()] File ", file, "does not exist!"), call. = FALSE)

    ##extract extra parameters
    import_settings <- modifyList(
      x = list(
        header = TRUE,
        skip = 0,
        sep = ","

        ),
      val = list(...))

    ##Import
    df <- read.table(
      file = file,
      header = import_settings$header,
      sep = import_settings$sep,
      skip = import_settings$skip,
      row.names = FALSE)

  }

  ##check sigma_s
  ##set reference
  sigma_s_ref <- c("s_K", "s_U", "s_Th", "s_gammaDR", "s_CAL", "s_intDR")

  if(!is.null(sigma_s) && !all(names(sigma_s) %in% sigma_s_ref))
      stop("[create_ThetaMatrix()] Value names do not match in 'sigma_s', please check the manual!", call. = FALSE)

  ##set NULL case
  if(is.null(sigma_s)){
    sigma_s <- numeric(length(sigma_s_ref))
    names(sigma_s) <- sigma_s_ref

  }

  #verify data.frame, we make a hard stop here
  df_names_ref <- c("SAMPLE_ID", "DR_BETA_K", "DR_BETA_U", "DR_BETA_TH", "DR_GAMMA", "DR_TOTAL", "DR_TOTAL_X")

  if(!all(colnames(df) %in% df_names_ref))
    stop("[create_ThetaMatrix()] The input data.frame has not the expected columns, please check the manual!",call. = FALSE)

  #if data.frame has only one row, it cannot work either
  if(nrow(df) < 2)
    stop("[create_ThetaMatrix()] The input data.frame has to contain at least 2 rows!", call. = FALSE)

  # Create matrix -------------------------------------------------------------------------------
  ##create and set names
  m <- matrix(data = NA, nrow = nrow(df), ncol = nrow(df))
  rownames(m) <- df[[1]]
  colnames(m) <- df[[1]]

  ##transform to matrix
  df <- as.matrix(df[,-1])

  ##get possible combinations and add the 1:1 combination
  cmb <- cbind(
    combn(x = 1:nrow(df), m = 2),
    matrix(rep(1:nrow(df),each = 2), nrow = 2))


  ##fill matrix - if you want to add further uncertainties this calculations needs to be
  ##modified
  for(j in 1:ncol(cmb)){
    ##fill the diagonal
    if(cmb[,j][1] == cmb[,j][2]){
      m[cmb[, j][1], cmb[, j][2]] <- df[cmb[,j][1],"DR_TOTAL_X"]^2 + (df[cmb[,j][1],"DR_TOTAL"] * sigma_s["s_CAL"])^2

    }else{
      ##fill the rest of the matrix
      m[cmb[, j][1], cmb[, j][2]] <-
        sum(df[cmb[, j][1], -ncol(df)] * df[cmb[, j][2], -ncol(df)] *
              sigma_s[-which(names(sigma_s) == "s_intDR")] ^ 2) +
        sigma_s["s_intDR"] ^ 2

    }

  }

  ##mirror data along diagonal to get a symmetric matrix
  m[lower.tri(m)] <- t(m)[lower.tri(m)]


  # Return --------------------------------------------------------------------------------------
  if(!is.null(output_file)){
    message("[create_ThetaMatrix()] Theta matrix exported to ", output_file)
    write.table(x = m, file = output_file, append = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")

  }

  ##in either case return matrix
  return(m)

}


