#' Create age plot
#'
#' This function creates an age plot showing the mean ages and along with the credible intervals.
#'
#' @param object [list] (**required**): Output as created by functions like [AgeC14_Computation].
#'
#' @param ... further arguments to control the plot output
#'
#' @return
#' A plot
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France), based on code
#' written by Claire Christophe
#'
#' @seealso [AgeC14_Computation]
#'
#' @examples
#'##TODO
#'
#' @md
#' @export
plot_Ages <- function(
  object,
  ...
){


  # Verify input --------------------------------------------------------------------------------
  if (is.null(attributes(object)$class) || attributes(object)$class != "BayLum.list")
    stop("[plot_Ages()] Wrong input, only objects of type 'BayLum.list' are allowed. Please check the manual!",
      call. = FALSE
    )



}
