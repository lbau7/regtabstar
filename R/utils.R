#' Helper function for updating row names using the labels from a data frame
#'
#' @param rownames string vector with names from the object `coefsm`
#' @param mod A model
#' @template addref
#' @template rowlabs_auto
#' @param covar_names a string vector containing the names of the covariates
#'  in the data set, default is `names(stats::model.frame(mod))[-1]`
#' @param covar_pos an integer vector containing the indexes of the rows
#'  of `coefsm` that contain the covariates
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#'
#' @keywords internal
generate_rowlabs <- function(rownames, mod, addref, rowlabs_auto,
                             covar_names = names(stats::model.frame(mod))[-1],
                             covar_pos){
  # By default, rowlabs as usual
  rowlabs <- rownames
  # Search for covariates with label in the data set rowlabs_auto
  # and replace the variable name in the respective rows
  covar_classes <- sapply(stats::model.frame(mod), class)
  i <- 1
  for(name in covar_names){
    label <- attr(rowlabs_auto[, name], "label")
    j <- 1
    if(covar_classes[name] %in% c("factor", "ordered")){
      j <- length(levels(stats::model.frame(mod)[,name]))
      if(!addref){
        j <- j - 1
      }
    }
    indexes <- covar_pos[(i:(i+j-1))]
    if(!is.null(label)) {
      rowlabs[indexes] <-
        paste0(gsub(pattern = paste0("^", name),
                    replacement = paste(label,"("),
                    x = rowlabs[indexes]), ")")

    }
    i <- i + j
  }
  # Remove empty brackets
  rowlabs <- gsub(pattern = " \\(\\)$", replacement = "", rowlabs)
}
