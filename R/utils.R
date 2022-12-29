#' Helper function for updating row names using the labels from a data frame
#'
#' @param rownames string vector with names from the object `coefsm`
#' @param mod the underlying model
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
#' Helper function for adding reference levels to the regression coefficients
#' of factors
#'
#' @param coefsm a data frame containing the regression coefficients
#' @param mod the underlying model
#' @template or
#'
#' @return a data frame containing the regression coefficients including their
#'  reference levels
#'
#' @keywords internal
add_reference_levels <- function(coefsm, mod, or){
  facrows <- sapply(stats::model.frame(mod), class)
  facrows <- sapply(facrows, function(x) x[[1]])
  facrows <- facrows %in% c("factor", "ordered")
  facrows[1] <- FALSE
  if (sum(facrows) > 0) {
    faclevs <- sapply(stats::model.frame(mod)[,facrows],
                      function(x) levels(x)[1])
    facrlabs <- paste0(names(faclevs), faclevs)

    facvec <- numeric()
    for(i in 1:length(facrows)) {
      if (facrows[i] == FALSE) {
        facvec <- c(facvec, FALSE)
      } else {
        facvec <- c(facvec, TRUE, rep(FALSE,
                                      length(levels(stats::model.frame(mod)[, i])) - 2))
      }
    }

    facvec <- facvec[-1]

    emptyrow <- c(0, rep(".", (ncol(coefsm) - 3)), "coefficient", ".")
    if(or) emptyrow <- c(1, rep(".", (ncol(coefsm) - 3)), "coefficient", ".")
    newrowpos <- grep(1, facvec)
    j <- 0
    for(i in 1:sum(facrows)) {
      if (newrowpos[i] == 1) {
        coefsm <- rbind(emptyrow, coefsm)
        rownames(coefsm)[1] <- facrlabs[i]
      } else {
        coefsm <- rbind(coefsm[1:(newrowpos[i] + j - 1), , drop = FALSE],
                        emptyrow,
                        coefsm[(newrowpos[i] + j):nrow(coefsm), , drop = FALSE])
        rownames(coefsm)[newrowpos[i] + j] <- facrlabs[i]
      }
      j <- j + 1
    }
  }
  return(coefsm)
}
