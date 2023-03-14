#' Helper function for updating row names using the labels from a data frame
#'
#' @param rownames string vector with names from the object `coefsm`
#' @param mod the underlying model
#' @template addref
#' @template rowlabs_auto
#' @param covar_names a string vector containing the names of the covariates
#'  in the data set, default is `names(stats::model.frame(mod))[-1]`
#' @template covar_pos
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
#' @template covar_pos
#'
#' @return a data frame containing the regression coefficients including their
#'  reference levels
#'
#' @keywords internal
add_reference_levels <- function(coefsm, mod, or, covar_pos){
  # facrows is a logical vector denoting whether the variables in the
  # model.frame are factors and therefore need a reference level
  facrows <- sapply(stats::model.frame(mod), class)
  facrows <- sapply(facrows, function(x) x[[1]])
  facrows <- facrows %in% c("factor", "ordered")
  # The first variable is the dependent variable, which doesn't need a
  # reference level
  facrows[1] <- FALSE
  # If there are no factor variables, no need to add reference levels.
  if (sum(facrows) > 0) {
    # faclevs contains the names of the reference level for the vactor variables
    faclevs <- sapply(stats::model.frame(mod),
                      function(x) levels(x)[1])[facrows]
    facrlabs <- paste0(names(faclevs), faclevs)
    # newrowpos denotes at what position in the updated coefficients table
    # the reference level will be. facvec is a helper vector for generating
    # newrowpos
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
    newrowpos <- grep(1, facvec)
    # Empty row is the table row for the reference level. Reference value is
    # 0 on an additive scale and 1 on a multiplicative scale
    emptyrow <- c(0, rep(".", (ncol(coefsm) - 3)), ".", "coefficient")
    if(or) emptyrow <- c(1, rep(".", (ncol(coefsm) - 3)), ".", "coefficient")
    # coefsm_c are the rows of coefsm which are actually covariates, not intercepts
    coefsm_c <- coefsm[covar_pos,]
    # coefsm_i are the rows of coefsm which are intercepts
    coefsm_i <- coefsm[-covar_pos,]
    # insert the reference levels into coefsm_c
    j <- 0
    for(i in 1:sum(facrows)) {
      if (newrowpos[i] == 1) {
        coefsm_c <- rbind(emptyrow, coefsm_c)
        rownames(coefsm_c)[1] <- facrlabs[i]
      } else {
        coefsm_c <- rbind(coefsm_c[1:(newrowpos[i] + j - 1), , drop = FALSE],
                        emptyrow,
                        coefsm_c[(newrowpos[i] + j):nrow(coefsm_c), , drop = FALSE])
        rownames(coefsm_c)[newrowpos[i] + j] <- facrlabs[i]
      }
      j <- j + 1
    }
    # position of intercept rows
    intercept_pos = (1:nrow(coefsm))[-covar_pos]
    # combine coefsm_c and coefsm_i
    if(max(intercept_pos) < min(covar_pos)){
      coefsm <- rbind(coefsm_i, coefsm_c)
    } else if(max(covar_pos) < min(intercept_pos)){
      coefsm <- rbind(coefsm_c, coefsm_i)
    } else{
      stop("Your coefficients table has a complicated structure where covariates
           and intercepts are intermingled. The function add_reference_levels
           needs to be updated to suit your need.")
    }
  }
  return(coefsm)
}
