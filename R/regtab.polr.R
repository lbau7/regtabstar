#' Regression tables for proportional odds regressions models from the MASS
#' package
#'
#' regtab method for models of class \code{polr}.
#'
#' @param mod A model of class \code{polr}.
#' @template format
#' @template style_options
#' @template or
#' @template logor
#' @template ci_or
#' @template ci_level
#' @template se_logor
#' @template vcov
#' @template teststatistic
#' @template pval
#' @template intercept
#' @template caption_glm
#' @template n_caption
#' @template rowlabs
#' @template rowlabs_auto
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @details Models of class \code{polr} are currently only supported for
#'   logistic method. If `pval = TRUE`, then the function `AER:::coeftest.polr`
#'   is used for for calculating p-values. Note that calculating p-values
#'   for proportional odds models may be unreliable.
#' @export
#'
#' @examples
#' library(MASS)
#' # Basic example taken from the MASS documentation
#' data(housing, package="MASS")
#' attr(housing$Sat, "label") <- "Satisfaction"
#' attr(housing$Infl, "label") <- "Degree of influence"
#' attr(housing$Type, "label") <- "Type"
#' attr(housing$Cont, "label") <- "Contact with other residents"
#' attr(housing$Freq, "label") <- "Number of residents"
#' house.plr <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq,
#'                         Hess = TRUE, data = housing)
#' regtab(house.plr, rowlabs_auto = housing)
regtab.polr <- function(mod, format = "latex", style_options = list(),
                       or = TRUE, logor = FALSE, ci = TRUE, ci_level = 0.95,
                       se_logor = FALSE, teststatistic = FALSE,
                       pval = TRUE, intercept = FALSE, caption = NULL,
                       n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                       digits = 3, rowlabs_auto = NULL, ...) {
  if (mod$method != "logistic") {
    stop("Only logistic proportional odds models are currently supported")
  }
  # Get model estimates
  coefsm <- data.frame(broom::tidy(mod, conf.int = TRUE, conf.level = ci_level,
                        p.values = FALSE))
  rownames(coefsm) = coefsm[,1]
  coefsm <- cbind(exp(coefsm[,2, drop = FALSE]),
                  coefsm[,c(5,6), drop = FALSE],
                  coefsm[,c(2,3,4,7), drop = FALSE],
                  `p-value` = rep(NA_real_, length(coefsm[,1])))
  # Exponentiate confidence intervals
  if(or){
    coefsm[,c(2, 3)] <- exp(coefsm[,c(2,3)])
  }
  # Calculate p-values
  if(pval){
    coefsm[["p-value"]] = AER:::coeftest.polr(mod, vcov. = vcov)[,"Pr(>|z|)"]
  }
  # Round to specified number of digits
  if (pval) highsig <- which(coefsm[, "p-value"] < 0.001)
  coefsm[,-7] <- round(coefsm[,-7], digits = digits)
  if (pval) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  # Add reference level
  if (addref) {
    facrows <- sapply(stats::model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    # Outcome variable does not need reference level
    facrows[1] <- FALSE
    if (sum(facrows) > 0) {
      faclevs <- sapply(stats::model.frame(mod)[, facrows],
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
      emptyrow <- c(1, ".", ".", 0, ".", ".", "coefficient", ".")
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
  }

  if (!is.null(rowlabs_auto) & is.null(rowlabs)) {
    covar_pos = which(coefsm$coef.type == "coefficient")
    covar_names = names(stats::model.frame(mod))[-1]
    if(!is.null(mod$model$`(weights)`)){
      covar_names <- covar_names[-length(covar_names)]
    }
    rowlabs <- generate_rowlabs(rownames = rownames(coefsm),  mod,
                                addref = addref, rowlabs_auto = rowlabs_auto,
                                covar_names = covar_names,
                                covar_pos = covar_pos)
  }

  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs

  # Remove intercept rows if unwanted
  if (!intercept) coefsm <- coefsm[-which(coefsm$coef.type == "scale"), , drop = FALSE]
  # Add caption
  if (is.null(caption)) caption <- "Proportional odds logistic regression"
  if (n_caption) {
    caption <- paste0(caption, " (n = ", nrow(stats::model.frame(mod)), ")")
  }
  # Remove unwanted columns
  colnames(coefsm) <- c("Odds Ratio", "Lower CL", "Upper CL",
                        "log OR", "SE (log OR)", "t-value",
                        "coef.type",  "p-value")
  inc_col <- which(c(or, ci, ci, logor, se_logor, teststatistic, 0, pval) != 0)
  coefsm <- coefsm[, inc_col, drop = FALSE]
  # Output table using kable
  out <- kableExtra::kbl(coefsm, format = format, booktabs = TRUE,
    caption = caption, ...)

  if (length(style_options) > 0) {
    do.call(kableExtra::kable_styling, c(list(out), style_options))
  } else {
    out
  }
}
