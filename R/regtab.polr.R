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
                       se_logor = FALSE, vcov = NULL, teststatistic = FALSE,
                       pval = TRUE, intercept = FALSE, caption = NULL,
                       n_caption = TRUE, rowlabs = NULL, addref = TRUE,
                       digits = 3, rowlabs_auto = NULL, ...) {
  if (mod$method != "logistic") {
    stop("Only logistic proportional odds models are currently supported")
  }
  # Get model estimates
  coefsm <- data.frame(broom::tidy(mod, conf.int = TRUE, conf.level = ci_level,
                        p.values = FALSE))
  rownames(coefsm) = coefsm[, 1]
  coefsm <- cbind(exp(coefsm[, 2, drop = FALSE]),
                  coefsm[, c(5,6), drop = FALSE],
                  coefsm[, c(2,3,4), drop = FALSE],
                  `p-value` = rep(NA_real_, length(coefsm[,1])),
                  coefsm[, 7, drop = FALSE])
  # Exponentiate confidence intervals
  if(or){
    coefsm[,c(2, 3)] <- exp(coefsm[,c(2,3)])
  }
  # Calculate p-values
  if(pval){
    coefsm[["p-value"]] = AER:::coeftest.polr(mod, vcov. = vcov)[,"Pr(>|z|)"]
  }
  # covar_pos denotes the position of covariates (i.e. rows which are not
  # intercepts)
  covar_pos = which(coefsm$coef.type == "coefficient")
  # Round to specified number of digits
  if (pval) highsig <- which(coefsm[, "p-value"] < 0.001)
  coefsm[,-which(names(coefsm) == "coef.type")] <-
    round(coefsm[,-which(names(coefsm) == "coef.type")], digits = digits)
  if (pval) coefsm[highsig, "p-value"] <- "<0.001"
  # Add reference level
  if (addref) {
    coefsm <- add_reference_levels(coefsm, mod, or, covar_pos)
  }

  if (!is.null(rowlabs_auto) & is.null(rowlabs)) {
    covar_names = names(stats::model.frame(mod))[-1]
    if(!is.null(mod$model$`(weights)`)){
      covar_names <- covar_names[-length(covar_names)]
    }
    rowlabs <- generate_rowlabs(rownames = rownames(coefsm),  mod,
                                addref = addref, rowlabs_auto = rowlabs_auto,
                                covar_names = covar_names,
                                covar_pos = covar_pos)
    if(intercept){
      outcome_label <- attr(rowlabs_auto[[names(stats::model.frame(mod))[1]]], "label")
      if(is.null(outcome_label)){
        rowlabs[which(coefsm$coef.type == "intercept")] <-
          paste0("Outcome scale level (", rowlabs[which(coefsm$coef.type == "intercept")], ")")
      } else{
        rowlabs[which(coefsm$coef.type == "intercept")] <-
          paste0(outcome_label, ": outcome scale level (", rowlabs[which(coefsm$coef.type == "intercept")], ")")
      }

    }
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
                        "p-value", "coef.type")
  inc_col <- which(c(or, ci, ci, logor, se_logor, teststatistic, pval, 0) != 0)
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
