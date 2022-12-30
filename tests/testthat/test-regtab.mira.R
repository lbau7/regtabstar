test_that("regtab.mira delivers the correct table for glm fits", {
  res_expected <- paste0("\\begin{table}\n\n",
                         "\\caption{Pooled estimates of logistic regression",
                         " estimates on multiply imputed data sets",
                         " (no. of subjects = 25, no. of imputations = 2)}\n",
                         "\\centering\n",
                         "\\begin{tabular}[t]{lllll}\n",
                         "\\toprule\n",
                         "  & Odds Ratio & Lower CL & Upper CL & p-value\\\\\n",
                          "\\midrule\n",
                          "BMI [kg/m\\textasciicircum{}2] (20-39) & 1 & . & . & .\\\\\n",
                          "BMI [kg/m\\textasciicircum{}2] (40-59) & 49603.632 & 0 & Inf & 0.996\\\\\n",
                          "BMI [kg/m\\textasciicircum{}2] (60-99) & 18630.752 & 0 & Inf & 0.996\\\\\n",
                          "Total serum cholesterol [mg/dl] & 1.038 & 0.741 & 1.453 & 0.564\\\\\n",
                          "\\bottomrule\n",
                          "\\end{tabular}\n",
                          "\\end{table}")
  class(res_expected) <- "knitr_kable"
  attr(res_expected, "format") <- "latex"
  data(nhanes2, package="mice")
  data <- nhanes2
  attr(data$age, "label") <- "Age [years]"
  attr(data$age, "label") <- "BMI [kg/m^2]"
  attr(data$hyp, "label") <- "Hypertension"
  attr(data$chl, "label") <- "Total serum cholesterol [mg/dl]"
  imp2 <- mice::mice(data, m = 2, print = FALSE, seed = 14221)
  set.seed(14)
  mod2 <- mice:::with.mids(imp2, glm(hyp ~ age + chl, family = binomial))
  expect_equal(regtab(mod2, rowlabs_auto = data), res_expected)
})
test_that("regtab.mira delivers the correct table for MASS::polr fits", {
  res_expected <- paste0(
    "\\begin{table}\n",
    "\n",
    "\\caption{Pooled estimates of ordinal logistic regression estimates",
    " on multiply imputed data sets (no. of subjects = 72, no. of imputations = 2)}\n",
    "\\centering\n",
    "\\begin{tabular}[t]{lllll}\n",
    "\\toprule\n",
    "  & Odds Ratio & Lower CL & Upper CL & p-value\\\\\n",
    "\\midrule\n",
    "Degree of influence (Low) & 1 & . & . & .\\\\\n",
    "Degree of influence (Medium) & 0.581 & 0.016 & 20.848 & 0.369\\\\\n",
    "Degree of influence (High) & 0.869 & 0.66 & 1.143 & 0.314\\\\\n",
    "Type (Tower) & 1 & . & . & .\\\\\n",
    "Type (Apartment) & 0.637 & 0.009 & 43.632 & 0.477\\\\\n",
    "\\addlinespace\n",
    "Type (Atrium) & 0.763 & 0.142 & 4.112 & 0.512\\\\\n",
    "Type (Terrace) & 1.292 & 0.108 & 15.509 & 0.597\\\\\n",
    "Contact with other residents (Low) & 1 & . & . & .\\\\\n",
    "Contact with other residents (High) & 1.68 & 1.259 & 2.241 & 0.004\\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}"
  )
  class(res_expected) <- "knitr_kable"
  attr(res_expected, "format") <- "latex"
  data(housing, package="MASS")
  set.seed(123)
  expect_warning(housing_amp <- mice::ampute(housing)$amp)
  housing_amp$Sat <- ordered(housing_amp$Sat,
                             levels = (1:3),
                             labels = levels(housing$Sat))
  housing_amp$Infl <- factor(housing_amp$Infl,
                             levels = (1:3),
                             labels = levels(housing$Infl))
  housing_amp$Type <- factor(housing_amp$Type,
                             levels = (1:4),
                             labels = levels(housing$Type))
  housing_amp$Cont <- factor(housing_amp$Cont,
                             levels = (1:2),
                             labels = levels(housing$Cont))
  attr(housing_amp$Sat, "label") <- "Satisfaction"
  attr(housing_amp$Infl, "label") <- "Degree of influence"
  attr(housing_amp$Type, "label") <- "Type"
  attr(housing_amp$Cont, "label") <- "Contact with other residents"
  imp3 <- mice::mice(housing_amp, m = 2, print = FALSE, seed = 14221)
  set.seed(67)
  mod3 <- mice:::with.mids(imp3,
                           MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq,
                                      Hess = TRUE))

  expect_equal(regtab(mod3, rowlabs_auto = housing_amp), res_expected)
})
