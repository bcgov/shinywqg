lookup_limits2 <- function (variable, ph = NULL, hardness = NULL, chloride = NULL, methyl_mercury = NULL, 
          term = "long") 
{
  # assert_that(is.null(ph) || (is.number(ph) && noNA(ph)))
  # assert_that(is.null(hardness) || (is.number(hardness) && 
  #                                     noNA(hardness)))
  # assert_that(is.null(chloride) || (is.number(chloride) && 
  #                                     noNA(chloride)))
  # assert_that(is.null(methyl_mercury) || (is.number(methyl_mercury) && 
  #                                           noNA(methyl_mercury)))
  # assert_that(is.string(term))
  term <- tolower(term)
  if (!term %in% c("short", "long")) 
    stop("term must be \"short\" or \"long\"")
  codes <- wqbc:::setup_codes()
  codes <- wqbc:::setup_condition_values(codes, ph = ph, hardness = hardness, 
                                  chloride = chloride, methyl_mercury = methyl_mercury)
  if (term == "long") {
    dates <- codes$Date
    codes <- rbind(codes, codes, codes, codes, codes)
    codes$Date <- c(dates, dates + 1, dates + 2, dates + 
                      3, dates + 21)
  }
  limits <- wqbc::calc_limits(codes, term = term, keep_limits = FALSE, 
                        messages = FALSE)
  limits <- wqbc:::add_missing_limits(limits, term = term)
  limits <- wqbc:::tidyup_limits(limits)
  limits
}
