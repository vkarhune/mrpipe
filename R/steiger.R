#' @export
# Steiger (signed) z statistic
steiger <- function(n_exposure, n_outcome,
                    r_exposure = sqrt(R2_exposure), r_outcome = sqrt(R2_outcome)){
  z <- psych::r.test(n = n_exposure, n2 = n_outcome,
                     r12 = r_exposure,
                     r34 = r_outcome)$z
  out <- z*(2*(r_exposure > r_outcome)-1)
  return(out)
}