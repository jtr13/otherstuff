r2tex <- function (x) {
  greek <- c(
    "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
    "gamma", "gamma", "varpi", "phi", "delta", "kappa", "rho",
    "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
    "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
    "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
    "Upsilon", "Omega", "Theta", "Pi", "Phi")

  lapply(greek, function(.x) gsub(.x, paste0("\\\\", .x), x))

  mu = 2; sigma = .25; n = 35; muprime = 2.09; alpha = .05
