# define van Genuchten model as a function
# https://en.wikipedia.org/wiki/Water_retention_curve
.vg <- function(phi, theta_r, theta_s, alpha, n) {
  theta_r + ((theta_s - theta_r) / ((1 + (alpha * phi)^n)^(1-1/n)))
}




## notes: Rosetta units for alpha and npar are cm and 1/cm
# VG_params: table of VG parameters from KSSL / Rosetta:  alpha and npar are in log10 form
# phi_min: lower limit for water potential in kPa
# phi_max: upper limit for water potential in kPa
# pts: number of points to include in curve
KSSL_VG_model <- function(VG_params, phi_min=10^-6, phi_max=10^8, pts=100) {
  
  # sanity check: no NA allowed
  # return NULL if present
  if(any(is.na(VG_params)))
    return(list(VG_curve=NULL, VG_inverse_function=NULL))
  
  # useful range in kPa suctions
  phi <- 10^seq(log(phi_min, base=10), log(phi_max, base=10), length.out = pts)
  m <- data.frame(phi=phi)
  
  # Rosetta units for alpha and npar are cm and 1/cm
  # convert kPa to cm of H20
  h <- m$phi * 10.19716
  
  # compute theta given measured parameters and sequence of phi
  m$theta <- .vg(phi= h, theta_r = VG_params$theta_r, theta_s = VG_params$theta_s, alpha = 10^(VG_params$alpha), n = 10^(VG_params$npar))
  
  # use splines to fit standard model: phi -> theta
  # scale of theta {0,1}
  # phi: units of kPa
  vg.fwd <- splinefun(m$phi, m$theta)
  
  # use splines to fit inverse model: theta -> phi
  # scale of theta {0,1}
  # phi: units of kPa
  vg.inv <- splinefun(m$theta, m$phi)
  
  # return curve and spline function
  return(list(VG_curve=m, VG_function=vg.fwd, VG_inverse_function=vg.inv))
}



