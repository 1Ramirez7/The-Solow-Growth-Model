#----------------------------------
# 2. SOLOW SIMULATION
#----------------------------------

simulate_solow <- function(num_periods, s, delta, n, z, l, A_init, L_init, experiments_df = NULL, endomods_df    = NULL) { # v4.1 add it endo
  params <- data.frame(
    Period   = 0:(num_periods - 1),
    s        = rep(s, num_periods),
    delta    = rep(delta, num_periods),
    n        = rep(n, num_periods),
    z        = rep(z, num_periods),
    l        = rep(l, num_periods),
    delta_k  = rep(0.0, num_periods),
    percent_delta_k = rep(0.0, num_periods),
    little_k = rep(0.0, num_periods),
    A        = rep(A_init, num_periods),
    L        = rep(L_init, num_periods),
    K        = rep(0.0, num_periods),
    Y        = rep(0.0, num_periods),
    investments   = rep(0.0, num_periods),  # v4.3
    consumption_s = rep(0.0, num_periods),  # v4.3
    depreciation  = rep(0.0, num_periods) # v4.3
  )
  
  # Calculate initial Capital per Capita Steady state k* 
  k_star_initial <- (s / (delta + n + z*l))^(1.5)*(1 - l)
  params$little_k[1] <- k_star_initial
  
  # this does the quantitative experiments. version 2.1
  if (!is.null(experiments_df) && nrow(experiments_df) > 0) { # v2.1
    for (exp in seq_len(nrow(experiments_df))) {
      start <- experiments_df$start_period[exp] + 1
      end   <- min(start + experiments_df$length[exp] - 1, num_periods)
      param_name <- experiments_df$param[exp]
      value  <- experiments_df$value[exp]
      params[start:end, param_name] <- value
    }
  }
  
  # v4.1 adding endo code to apply endo changes at beg to params
  if (!is.null(endomods_df) && nrow(endomods_df) > 0) {
    for (endo in seq_len(nrow(endomods_df))) {
      start <- endomods_df$start_period[endo] + 1
      end   <- min(start + endomods_df$length[endo] - 1, num_periods)
      param_name <- endomods_df$param[endo]
      value      <- endomods_df$value[endo]
      params[start:end, param_name] <- value
    }
  } # end of v4.1
  
  # Calculation portion. # v2.1
  # the "for (i in seq_len" makes sure all calculations are done in that order once every period. If you need to ask why don't edit this portion. 
  for (i in seq_len(num_periods)) { 
    if (i == 1) { # i=1 corresponds to Period=0 in the df
      params$delta_k[i] <- 0
    } else {
      # i-1 = t - 1
      params$delta_k[i] <- params$s[i] * ( (1 - params$l[i])^(2/3) ) * 
        (params$little_k[i-1]^(1/3)) - 
        (params$z[i] * params$l[i] + params$n[i] + params$delta[i]) * params$little_k[i-1]
      
      params$little_k[i] <- params$little_k[i-1] + params$delta_k[i]
    } # different calculation if period 0 or >0
    
    if (!is.null(endomods_df) && # v4.1
        any(endomods_df$param == "delta_k" & endomods_df$start_period + 1 == i)) {
      params$delta_k[i] <- endomods_df$value[endomods_df$param == "delta_k" & endomods_df$start_period + 1 == i]
    } # v4.1
    
    # best way to  add this code is adding the formula after little_k calc
    # so just let it run normally and overwrite right after.
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "little_k" & endomods_df$start_period + 1 == i)) {
      params$little_k[i] <- endomods_df$value[endomods_df$param == "little_k" & endomods_df$start_period + 1 == i]
    } # v4.1
    
    if (i > 1) {# cant be done till period 1. 
      params$A[i] <- params$A[i-1] * (1 + params$z[i]*params$l[i])
      params$L[i] <- params$L[i-1] * (1 + params$n[i])
    }
    
    if (!is.null(endomods_df) && # v4.1
        any(endomods_df$param == "L" & endomods_df$start_period + 1 == i)) {
      params$L[i] <- endomods_df$value[endomods_df$param == "L" & endomods_df$start_period + 1 == i]
    } # v4.1
    # v4.1 adding checkl for A
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "A" & endomods_df$start_period + 1 == i)) {
      params$A[i] <- endomods_df$value[endomods_df$param == "A" & endomods_df$start_period + 1 == i]
    } # v4.1
    
    
    # this calculations are done every period
    params$K[i] <- params$little_k[i] * params$A[i] * params$L[i]
    params$Y[i] <- (1 - params$l[i])^(2/3) * params$K[i]^(1/3) * (params$A[i] * params$L[i])^(2/3)
    
    if (!is.null(endomods_df) && #v4.1
        any(endomods_df$param == "K" & endomods_df$start_period + 1 == i)) {
      params$K[i] <- endomods_df$value[endomods_df$param == "K" & endomods_df$start_period + 1 == i]
    } # v4.1
    
    if (!is.null(endomods_df) && #4.1
        any(endomods_df$param == "Y" & endomods_df$start_period + 1 == i)) {
      params$Y[i] <- endomods_df$value[endomods_df$param == "Y" & endomods_df$start_period + 1 == i]
    } # v4.1
  }
  
  # this calculations are done once the for i loop is done for better app efficiency. 
  params$MPL <- (1 - (1/3)) * ((1 - params$l)^(1 - (1/3))) * (params$K / (params$A * params$L)^(1/3)) * params$A # v3.3.1 ch function from ch5 mpk to ch6 mpk
  params$MPK <- (1/3) * ((1 - params$l)^(1 - (1/3))) * (((params$A *  params$L) / params$K)^(1 - (1/3))) # v3.3.1 ch function from ch5 mpk to ch6 mpk
  params$log_L <- log(params$L)
  params$log_K <- log(params$K)
  params$log_Y <- log(params$Y)
  params$log_A <- log(params$A)
  params$percent_delta_k <- ((round(params$delta_k, 3)) / params$little_k) * 100 # v3.3.3 round 3 decimal places
  
  
  for (i in seq_len(num_periods)) { # v4.3
    if (i == 1) {
      params$investments[i] <- params$s[i] * params$Y[i]  # v4.3 ask Moncayo if we can use this one or I_star?
      
      params$consumption_s[i] <- (1 - params$s[i]) * params$Y[i] # v4.3. confirm calc. this is c_star in solow-romer math
      
      params$depreciation[i] <- params$delta[i] * params$K[i]
    } else {
      params$investments[i] <- params$s[i] * params$Y[i]  # v4.3 ask Moncayo if we can use this one
      
      params$consumption_s[i] <- (1 - params$s[i]) * params$Y[i] # v4.3. confirm calc. this is c_star in solow-romer math
      
      params$depreciation[i] <- params$delta[i] * params$K[i-1]
    }
  } # v4.3
  
  params # v3. before v3, this was return(params)
  # when adding calc that need plots = add to tabpanel, and both codes.
}


# added calculation spacers for investment, consumption and depreciation.