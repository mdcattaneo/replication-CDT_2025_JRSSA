#******************************************************************************#
#******************************************************************************#
#** Randomization inference for before-and-after studies with multiple units **# 
#********** An application to a criminal procedure reform in Uruguay **********#
#******************************************************************************#
#******************************************************************************#

#* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */
#* 

#******************************************************************************/
# Function to perform randomization inference analysis using multiple mechanisms
#******************************************************************************/
library(dplyr)
library(magrittr)

BArandinf = function(Y, time, unit, w, method, statistic="diffmeans", a = NULL, N=1000, N_CI=N, seed=NULL, ConfInt=FALSE, thetas=NULL, match_BA_BAfast = FALSE){
  
  # Y outcome of interest
  # score: time periods or other discrete score, taking values -2, -1, 1, 2, etc, with 1 the first treated period
  # w local window where analysis is conducted
  # method = c("TR", "AT")
  # 'TR' assigns either the pre-intervention period to treatment and the post-intervention period to control, or viceversa
  # 'AT' assigns the time of adoption randomly
  
  set.seed(seed)
  if(method == "AT" & is.null(a))  stop("ERROR: Must specify a for AT mechanism")
  if(method == "AT") if ((min(a) < -w+1 |  max(a) > w-1) &  method == "AT") stop("Vector a must be within the window")
  
  # Create the score that has 0 as first treated period: -2,-1,0,1,2,3..
  time0 = ifelse(time >= 1, time-1, time)
  B = length(unique(unit))
  
  data = data.frame(Y, time, time0, unit)
  
  if(statistic == "timeadj") {
    # Fit linear trend model, and replace Y with residuals
    data$Y = lm(Y ~ time0 + factor(unit) + time0:factor(unit), data=data)$residuals
  }
  
  # Keep data in in window
  data = data %>% filter(abs(time)<=w)
  
  Sobs_data <- data %>%
    mutate(tr_group = time >= 0) %>%
    group_by(unit, tr_group) %>%
    summarise(meanY = mean(Y), .groups = "drop")
  
  Sobs <- mean(Sobs_data$meanY[Sobs_data$tr_group == TRUE]) - mean(Sobs_data$meanY[Sobs_data$tr_group == FALSE])
  #Sobs =  mean(data$Y[data$time>=0]) - mean(data$Y[data$time<0])
  meanCo= mean(data$Y[data$time<0])
  
  # If match_BA_BAfast == TRUE, I generate all random assignments BEFORE the loop; this ensures that  BArandinf_fast and BArandinf give same result for the same seed
  if(match_BA_BAfast == TRUE) {
    assignments <- if (method == "TR") c(1, -1) else a
    C_matrix <- matrix(sample(assignments, size = N * B, replace = TRUE), nrow = N, ncol = B)
  }
  
  S = numeric(N)
  CI = c(NA,NA)
  
  cat("Starting randomization inference for method ",method,"with ", N, "simulations in window ",w," \n")
  for(i in 1:N) {
    
    if(match_BA_BAfast == TRUE) {
    # Use the pre-generated random assignment for this iteration
      C =  C_matrix[i, ]
    } else {
      if (method == "TR") {
        C = sample(c(1,-1), size = B, replace = TRUE)   # draw -1, or 1 for every unit, to flip entire pre/post period
      } else {
        C =  sample(a, size = B, replace = TRUE)  # draw random cutoff for every unit
      }
    } 
    
    dataC = data.frame(unit= 1:B, cutoff = C)
    df = data %>% left_join(dataC, by = "unit")
    
    
    if (method == "TR") {
      df = df %>% mutate(time_new = time * cutoff , tr_new = time_new >=0) # Multiple original score by 1 or -1 for each unit: assigns pre or post intervention period to treatment for each unit
    } else {
      df = df %>% mutate(time_new = time0 - cutoff, tr_new = time_new >=0) # Create normalized score using each unit's specific cutoff (0 must be true cutoff)
    }
    
    # Keep df_treated and df_control, then merge. Then I will see that multiple units do not have Tr or Co 
    dfTr = df%>% filter(tr_new ==1) %>% group_by(unit, tr_new) %>% summarise(meanY = mean(Y), n=n(), .groups = "drop") %>% as.data.frame()
    dfCo = df%>% filter(tr_new ==0) %>% group_by(unit, tr_new) %>% summarise(meanY = mean(Y), n=n(), .groups = "drop") %>% as.data.frame()
    
    if(nrow(dfTr) != B) stop("Not all units are treated in this window")
    if(nrow(dfCo) != B) stop("Not all units are untreated in this window")
    
    df = rbind(dfTr,dfCo)
    
    S[i] = mean(df$meanY[df$tr_new==1]) -  mean(df$meanY[df$tr_new==0])
    
  }
  pval = sum(abs(S) >= abs(Sobs))/N
  
  if(ConfInt){
    cat("Starting Confidence Interval Calculation \n")
    nms = c("theta", "pval")
    CIresults = data.frame(matrix(NA,ncol=length(nms), nrow=length(thetas)*length(length), dimnames = list(NULL,nms)))
    
    for(k in 1:length(thetas)){
      
      theta = thetas[k]
      Yadj = data$Y - theta * (data$time>=0)
      SAobs =  mean(Yadj[data$time>=0]) - mean(Yadj[data$time<0]) 
      
      SA = numeric(N)  
      dA   = data.frame(Y=Yadj, time= data$time, time0=data$time0, unit=data$unit)
      
      if(match_BA_BAfast == TRUE) {
        assignments <- if (method == "TR") c(1, -1) else a
        C_matrix_CI <- matrix(sample(assignments, size = N_CI * B, replace = TRUE), nrow = N_CI, ncol = B)
      }
      
      for(i in 1:N_CI) {
        
        if(match_BA_BAfast == TRUE) {
          C = C_matrix_CI[i, ]
        } else {
          if (method == "TR") {
            C = sample(c(1,-1), size = B, replace = TRUE)   # draw -1, or 1 for every unit, to flip entire pre/post period
          } else {
            C =  sample(a, size = B, replace = TRUE)  # draw random cutoff for every unit
          }
        }
        
        dataC = data.frame(unit= 1:B, cutoff = C)
        df = dA %>% left_join(dataC, by = "unit")
        
        if (method == "TR") {
          df = df %>% mutate(time_new = time * cutoff , tr_new = time_new >=0) # Multiple original score by 1 or -1 for each unit: assigns pre or post intervention period to treatment for each unit
        } else {
          df = df %>% mutate(time_new = time0 - cutoff, tr_new = time_new >=0) # Create normalized score using each unit's specific cutoff (0 must be true cutoff)
        }
        
        # Keep df_treated and df_control, then merge. Then I will see that multiple units do not have Tr or Co 
        dfTr = df%>% filter(tr_new ==1) %>% group_by(unit, tr_new) %>% summarise(meanY = mean(Y), n=n(), .groups = "drop") %>% as.data.frame()
        dfCo = df%>% filter(tr_new ==0) %>% group_by(unit, tr_new) %>% summarise(meanY = mean(Y), n=n(), .groups = "drop") %>% as.data.frame()
        
        if(nrow(dfTr) != B) stop("Not all units are treated in this window")
        if(nrow(dfCo) != B) stop("Not all units are untreated in this window")
        
        df = rbind(dfTr,dfCo)
        
        SA[i] = mean(df$meanY[df$tr_new==1]) -  mean(df$meanY[df$tr_new==0])
        
      }
      p = sum(abs(SA) >= abs(SAobs))/N_CI
      CIresults[k, "theta"] = theta
      CIresults[k, "pval"] = p
      
      if (k==1 | (k %% 1 == 0))  cat("Theta:", thetas[k], "-- Pval: ",p, "\n")
      
    }
    CI =c(min(CIresults$theta[CIresults$pval > 0.05]),max(CIresults$theta[CIresults$pval > 0.05]))
  }
  
  return(list(Sobs=Sobs, meanCo=meanCo,pval=pval, method=method, CI=CI))
}

########################################
# This rewritten function, BArandinf_fast, is a fast version of BArandinf that uses matrix algebra to avoid loops and data frame manipulation during the simulation step. 
################################################

BArandinf_fast <- function(Y, time, unit, w, method, statistic = "diffmeans", a = NULL, N = 1000, N_CI = N, seed = NULL, ConfInt = FALSE, thetas = NULL) {
  
  # Vectorized helper function with strict error checking
  vectorized_simulation_strict <- function(Y_vec, N_sim, B, data_mat, method, a) {
    
    # --- Step 1: Generate Random Assignments ---
    assignments <- if (method == "TR") c(1, -1) else a
    C_matrix <- matrix(sample(assignments, size = N_sim * B, replace = TRUE), nrow = N_sim, ncol = B)
    
    # --- Step 2: Calculate Treatment Status ---
    unit_matrix <- model.matrix(~ as.factor(unit) - 1, data_mat)
    
    if (method == "TR") {
      time_new_mat <- (C_matrix %*% t(unit_matrix)) * matrix(data_mat$time, nrow = N_sim, ncol = nrow(data_mat), byrow = TRUE)
    } else { # Handles 'AT'
      time_new_mat <- matrix(data_mat$time0, nrow = N_sim, ncol = nrow(data_mat), byrow = TRUE) - (C_matrix %*% t(unit_matrix))
    }
    tr_new_mat <- time_new_mat >= 0
    
    # --- Step 3: Calculate Unit-Level Counts ---
    Y_mat <- matrix(Y_vec, nrow = N_sim, ncol = length(Y_vec), byrow = TRUE)
    tr_count_by_unit <- tr_new_mat %*% unit_matrix
    co_count_by_unit <- (!tr_new_mat) %*% unit_matrix
    
    # --- Step 4: Strict Error Check ---
    # This check stops the entire function if any simulation creates a zero-count unit.
    if (any(tr_count_by_unit == 0) || any(co_count_by_unit == 0)) {
      stop("ERROR: A random assignment resulted in a unit with no treated or control observations. Your window 'w' is likely too small for the specified 'a'.")
    }
    
    # --- Step 5: Calculate Means ---
    tr_sum_Y_by_unit <- (tr_new_mat * Y_mat) %*% unit_matrix
    co_sum_Y_by_unit <- ((!tr_new_mat) * Y_mat) %*% unit_matrix
    
    tr_mean_by_unit <- tr_sum_Y_by_unit / tr_count_by_unit
    co_mean_by_unit <- co_sum_Y_by_unit / co_count_by_unit
    
    S_vector <- rowMeans(tr_mean_by_unit) - rowMeans(co_mean_by_unit)
    
    return(S_vector)
  }
  
  # --- Main Function Body ---
  
  set.seed(seed)
  # --- Original Error Check Reinstated ---
  if (method == "AT") {
    if (is.null(a)) stop("ERROR: Must specify a for method 'persistent'")
    if(method == "AT") if ((min(a) < -w |  max(a) > w) &  method == "AT"){
      stop("Vector a must be within the window")
    }
  }
  
  time0 <- ifelse(time >= 1, time - 1, time)
  B <- length(unique(unit))
  data <- data.frame(Y, time, time0, unit)
  
  if (statistic == "timeadj") {
    data$Y <- lm(Y ~ time0 + factor(unit) + time0:factor(unit), data = data)$residuals
  }
  
  data <- data %>% filter(abs(time) <= w)
  
  
  # Correctly calculate Sobs as a mean-of-unit-means to match the simulation
  Sobs_data <- data %>%
    mutate(tr_group = time >= 0) %>%
    group_by(unit, tr_group) %>%
    summarise(meanY = mean(Y), .groups = "drop")
  Sobs <- mean(Sobs_data$meanY[Sobs_data$tr_group == TRUE]) - mean(Sobs_data$meanY[Sobs_data$tr_group == FALSE])
  #Sobs <- mean(data$Y[data$time >= 0]) - mean(data$Y[data$time < 0])
  meanCo <- mean(data$Y[data$time < 0])
  
  # --- P-value Calculation ---
  cat("Starting randomization inference for method '", method, "' with ", N, " simulations...\n", sep="")
  S_dist <- vectorized_simulation_strict(data$Y, N, B, data, method, a)
  pval <- sum(abs(S_dist) >= abs(Sobs)) / N
  
  # --- Confidence Interval Calculation ---
  CI <- c(NA, NA)
  if (ConfInt) {
    if (is.null(thetas)) stop("ERROR: Must provide a vector of 'thetas' to search for confidence intervals.")
    cat("Starting Confidence Interval Calculation...\n")
    
    # Use sapply to loop over the grid of theta values
    pvals_ci <- sapply(thetas, function(theta) {
      # Adjust the outcome variable by the hypothesized treatment effect theta
      Yadj <- data$Y - theta * (data$time >= 0)   # using individual level model
      
      SAobs <- mean(Yadj[data$time >= 0]) - mean(Yadj[data$time < 0])
      
      # Run the strict, vectorized simulation on the adjusted data
      SA_dist <- vectorized_simulation_strict(Yadj, N_CI, B, data, method, a)
      
      # Calculate the p-value for this specific theta
      p <- sum(abs(SA_dist) >= abs(SAobs)) / N_CI
      
      cat("Theta:", theta, "-- P-val: ", round(p, 4), "\n")
      return(p)
    })
    
    # Find the range of thetas where the p-value is not significant (p > 0.05)
    eligible_thetas <- thetas[pvals_ci > 0.05]
    if (length(eligible_thetas) > 0) {
      CI <- c(min(eligible_thetas), max(eligible_thetas))
    }
  }
  
  return(list(Sobs = Sobs, meanCo = meanCo, pval = pval, method = method, CI = CI))
}



########################################

# Version of BArandinf  to do joint inference for multiple windows

################################################


BArandinf_Joint = function(Y, time, unit,J, method, a = NULL, N=1000, seed=NULL,thetas=NULL){
  
  # Y outcome of interest
  # score: time periods or other discrete score, taking values -2, -1, 1, 2, etc, with 1 the first treated period
  # method = c("TR", "AT")
  # 'reversible' assigns either the pre-intervention period to treatment and the post-intervention period to control, or viceversa
  # 'persistent' assigns the cutoff randomly
  
  set.seed(seed)
  if(method == "AT" & is.null(a))  stop("ERROR: Must specify a for AT mechanism")
  
  # Create the score that has 0 as first treated period: -2,-1,0,1,2,3..
  time0 = ifelse(time >= 1, time-1, time)
  B = length(unique(unit))
  n = B
  
  data = data.frame(Y, time, time0, unit)

  jmin = 1 
  if (method == "AT") {
    jmin = (length(a)-1)/2 + 1
    if(jmin > J) stop("Not enough observations to use method persistent with this choice of J and a")
  }
  Jlist = jmin:J
  
  # Calculate observed statistics
  Diff = numeric(J-jmin+1)
  D1   = matrix(data=NA, ncol= J-jmin+1, nrow = n)
  D0 = D1
  
  for(j in 1:length(Jlist)) {
    # Keep data in in window
    dataW    = data %>% filter(abs(time)<=Jlist[j])
    dataW$Tr = dataW$time>=0 
    # Collapse to mean by unit above and below adoption time
    dataW  = dataW %>% group_by(unit, Tr) %>% summarize(Y=mean(Y), .groups = "drop") 
    
    D1[,j]    = dataW$Y[dataW$Tr==1]
    D0[,j]    = dataW$Y[dataW$Tr==0]
    Diff[j]     = mean(D1[,j]) -  mean(D0[,j])
  }
  SigmaTr = cov(D1) 
  SigmaCo = cov(D0)
  Sigma = ((n-1)*SigmaTr + (n-1)*SigmaCo)/(n+n-2)
  
  W1obs  = max(Diff)
  if(j==1) {
    W2obs =  ((n*n)/(n+n)) * (Diff^2)/as.numeric(Sigma)
  } else {
    W2obs =  ((n*n)/(n+n)) * as.numeric(t(Diff)%*%solve(Sigma)%*%(Diff))
  }
  W3obs = mean(Diff)
 
  W1 = numeric(N)
  W2 = numeric(N)
  W3 = numeric(N)
  
  cat("Starting randomization inference for method ",method,"with ", N," \n")
  for(i in 1:N) {
    
    if (method == "TR") {
      C = sample(c(1,-1), size = B, replace = TRUE)   # draw -1, or 1 for every unit, to flip entire pre/post period
    } else {
      C =  sample(a, size = B, replace = TRUE)  # draw random cutoff for every unit
    }
    
    dataC = data.frame(unit= 1:B, cutoff = C)
    df = data %>% left_join(dataC, by = "unit")
    
    
    if (method == "TR") {
      df = df %>% mutate(time_new = time * cutoff , tr_new = time_new >=0) # Multiple original score by 1 or -1 for each unit: assigns pre or post intervention period to treatment for each unit
    } else {
      df = df %>% mutate(time_new = time0 - cutoff, tr_new = time_new >=0) # Create normalized score using each unit's specific cutoff (0 must be true cutoff)
    }
    
  
    # Calculate statistics
    Diffsim = numeric(J-jmin+1) 
    D1sim   = matrix(data=NA, ncol= J-jmin+1, nrow = n)
    D0sim   = D1sim
    
    for(j in 1:length(Jlist)) {
      
      # Keep data in in window
      dataW    = df %>% filter(abs(time_new)<=Jlist[j])
       
      # Collapse to mean by unit above and below adoption time
      dataW  = dataW %>% group_by(unit, tr_new) %>% summarize(Y=mean(Y), .groups = "drop") 

      D1sim[,j]    = dataW$Y[dataW$tr_new==1]
      D0sim[,j]    = dataW$Y[dataW$tr_new==0]
      Diffsim[j]   = mean(D1sim[,j]) -  mean(D0sim[,j])
    }
    SigmasimTr = cov(D1sim) 
    SigmasimCo = cov(D0sim)
    Sigmasim = ((n-1)*SigmasimTr + (n-1)*SigmasimCo)/(n+n-2)
    W1[i] = max(Diffsim)
    
    if(j==1) {
      W2[i] =  ((n*n)/(n+n)) * (Diffsim^2)/as.numeric(Sigmasim)
    } else {
      W2[i] = ((n*n)/(n+n)) * as.numeric(t(Diffsim)%*%solve(Sigmasim)%*%(Diffsim))
    }
    W3[i] = mean(Diffsim)
  }    
    
  pval1 = sum(abs(W1) >= abs(W1obs))/N
  pval2 = sum(abs(W2) >= abs(W2obs))/N
  pval3 = sum(abs(W3) >= abs(W3obs))/N
 

  return(list(pval1=pval1, pval2=pval2, pval3=pval3, W1obs=W1obs, W2obs=W2obs, W3obs=W3obs, method=method))
}

