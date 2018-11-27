#########################################################################
##
##
##          Perfect Withdrawal Amount
##
##
#########################################################################
##
##
## The Perfect Withdrawal Amount - Suarez, Suarez & Walz, SSRN2551370.
##
## https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2551370
##
## Newfound Research piece... 
##
## https://blog.thinknewfound.com/2018/08/the-state-of-risk-management/
##
##

# download S&P500 returns
getSymbols("^GSPC", from = "1980-01-01")

# convert to annual returns
spx_annual_returns = round(ROC(to.yearly(Ad(GSPC), OHLC = FALSE), n = 1),4)[-1]

# PWR - Perfect Withdrawal Rate - formula comes from the paper by Suarez, Suarez and Walz

PWR <- function(returns, Ks = 1000000, Ke = 0){
  
  total = vector(mode = 'numeric', length = length(returns))
  
  for(i in 1:length(returns)){
    total[i] = prod((1+returns)[i:length(returns)])
  }
  
  Sn = 1 / sum(total)
  
  w_rate = Rn*Sn - Sn*(Ke/Ks)
  
  return(w_rate)
  
}


# run the simulation over 10,000 randomly sampled 30-year return series.

PWR_results <- vector(mode = 'numeric', length = 10000)

for(i in 1:10000){

  spx_sampled_returns = sample(spx_annual_returns, size = 30, replace = TRUE)

  PWR_results[i] <- PWR(spx_sampled_returns)

}


# plot the results

PWR_results %>% truehist(xlim = c(-0.02, 0.15))
PWR_results %>% density(kernel = 'gaussian') %>% plot(xlim = c(-0.05, 0.3))
quantile(PWR_results)


# run the same simulation leaving a 500,000 legacy (Ke = 500k)

PWR_results_with_a_legacy <- vector(mode = 'numeric', length = 10000)

for(i in 1:10000){
  
  spx_sampled_returns = sample(spx_annual_returns, size = 30, replace = TRUE)

  PWR_results_with_a_legacy[i] <- PWR(spx_sampled_returns, Ke = 500000)
  
}

PWR_results_with_a_legacy %>% truehist(xlim = c(-0.02, 0.15), nbins = 100)
PWR_results_with_a_legacy %>% density(kernel = 'gaussian') %>% plot(xlim = c(-0.02,0.15))
range(PWR_results_with_a_legacy)
quantile(PWR_results_with_a_legacy)


# Comparing no legacy (Ke = 0) with leaving a legacy (Ke = 500k)

PWR_results %>% density(kernel = 'gaussian') %>% plot(xlim = c(-0.02, 0.20), col = 'blue')
PWR_results_with_a_legacy %>% density(kernel = 'gaussian') %>% lines(xlim = c(-0.02,0.20), col = 'red')

