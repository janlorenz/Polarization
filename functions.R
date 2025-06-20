library(tidyverse)
confined_discretized_dnorm <- function(mean = 5, sd = 3) {
  c(pnorm(0.5, mean, sd), 
    pnorm(1.5, mean, sd) - pnorm(0.5, mean, sd),
    pnorm(2.5, mean, sd) - pnorm(1.5, mean, sd),
    pnorm(3.5, mean, sd) - pnorm(2.5, mean, sd),
    pnorm(4.5, mean, sd) - pnorm(3.5, mean, sd),
    pnorm(5.5, mean, sd) - pnorm(4.5, mean, sd),
    pnorm(6.5, mean, sd) - pnorm(5.5, mean, sd),
    pnorm(7.5, mean, sd) - pnorm(6.5, mean, sd),
    pnorm(8.5, mean, sd) - pnorm(7.5, mean, sd),
    pnorm(9.5, mean, sd) - pnorm(8.5, mean, sd),
    1 - pnorm(9.5, mean, sd)
  )
}
dist_mean <- function(p, x = 0:(length(p)-1)) sum(p * x) 
dist_sd <- function(p, x = 0:(length(p)-1)) 2*sqrt(sum(p * (x - dist_mean(p,x))^2))/(max(x) - min(x))
dist_pol <- function(p, x = 0:(length(p)-1), alpha = 0) tibble(
 p1 = rep(p, each = length(p)), x1 = rep(x, each = length(p)),
 p2 = rep(p, length(p)), x2 = rep(x, length(p))) |> 
 mutate(antagonism = abs(x1-x2)) |> 
 summarize(polarization = 2^(1+alpha)*sum(p1^(1+alpha)*p2*antagonism)/(max(antagonism))) |> pull(polarization)




p = rep(1,100);p=p/sum(p)
 

dist_pol(p, alpha = 0)
dist_sd(p)
