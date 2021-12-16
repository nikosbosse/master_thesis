source("functions_master_thesis.R")

# ================================================= # 
# ================================================= # 
## test that the Probability Integral Transformation PIT works and gives reasonable results

sim_true <- rnorm(100, 1:100)
sim_estim <- matrix(nrow = 5000, ncol = 100) 
for (j in 1:100){
	sim_estim[,j] <- rnorm(n = 5000, mean = sim_true[j])
}
u_test <- my_PIT(sim_true, sim_estim)
hist(u_test, breaks = 30, family = "Serif")



# mixed results. Talk to Sebastian


# ================================================= # 
# ================================================= # 
