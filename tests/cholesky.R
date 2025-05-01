sigma=c(2,3,5)
sigma_diag=diag(sigma,3,3) 
sigma_v=matrix(sigma,nrow=3)
corr = matrix(c(1,0.5,0.2,0.5,1,0.3,0.2,0.3,1),byrow=FALSE,nrow=3)
cova = sigma_diag %*% corr %*% sigma_diag
cova
sigma=matrix(sigma,nrow=3)

#cholesky
L_t = chol(corr)
L=t(L_t)
L%*%L_t


sigma=matrix(c(2,3,5),nrow=3)
L%*%L_t %*% sigma%*%t(sigma)

L %*% t(sigma_diag%*%sigma_diag) %*% L_t

sigma_diag%*%L %*% t(sigma_diag%*%L)












# Load necessary libraries
install.packages("MASS")  # For mvrnorm
install.packages("rgl")   # For 3D plotting
library(MASS)
library(rgl)

# Set parameters
n_samples <- 1000
mean <- c(0, 0, 0)  # Mean vector for the multivariate normal distribution

# Covariance matrix: rho=0.95 for X1-X2, X1-X3, and rho=0 for X2-X3
rho_seq=seq(0,1,by=0.01)
rho0_seq=seq(0.6,1,by=0.01)
df=data.frame()
for(i in 1:length(rho_seq)){
  for(j in 1:length(rho0_seq)){
    cov_matrix <- matrix(c(1, rho0_seq[j], rho0_seq[j],
                           rho0_seq[j], 1, rho_seq[i],
                           rho0_seq[j], rho_seq[i], 1), nrow = 3)
    df=rbind(df,
               data.frame(rho=rho_seq[i],
                          rho0=rho0_seq[j],
                          min_eigen=min(eigen(cov_matrix)$values)))
  }
}
df %>% #filter(rho0==0.9) %>% 
  ggplot(aes(x=rho,y=min_eigen,col=factor(rho0))) +
  geom_point()+
  geom_hline(yintercept = 0)+
  xlim(0,1)

df %>% group_by(rho0) %>% 
  dplyr::summarise(min_rho = min(rho[min_eigen>=0]),
                   max_rho = max(rho[min_eigen>=0])) %>% 
  ggplot(aes(x=rho0,y=min_rho,ymin=min_rho,ymax=max_rho))+
  geom_pointrange()

# Generate multivariate normal samples
set.seed(42)  # For reproducibility
rho0=0.7

rho=0
cov_matrix <- matrix(c(1, rho0, rho0,
                       rho0, 1, rho,
                       rho0, rho, 1), nrow = 3)
data1 <- mvrnorm(n_samples, mu = mean, Sigma = cov_matrix)
rho=0.99
cov_matrix <- matrix(c(1, rho0, rho0,
                       rho0, 1, rho,
                       rho0, rho, 1), nrow = 3)
data2 <- mvrnorm(n_samples, mu = mean, Sigma = cov_matrix)

# Convert the data to a data frame for easier manipulation
df <- data.frame(X1 = data1[, 1], X2 = data1[, 2], X3 = data1[, 3])
df <- data.frame(X1 = data2[, 1], X2 = data2[, 2], X3 = data2[, 3])

# 3D Scatter Plot
# Create 3D scatter plot
plot3d(df$X1, df$X2, df$X3, col = rainbow(n_samples), size = 3, type = 's',
       xlab = "X1", ylab = "X2", zlab = "X3", main = "3D Scatter Plot of (X1, X2, X3)")

# 2D Scatter Plots
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))  # Set up a 1x3 grid for the plots

# X1 vs X2
plot(df$X1, df$X2, col = 'blue', pch = 16, main = "Projection: X1 vs X2", xlab = "X1", ylab = "X2")

# X1 vs X3
plot(df$X1, df$X3, col = 'green', pch = 16, main = "Projection: X1 vs X3", xlab = "X1", ylab = "X3")

# X2 vs X3
plot(df$X2, df$X3, col = 'red', pch = 16, main = "Projection: X2 vs X3", xlab = "X2", ylab = "X3")

