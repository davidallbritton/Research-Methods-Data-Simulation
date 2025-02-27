# # temp
# 
# # Example data: a normally distributed vector
# ##  set.seed(123)
# x <- rnorm(10000, mean = 40, sd = 1)
# 
# 
# # # Create the ECDF function
# # F <- ecdf(x)
# # # You can also get the "quantile-like" values for every data point
# # quantile_ranks <- F(x)
# # head(quantile_ranks)
# # y <- scales::rescale(x, to = c(-1, 1))
# # # stretch the distribution out on the right and compress it on the left
# # y <- y * (.5 + quantile_ranks)
# 
# 
# 
# y <- scales::rescale(x, to = c(0, 2.5))
# y <- exp(y)
# 
# 
# y <- scales::rescale(y, to = c(100, 1000))

tempdf <- read.csv("/Users/dallbrit/Downloads/simulated_data (4).csv")
x <- tempdf$DV
y <- tempdf$DV_rt

# Compare distributions
par(mfrow = c(1, 3))
hist(x, main = "Original Distribution", col = "lightblue", border = "white", breaks=33)
hist(y, main = "Transformed Distribution", col = "salmon", border = "white", breaks=33)
plot(x,y)

seq(.5,7.5,1)

# length(y)
# plot(sort(rnorm(length(x))), sort(x))
# plot(sort(rnorm(length(y))), sort(y))
# plot(sort(x), sort(y))
# 
# plot(sort(rnorm(length(y))), sort(log(y)))
# plot(sort(rnorm(1000)), sort(rnorm(1000)))

hist(y, main = "Transformed Distribution", col = "salmon", border = "white",
    breaks="Sturges")

