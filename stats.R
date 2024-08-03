# plot binom pdf,cdf ----

# Load necessary libraries
library(ggplot2)
library(patchwork)

# Define the parameters
n <- 10
ps <- c(0.5, 0.3, 0.6)

# Create a data frame to hold the data for plotting
df <- data.frame()

for (p in ps) {
    # Calculate the PMF and CDF for each p
    x <- 0:n
    pmf <- dbinom(x, size = n, prob = p)
    cdf <- pbinom(x, size = n, prob = p)
    
    # Add the data to the data frame
    df <- rbind(df, data.frame(x = x, pmf = pmf, cdf = cdf, p = factor(p)))
}

# Plot the PMF using ggplot2
pmf_plot <- ggplot(df, aes(x = x, y = pmf, fill = p)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    labs(title = "PMF of Binomial Distribution",
         x = "Number of successes",
         y = "Probability",
         fill = "Probability p") +
    theme_minimal()

# Plot the CDF using ggplot2
cdf_plot <- ggplot(df, aes(x = x, y = cdf, color = p, group = p)) +
    geom_line(size = 1) +
    labs(title = "CDF of Binomial Distribution",
         x = "Number of successes",
         y = "CDF",
         color = "Probability p") +
    theme_minimal()

# Combine the plots using patchwork
pmf_plot + cdf_plot + plot_layout(ncol = 1)

# plot prob for failure (binomial) at various n ----

# Load necessary libraries
library(ggplot2)
library(patchwork)

# Define the parameters
p <- 0.5
ns <- c(1, 2,5,10,20)

# Calculate P(X = 0) = (1 - p)^n for each n
results <- data.frame(n = ns, P_X_0 = (1 - p)^ns)

# Plot on a linear scale
linear_plot <- ggplot(results, aes(x = n, y = P_X_0)) +
    geom_point() +
    geom_line() +
    labs(title = "P(X = 0) on Linear Scale",
         x = "Number of Trials (n)",
         y = "P(X = 0)") +
    theme_minimal()

# Plot on a logarithmic scale
log_plot <- ggplot(results, aes(x = n, y = P_X_0)) +
    geom_point() +
    geom_line() +
    scale_y_log10() +
    labs(title = "P(X = 0) on Log Scale",
         x = "Number of Trials (n)",
         y = "P(X = 0) (Log Scale)") +
    theme_minimal()

# Combine the plots using patchwork
linear_plot + log_plot + plot_layout(ncol = 1)

# exercise 5.18 ----

# problem a
x = 12
n = 20
p = 0.4

cdf <- pbinom(x, size = n, prob = p)

cdf

# problem b
x = 9
n = 16
p = 0.5

cdf <- pbinom(x, size = n, prob = p)

1-cdf


# expected value for geometric ----

# Set parameters
p <- 0.5 # Probability of success

# Define the range of k (number of trials)
k <- 0:20

# Calculate the cumulative mean of the geometric distribution
cumulative_mean <- cumsum(dgeom(k, prob = p) * (k + 1)) / cumsum(dgeom(k, prob = p))

# Calculate the expected value of the geometric distribution
expected_value <- 1 / p

# Create a data frame with cumulative mean values
data <- data.frame(
    k = k,
    cumulative_mean = cumulative_mean
)

# Plot the change in cumulative mean value
ggplot(data, aes(x = k, y = cumulative_mean)) +
    geom_point(color = "blue", size = 3) +
    geom_line(color = "blue") +
    geom_hline(yintercept = expected_value, color = "red", linetype = "dashed") +
    annotate("text", x = max(k), y = expected_value + 0.1, 
             label = paste("Expected Value:", round(expected_value, 2)), 
             color = "red", hjust = 1) +
    labs(title = "Convergence to Expected Value in Geometric Distribution",
         x = "Number of Trials (k)",
         y = "Cumulative Mean Value",
         caption = paste("Expected Value: E(X) =", round(expected_value, 2))) +
    theme_minimal()



# PMF geometric distribution ---------------
# Set parameters
k <- 0:10  # Range of k values
p_values <- c(0.1, 0.3, 0.5, 0.7)  # Different probability values

# Create a data frame to store PMF values
data <- expand.grid(k = k, p = p_values)
data$pmf <- mapply(function(k, p) dgeom(k, prob = p), data$k, data$p)

# Plot the PMF for different values of p
ggplot(data, aes(x = k, y = pmf, color = factor(p))) +
    geom_line() +
    geom_point() +
    labs(title = "PMF of Geometric Distribution for Different Probabilities",
         x = "Number of Trials (k)",
         y = "Probability Mass Function",
         color = "Probability (p)") +
    theme_minimal()

# PDF- binomial ~ normal ---------------

# Load necessary library
library(ggplot2)

# Function to plot binomial and normal distributions
plot_binomial_normal <- function(n, p) {
    q <- 1 - p
    mean <- n * p
    sd <- sqrt(n * p * q)
    
    # Generate binomial probabilities
    x <- 0:n
    binom_probs <- dbinom(x, size = n, prob = p)
    
    # Generate normal approximation
    normal_approx <- dnorm(x, mean = mean, sd = sd)
    
    # Data frame for plotting
    df <- data.frame(
        x = rep(x, 2),
        Probability = c(binom_probs, normal_approx),
        Distribution = rep(c("Binomial", "Normal Approximation"), each = length(x))
    )
    
    # Plotting
    ggplot(df, aes(x = x, y = Probability, color = Distribution)) +
        geom_line(size = 1) +
        labs(title = paste("Binomial vs Normal Approximation (n =", n, ", p =", p, ")"),
             x = "Number of Successes",
             y = "Probability") +
        theme_minimal()
}

# Set number of trials
n <- 30

# Plot for different values of p
plot_binomial_normal(n, p = 0.1)
plot_binomial_normal(n, p = 0.3)
plot_binomial_normal(n, p = 0.5)
plot_binomial_normal(n, p = 0.7)
plot_binomial_normal(n, p = 0.9)


# Criteria for binomial ~ normal ------

    # Load necessary library
    library(ggplot2)

# Function to plot the ratio np/(n(1-p)) against p with detailed y-axis ticks
plot_ratio_detail_ticks <- function(n) {
    # Generate values for p from 0.01 to 0.99 (excluding 0 and 1 to avoid division by zero)
    p_values <- seq(0.01, 0.99, by = 0.01)
    
    # Calculate np and n(1-p) for each p
    np <- n * p_values
    n1_p <- n * (1 - p_values)
    
    # Calculate the ratio np/(n(1-p))
    ratio <- np / n1_p
    
    # Data frame for plotting
    df <- data.frame(
        p = p_values,
        Ratio = ratio
    )
    
    # Plotting with detailed y-axis ticks
    ggplot(df, aes(x = p, y = Ratio)) +
        geom_line(size = 1, color = "blue") +
        geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
        scale_y_continuous(
            limits = c(-0.5, 10),  # Setting y-axis limits
            breaks = seq(-0.5, 10, by = 0.5)  # Detailed tick marks every 0.5 units
        ) +
        labs(title = paste("Ratio of np to n(1-p) for n =", n),
             x = "Probability of Success (p)",
             y = "Ratio np / (n(1-p))") +
        theme_minimal()
}

# Set number of trials
n <- 30

# Plot the ratio with detailed y-axis ticks for the given value of n
plot_ratio_detail_ticks(n)
