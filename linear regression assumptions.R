library(tidyverse)
library(broom)
theme_set(theme_classic())

# Load the data
data("marketing", package = "datarium")
# Inspect the data
sample_n(marketing, 3)

model <- lm(sales ~ youtube, data = marketing)
model

model.diag.metrics <- augment(model)
head(model.diag.metrics)

ggplot(model.diag.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 0.3)

par(mfrow = c(2, 2))
plot(model)

library(ggfortify)
autoplot(model)

# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

plot(model, 1)

plot(model, 3)

model2 <- lm(log(sales) ~ youtube, data = marketing)
plot(model2, 3)

plot(model, 2)

plot(model, 5)

# Cook's distance
plot(model, 4)
# Residuals vs Leverage
plot(model, 5)

plot(model, 4, id.n = 5)

model.diag.metrics %>%
  top_n(3, wt = .cooksd)

df2 <- data.frame(
  x = c(marketing$youtube, 500, 600),
  y = c(marketing$sales, 80, 100)
)
model2 <- lm(y ~ x, df2)

# Cook's distance
plot(model2, 4)
# Residuals vs Leverage
plot(model2, 5)



