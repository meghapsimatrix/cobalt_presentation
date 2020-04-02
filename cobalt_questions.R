library(cobalt)
library(tidyverse)

# Fitting ps model 
covs <- lalonde %>% 
  select(-1)

f_lin <- f.build("treat", covs)

ps_logit <- glm(f_lin, data = lalonde, family = "binomial")
lalonde$ps <- predict(ps_logit, type = "response")

# calculate the weights - ATT weighting by odds of treatment
lalonde <- lalonde %>%
  mutate(att_wt = treat + (1 - treat) * ps/(1 - ps))

# bal tab

b_w1 <- bal.tab(f_lin, data = lalonde, weights = "att_wt", 
                method = "weighting", estimand = "ATT", 
                disp.v.ratio = TRUE, un = TRUE)

# alpha works in love.plot
love.plot(b_w1, threshold = .1, colors = c("red", "blue"),
          size = 5, alpha = .7, stars = "raw") + 
  theme_bw()


# but it doesn't in bal.plot. Also there is some warning message. 
bal.plot(f_lin, data = lalonde, weights = "att_wt", 
         method = "weighting", estimand = "ATT",
         var.name = "race", which = "both",
         alpha = .2, position = "fill") + # alpha doesn't seem to work?  and position doens't work 
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()

lalonde <- lalonde %>%
  mutate(trt = if_else(treat == 1, "treatment", "control"))

# I am trying to get something like this to see the proportion more easily?
ggplot(lalonde, aes(x = trt, fill = race)) +
  geom_bar(position = "fill") + 
  theme_bw()

ggplot(lalonde, aes(x = race, fill = trt)) +
  geom_bar(position = "fill") + 
  theme_bw()
