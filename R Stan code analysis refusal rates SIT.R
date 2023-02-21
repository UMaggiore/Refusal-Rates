## load library and import the dataset
library(rstanarm)
library(bayesplot)
library(brms)  
library(tidyverse)
library(haven)
df <- read_dta("C:/Documenti/GrossiAA/DATI PROGETTO ME TOO/DATI ICU - SIT (OK)/datset_oppositions.dta")

## fit the two models
# Model for Ethnicity
mr.e <- stan_glmer(DX_WILLINGNESSTODONATE  ~ (1 | DX_REGRESIDENCY) +
             (1 | DX_ETHNICITY), 
            data = df, 
            family = binomial) 

# Model for Country
mr.c <- stan_glmer(DX_WILLINGNESSTODONATE  ~ (1 | DX_REGRESIDENCY) +
                    (1 | DX_COUNTRYOFBIRTH), 
                  data = df, 
                  family = binomial) 


# Ehnicity: check summary and diagnostic using shinystan
summary(mr.e)
launch_shinystan(mr.e)

# Ethnicity: Extract summary of the posterior and prepare the datset for plotting
res.mr.e <- summary(mr.e, probs = c(0.025, 0.975))
m.re <-   res.mr.e[c(23:28), 1]
lb.re <-  res.mr.e[c(23:28), 4]
ub.re <-  res.mr.e[c(23:28), 5]
names <- row.names(res.mr.e)[c(23:28)]
names <- recode(names, 
                "b[(Intercept) DX_ETHNICITY:1]" = "EU-born",
                "b[(Intercept) DX_ETHNICITY:2]" = "Eastern European",
                "b[(Intercept) DX_ETHNICITY:3]" = "Asian",
                "b[(Intercept) DX_ETHNICITY:4]"= "Hispanic",
                "b[(Intercept) DX_ETHNICITY:5]"= "African" ,
                "b[(Intercept) DX_ETHNICITY:6]"= "North Africa & Middle East")

m.re = exp(m.re) / ( 1 + exp(m.re)) * 100
lb.re =   exp(lb.re) / ( 1 + exp(lb.re)) * 100
ub.re =   exp(ub.re) / ( 1 + exp(ub.re)) * 100

db.re <- data.frame(cbind(names, m.re, lb.re, ub.re))
rm(m.re, lb.re, ub.re)

db.re <- db.re %>%
  select(-names) %>%
  mutate_if(is.character,as.numeric)

# Etnicity: make the plot
db.re$title <- "Refusal to Donation by Ethnicity"
ggplot(data=db.re, aes(x = m.re, 
                      xmin = lb.re, xmax = ub.re, 
                      y= reorder(names, -m.re), 
)) +
  geom_point(pch = 21, size = 3, 
             color = c(rgb(72,108,140,maxColorValue = 255))) + 
  geom_errorbarh(height=0.1, color = c(rgb(72,108,140,maxColorValue = 255))) +
  labs(x = "(%)", y = NULL )  +
  scale_x_continuous(breaks = seq(10,100, by =10)) +
  geom_vline(xintercept=db.re$m.re[1], 
             color= c(rgb(144,53,59,maxColorValue = 255)), 
             linetype='dashed', alpha=.5) + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 13)
  ) +
  facet_grid(. ~ title) 
ggsave("rstan_re_ethinicty.png", dpi=600, width = 8, height = 8, units = "in")
ggsave("rstan_re_ethinicty.pdf", dpi=600, width = 8, height = 8, units = "in")

# Country: check summary and diagnostic using shinystan
summary(mr.c)
launch_shinystan(mr.c)

# Country : Extract summary of the posterior and prepare the datset for plotting
res.mr.c <- summary(mr.c, probs = c(0.025, 0.975))
m.rc <-   res.mr.c[c(2:25), 1]
lb.rc <-  res.mr.c[c(2:25), 4]
ub.rc <-  res.mr.c[c(2:25), 5]
names <- row.names(res.mr.c)[c(2:25)]
names <- recode(names, 
                "b[(Intercept) DX_COUNTRYOFBIRTH:3]" = "Albania",
                "b[(Intercept) DX_COUNTRYOFBIRTH:10]" = "Bangladesh",
                "b[(Intercept) DX_COUNTRYOFBIRTH:16]" = "Brazil",
                "b[(Intercept) DX_COUNTRYOFBIRTH:23]"= "China",
                "b[(Intercept) DX_COUNTRYOFBIRTH:35]"= "Egypt" ,
                "b[(Intercept) DX_COUNTRYOFBIRTH:40]"= "Philippines",
                "b[(Intercept) DX_COUNTRYOFBIRTH:42]"= "France",
                "b[(Intercept) DX_COUNTRYOFBIRTH:45]"= "Germany",
                "b[(Intercept) DX_COUNTRYOFBIRTH:46]"= "Ghana",
                "b[(Intercept) DX_COUNTRYOFBIRTH:53]"= "India",
                "b[(Intercept) DX_COUNTRYOFBIRTH:60]"= "Italy",
                "b[(Intercept) DX_COUNTRYOFBIRTH:74]"= "Morocco",
                "b[(Intercept) DX_COUNTRYOFBIRTH:76]"= "Moldavia",
                "b[(Intercept) DX_COUNTRYOFBIRTH:81]"= "Nigeria",
                "b[(Intercept) DX_COUNTRYOFBIRTH:84]"= "Pakistan",
                "b[(Intercept) DX_COUNTRYOFBIRTH:85]"= "Peru'",
                "b[(Intercept) DX_COUNTRYOFBIRTH:86]"= "Poland",
                "b[(Intercept) DX_COUNTRYOFBIRTH:87]"= "United Kingdom",
                "b[(Intercept) DX_COUNTRYOFBIRTH:91]"= "Romania",
                "b[(Intercept) DX_COUNTRYOFBIRTH:96]"= "Senegal",
                "b[(Intercept) DX_COUNTRYOFBIRTH:106]"= "Sri Lanka",
                "b[(Intercept) DX_COUNTRYOFBIRTH:111]"= "Switzerland",
                "b[(Intercept) DX_COUNTRYOFBIRTH:118]"= "Tunisia",
                "b[(Intercept) DX_COUNTRYOFBIRTH:120]"= "Ukraine"
                  )


m.rc = exp(m.rc) / ( 1 + exp(m.rc)) * 100
lb.rc =   exp(lb.rc) / ( 1 + exp(lb.rc)) * 100
ub.rc =   exp(ub.rc) / ( 1 + exp(ub.rc)) * 100

db.rc <- data.frame(cbind(names, m.rc, lb.rc, ub.rc))
rm(m.rc, lb.rc, ub.rc)

db.rc <- db.rc %>%
  select(-names) %>%
  mutate_if(is.character,as.numeric)

# Country: make the plot
db.rc$title <- "Refusal to Donation by Country"
ggplot(data=db.rc, aes(x = m.rc, 
                       xmin = lb.rc, xmax = ub.rc, 
                       y= reorder(names, -m.rc), 
)) +
  geom_point(pch = 21, size = 3, 
             color = c(rgb(72,108,140,maxColorValue = 255))) + 
  geom_errorbarh(height=0.1, color = c(rgb(72,108,140,maxColorValue = 255))) +
  labs(x = "(%)", y = NULL )  +
  scale_x_continuous(breaks = seq(10,100, by =10)) +
  geom_vline(xintercept=db.rc$m.rc[11], 
             color= c(rgb(144,53,59,maxColorValue = 255)), 
             linetype='dashed', alpha=.5) + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 13)
  ) +
  facet_grid(. ~ title) 
ggsave("rstan_re_country.png", dpi=600, width = 8, height = 8, units = "in")
ggsave("rstan_re_country.pdf", dpi=600, width = 8, height = 8, units = "in")



