### regressions ties-around-home paper ###

# packages
library(dplyr)
library(data.table)
library(stargazer)


# import data
degree_tab <- fread("../data/degree_tab_top50.csv.gz")
clust_tab <- fread("../data/clust_tab_top50.csv.gz")
support_tab <- fread("../data/supp_tab_top50.csv.gz")
censusdata <-  fread("../data/census_for_regression.csv.gz")


# regression dataframe
regdf <- merge(degree_tab, censusdata, by = c("user_id", "cbsacode", "short_name"), all.x = TRUE, all.y = FALSE)
regdf <- merge(regdf, clust_tab, by = "user_id", all.x = TRUE, all.y = FALSE)
regdf <- merge(regdf, support_tab, by = "user_id", all.x = TRUE, all.y = FALSE)

# remove the few rows with empty tract_home (...)
regdf <- subset(regdf, tract_home != "")

# variable manipulation
regdf$log_income <- log10(regdf$income)
regdf$log_population <- log10(regdf$population)
regdf$BA_share <- regdf$education_bachelor / regdf$population
regdf$log_degree <- log(regdf$degree)


### models
# degree -- cummulative degree share in 10/5/1 km
#d1 <- lm(dcum10000_share ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
d1 <- lm(dcum10000_share ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(d1)

#d2 <- lm(dcum5000_share ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
d2 <- lm(dcum5000_share ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(d2)

#d3 <- lm(dcum1000_share ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
d3 <- lm(dcum1000_share ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(d3)

stargazer(d1, d2, d3, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")



# clustering -- closed triads in 10/5/1 km
#c1 <- lm(clust10000 ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
#c1 <- lm(clust10000 ~ log_degree + log_income + log_population + as.factor(cbsacode), data = regdf)
c1 <- lm(clust10000 ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(c1)

#c2 <- lm(clust5000 ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
c2 <- lm(clust5000 ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(c2)

#c3 <- lm(clust1000 ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
c3 <- lm(clust1000 ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(c3)

stargazer(c1, c2, c3, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")



# support -- supported ties in 10/5/1 km
#s1 <- lm(support10000 ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
s1 <- lm(support10000 ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(s1)

#s2 <- lm(support5000 ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
s2 <- lm(support5000 ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(s2)

#s3 <- lm(support1000 ~ log_income + BA_share + log_population + as.factor(cbsacode), data = regdf)
s3 <- lm(support1000 ~ log_income + log_population + as.factor(cbsacode), data = regdf)
summary(s3)

stargazer(s1, s2, s3, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")




# poor as dependent variable
p0 <- lm(poor ~ BA_share + log_population + as.factor(cbsacode), data = regdf)
summary(p0)

p1 <- lm(poor ~ dcum10000_share + BA_share + log_population + as.factor(cbsacode), data = regdf)
summary(p1)

p2 <- lm(poor ~ clust10000 + BA_share + log_population + as.factor(cbsacode), data = regdf)
summary(p2)

p3 <- lm(poor ~ support10000 + BA_share + log_population + as.factor(cbsacode), data = regdf)
summary(p3)

p4 <- lm(poor ~ dcum10000 + clust10000 + support10000 + BA_share + log_population + as.factor(cbsacode), data = regdf)
summary(p4)



p0 <- glm(poor ~ BA_share + log_population + as.factor(cbsacode), family=binomial(link='logit'), data = regdf)
summary(p0)

p1 <- glm(poor ~ dcum10000_share + BA_share + log_population + as.factor(cbsacode), family=binomial(link='logit'), data = regdf)
summary(p1)

p2 <- glm(poor ~ clust10000 + BA_share + log_population + as.factor(cbsacode), family=binomial(link='logit'), data = regdf)
summary(p2)

p3 <- glm(poor ~ support10000 + BA_share + log_population + as.factor(cbsacode), family=binomial(link='logit'), data = regdf)
summary(p3)

p4 <- glm(poor ~ dcum10000_share  + clust10000 + support10000 + BA_share + log_population + as.factor(cbsacode), family=binomial(link='logit'), data = regdf)
summary(p4)

###



