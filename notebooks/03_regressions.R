### regressions -- ties-around-home ###

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
regdf$rich <- (regdf$poor-1)*(-1)


### Figure 4 -- regressions
cbsalist <- unique(regdf$cbsacode)
icoeff_deg <- c()
icoeff_clust <- c()
icoeff_supp <- c()
ise_deg <- c()
ise_clust <- c()
ise_supp <- c()

# loop to run the same model for different cities and store log_income coefficients
for (c in 1:length(cbsalist))
{
  model_df <- subset(regdf, cbsacode==cbsalist[c])
  md <- summary(lm(dcum10000_share ~ rich + log_population, data = model_df))
  icoeff_deg[c] <- (data.frame(md$coefficients)[2,1])
  ise_deg[c] <- (data.frame(md$coefficients)[2,2])
  
  mc <- summary(lm(clust10000 ~ rich + log_population, data = model_df))
  icoeff_clust[c] <- (data.frame(mc$coefficients)[2,1])
  ise_clust[c] <- (data.frame(mc$coefficients)[2,2])
  
  ms <- summary(lm(support10000 ~ rich + log_population, data = model_df))
  icoeff_supp[c] <- (data.frame(ms$coefficients)[2,1])
  ise_supp[c] <- (data.frame(mc$coefficients)[2,2])
}

# create a common data.frame
coeff_df <- data.table(cbsalist, icoeff_deg, ise_deg, icoeff_clust, ise_clust, icoeff_supp, ise_supp)

# add color col
coeff_df$col_deg <- ifelse(coeff_df$icoeff_deg>0, "darkblue", "grey")
coeff_df$col_clust <- ifelse(coeff_df$icoeff_clust>0, "darkblue", "grey")
coeff_df$col_supp <- ifelse(coeff_df$icoeff_supp>0, "darkblue", "grey")

# save
write.table(coeff_df, file="../data/coeff_df.csv", row.names=FALSE, col.names=TRUE, sep=";")
###


### Table 2 in main text -- inside 10 km
m1 <- lm(dcum10000_share ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
m2 <- lm(clust10000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
m3 <- lm(support10000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)

stargazer(m1, m2, m3, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")
###


### Table S1 -- supplementary

# degree -- cummulative degree share in 10/5/1 km
d1 <- lm(dcum10000_share ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
d2 <- lm(dcum5000_share ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
d3 <- lm(dcum3000_share ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
d4 <- lm(dcum1000_share ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)

stargazer(d1, d2, d3, d4, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")
###


### Table S2 -- supplementary

# clustering -- closed triads in 10/5/1 km
c1 <- lm(clust10000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
c2 <- lm(clust5000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
c3 <- lm(clust3000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
c4 <- lm(clust1000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)

stargazer(c1, c2, c3,c4, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")
###

### Table S3 -- supplementary
# support -- supported ties in 10/5/1 km
s1 <- lm(support10000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
s2 <- lm(support5000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
s3 <- lm(support3000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)
s4 <- lm(support1000 ~ log_income + log_population + degree + as.factor(cbsacode), data = regdf)

stargazer(s1, s2, s3, s4, omit = c("cbsacode"), omit.labels = ("Metro FE"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt")
###
