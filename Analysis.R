library(readxl)
library(tidyverse)
library(dplyr)



# Importing data
df <- read.csv('Source/region_stats.csv')
glimpse(df)



# Converting string values to factors for further use
df1 <- df |> mutate(across(where(is_character), as.factor))
glimpse(df1)



# Total number of states based on majority personality categories
par(mar = c(bottom = 5, left = 5, top = 3, right = 5))
df1 |> select(psy_reg) |> table() |>sort(decreasing = T) |>
  barplot(main = 'Majority personalities of contiguous states',
          horiz = T,
          ylab = 'Majority personality categories',
          xlab = 'Number of states',
          xlim = c(0,25),
          col = colors(3))



# Region wise subset where each value represents relative percentage
# Contingency table is used here
table(df1$region, df1$psy_reg) |>
  prop.table() |> round(2)*100



# Plotting those subsets
df_temp <- df1 |> select(psy_reg, region) |> table()
df_temp|>barplot(main = 'Personalities of Contiguous states',
          horiz = T,
          ylab = 'Regionwise personality categories',
          xlab = 'Number of states',
          xlim = c(0,16),
          col = c('blue4', 'green4', 'red4'),
          legend = rownames(df_temp),
          beside = T)



# Analyzing popularity of each subject
par(mar = c(bottom = 5, left = 8, top = 3, right = 5))
df1[c('data_science','artificial_intelligence','machine_learning','data_analysis','business_intelligence','spreadsheet','statistics')] |>
  apply(2, as.numeric, na.rm = TRUE) |>
  colMeans() |> sort(decreasing = T) |>
  barplot(horiz = T,
        xlim = c(0,100),
        las=2, cex.names = 0.8,
        main = 'Popularity of each subject by Google trends score',
        xlab = 'Total popularity score')
# Spreadsheet is the most popular



# Density plot of spreadsheet score 
par(mar = c(bottom = 5, left = 5, top = 3, right = 3))
df_temp <- df1 |> pull(spreadsheet) |> as.numeric() |> density()
df_temp |> plot(
    main = "Google trends search score for \'Spreadsheet\'",
    xlab = "Search score for \'Spreadsheet\'",
    ylab   = "Total frequency of all states")
df_temp |> polygon(col = 'cyan4')
# Most scores are between 60 and 70



# Google trends score for entertainment category
par(mar = c(bottom = 3, left = 5, top = 3, right = 3))
df1 |>
  select(art:museum) |>
  boxplot(main='Entertainment categories',
          ylab='Score')
# category of museum has more outliers



# getting more insights on museum
par(mar = c(bottom = 5, left = 3, top = 2, right = 3))
df1 |> select(museum) |>
  boxplot(
  horizontal = T,
  main = 'Museum trend boxplot for outliers',
  xlab = 'Trend score for museum',
  col = 'maroon2',
  notch = T)
fivenum(df6$museum)

# Checking those outliers
df1 |> filter(museum>37) |> select(state, museum) |> arrange(desc(museum))
# Museum is more famous in New York and Massachusetts



# Checking if there is any correlation between different sports
df1 |> select(basketball:hockey) |> plot(main='Relation between different sports')
# there some relation between popularity of baseball and football



# Getting more insights in those two games
par(mar = c(bottom = 5, left = 5, top = 2, right = 3))
df1 |> select(football,baseball) |> plot(main='Relation between football and baseball',
                                         ylab='baseball')
lm(df1$football~df1$baseball) |> abline()
# these two games have good relation in search scores



# Checking which states are closely related based on games trend score
df_temp2 <- df1 |> select(basketball:hockey) |> dist() |> hclust()
df_temp2 |> plot(labels = df1$state,
       main = 'Clustering of states based on similar sports trend scores',
       xlab = 'States',
       ylab = 'Score',
       sub = '')
df_temp2 |> rect.hclust(k=5, border = 5:11)



# Checking for any relation between trend scores of different subjects
df_temp3 <- df1 |> select(
  DS = data_science,  # New = old
  AI = artificial_intelligence,
  ML = machine_learning,
  DA = data_analysis,
  BI = business_intelligence,
  SS = spreadsheet,
  Stats = statistics)
plot(df_temp3, main='Relation between subjects')
cor(df_temp3) |> round(2)
# there is very good correlation between DS, AI, ML, DA



# Checking for any relation between subjects like DS and population traits
df_temp4 <- df1 |> select(data_science, extraversion:openness)
plot(df_temp4, main='Relation between DS and traits')
lm(df_temp4)

df_temp4 |> select(data_science,openness) |> plot(main='Relation between DS and Openness')
lm1 <- lm(df1$data_science~df1$openness) |> abline()

