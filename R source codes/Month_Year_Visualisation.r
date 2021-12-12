library(dplyr)
library(ggplot2)
df <- read.csv("C:/Users/Faizan/Downloads/only_road_accidents_data_month2.csv")
View(df)
sum(is.na(df))
colnames(df)
states <- unique(df["STATE.UT"])
states <- as.vector(states[, 1])
dim(states)
states

years = seq(2001, 2014)

for (i in years) {
  d <- df %>%
    filter(YEAR == i)
  
  print(ggplot(d, aes(x = STATE.UT, y = TOTAL, )) +
    geom_col() + 
    ggtitle(i) + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
}

d <- df %>%
  group_by(STATE.UT) %>%
  dplyr::select(TOTAL, STATE.UT, YEAR) %>%
  summarise(TOTAL = sum(TOTAL))

d

ggplot(d, aes(x = STATE.UT, y = TOTAL)) +  
  geom_col() + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

d <- df %>%
  group_by(STATE.UT) %>%
  dplyr::select(TOTAL, STATE.UT, YEAR)

d

ggplot(d, aes(x = STATE.UT, y = TOTAL)) +  
  geom_col() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


by(df,df$TOTAL,summary)

for (state in states) {
  d <- df %>%
    filter(STATE.UT == state)
  
  print(ggplot(d, aes(x = YEAR, y = TOTAL))+
      geom_col()+
      ggtitle(state))
}

months = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

for (state in states) {
  d <- df %>%
    filter(STATE.UT == state)
  
  d <- data.frame(Month = months, "2001" = as.vector(t(d[1, c(3:14)])), "2002" = as.vector(t(d[2, c(3:14)])),"2003" = as.vector(t(d[3, c(3:14)])),"2004" = as.vector(t(d[4, c(3:14)])),"2005" = as.vector(t(d[5, c(3:14)])),"2006" = as.vector(t(d[6, c(3:14)])), "2007" = as.vector(t(d[7, c(3:14)])), "2008" = as.vector(t(d[8, c(3:14)])), "2009" = as.vector(t(d[9, c(3:14)])), "2010" = as.vector(t(d[10, c(3:14)])), "2011" = as.vector(t(d[11, c(3:14)])), "2012" = as.vector(t(d[12, c(3:14)])), "2013" = as.vector(t(d[13, c(3:14)])), "2014" = as.vector(t(d[14, c(3:14)])))
  
  d <- d %>%
    rowwise() %>%
    mutate(Total = sum(X2001, X2002, X2003, X2004, X2005, X2006, X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014))
  
  print(ggplot(d, aes(x = Month, y = Total)) +
    ggtitle(state) +
    geom_col())
}
