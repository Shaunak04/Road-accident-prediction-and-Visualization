df <- read.csv("C:\\Users\\Faizan\\Desktop\\final.csv")
View(df)
dim(df)
for(i in 3:42) {
  col <- df[[i]]
  min <- quantile(col, 0)
  a <- quantile(col, 0.1428)
  b <- quantile(col, 0.2856)
  c <- quantile(col, 0.4285)
  d <- quantile(col, 0.5714)
  e <- quantile(col, 0.7142)
  f <- quantile(col, 0.8571)
  max <- quantile(col, 1)
  new = c()
  for(x in col) {
    if(x>= min && x<a) {
      new <- append(new, 1)
    }
    else if(x>= a && x<b) {
      new <- append(new, 2)
    }
    else if(x>= b && x<c) {
      new <- append(new, 3)
    }
    else if(x>= c && x< d) {
      new <- append(new, 4)
    }
    else if(x>= d && x< e) {
      new <- append(new, 5)
    }
    else if(x>= e && x< f) {
      new <- append(new, 6)
    }
    else if(x>= f && x<= max) {
      new <- append(new, 7)
    }
  }
  df[[i]] <- new
}

View(df)
write.csv(df, "C:\\Users\\Faizan\\Desktop\\final_severity.csv", row.names = FALSE)
