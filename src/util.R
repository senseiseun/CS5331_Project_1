#data cleansing

reduce_outliers <- function(dt, col) {
  quartiles <- quantile(dt[,col], probs = c(0,0.25,0.5,0.75,1));
  print(quartiles)
  IQR <-  (quartiles[["75%"]] - quartiles[["25%"]]) * 1.5
  print(IQR)
  outlier_max <- quartiles[["75%"]] + IQR
  outlier_min <- quartiles[["25%"]] + IQR
  print(outlier_max)
  new_dt <- dt[dt[,col] <= outlier_max]
  new_dt <- new_dt[new_dt[,col] >= 2]
  return(new_dt)
}

remove_duplicates <- function(dt) {
  library("r2r")
  set <- hashset()
  new_df <- dt[0,]
  for (i in 1:nrow(dt)) {
    str_rep <- toString(dt[i,])
    print(str_rep)
    if (!has_key(set, str_rep)) {
      insert(set, str_rep)
      new_df <- rbind(new_df, dt[i,])
    }
  }
  return(new_df)
}

test_reduce_outliers <- function() {
  df <- data.frame (name  = c("Seun Suberu", "Jasmine Patrick", "Julian Brown"),
                    birth_year = c(2001, 2009, 2001),
                    birth_month = c("April", "November", "January"),
                    birth_day = c(6, 27, 2))
  df <- remove_duplicates(df)
  df <- reduce_outliers(df, "birth_year")
  View(df)
}

test_reduce_outliers();