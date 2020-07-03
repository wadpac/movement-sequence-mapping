# Generate the sequence mapping
df_generation_all <- function(bouts_values, bouts_lengths) {
  df <- data.frame(Z = bouts_values, Y =  bouts_lengths, A = 0:30)
  df$A <- NA # Empty out A
  # Re-create A
  df$A[df$Z == 0 & df$Y == 1] <- 0
  df$A[df$Z == 0 & df$Y == 2] <- 0
  df$A[df$Z == 0 & df$Y == 3] <- 0
  df$A[df$Z == 0 & df$Y == 4] <- 0
  
  df$A[df$Z == 1 & df$Y == 1] <- 4
  df$A[df$Z == 1 & df$Y == 2] <- 3
  df$A[df$Z == 1 & df$Y == 3] <-2
  df$A[df$Z == 1 & df$Y == 4] <- 1
  
  df$A[df$Z == 2 & df$Y == 1] <- 5
  df$A[df$Z == 2 & df$Y == 2] <- 6
  df$A[df$Z == 2 & df$Y == 3] <-7
  df$A[df$Z == 2 & df$Y == 4] <-8
  
  df$A[df$Z == 3 & df$Y == 1] <- 9
  df$A[df$Z == 3 & df$Y == 2] <- 10
  df$A[df$Z == 3 & df$Y == 3] <- 11
  df$A[df$Z == 3 & df$Y == 4] <- 12

  # df$A[df$Z == 4 & df$Y == 1] <- 13
  # df$A[df$Z == 4 & df$Y == 2] <- 14
  # df$A[df$Z == 4 & df$Y ==3] <- 15
  # df$A[df$Z == 4 & df$Y ==4] <- 16
  # 
  # df$A[df$Z == 5 & df$Y == 1] <- 17
  # df$A[df$Z == 5 & df$Y == 2] <- 18
  # df$A[df$Z == 5 & df$Y ==3] <- 19
  # df$A[df$Z == 5 & df$Y ==4] <- 20

  df <- df[order(df$A), ]
  return(df)
}
