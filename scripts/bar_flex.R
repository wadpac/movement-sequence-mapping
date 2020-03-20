# rewrite teh sequencing
bar_flex<- function(bouts_values,bouts_lengths,f, bts){
  dff<- df_generation(bouts_values,bouts_lengths)
  #print(c("values",bouts_values))
  #print(c("lengths",bouts_lengths))
  d=length(bouts_values)
  btss=bts*f
  #print(c("btss",btss))
  bouts_lengthss <- findInterval(bouts_lengths,btss, all.inside = F)
  barcodes= rep(0,d)
  for (k in 1:d){
    #print(c( bouts_values[k] ,bouts_lengthss[k]))
    barcodes[k]=dff$A[dff$Z == bouts_values[k] & dff$Y == bouts_lengthss[k]]
  }
  return(barcodes)
}
