# tolearace normal
tor_flex_constant<- function(bouts_values, bouts_lengths, allow_bout,
                             ttt1, ttt2, f){
  d=length(bouts_lengths)
  index=1:d
  index=index[bouts_values==allow_bout] #vector with indices bouts (if 4 --> MPA bouts)
  index=index[!is.na(index)]
  test=0
  dd<- length(index) 
  i=1
  if(dd>=2){
    while(i<=(dd-1)){ 
      flag=0
      total_time=bouts_lengths[index[i]]
      tolerance_time=0
      tt=i
      if (total_time>ttt2){i=i+1}
      while(flag==0 & total_time<= ttt2 & i<=(dd-1)) {
        total_time1=total_time
        t1<- bouts_lengths[(index[i]+1):(index[i+1])] #kijk in de data tussen de bouts met 4
        t2<- bouts_values[(index[i]+1):(index[i+1])]
        cur_tor_low=sum(t1[t2<allow_bout])
        cur_tor_upp=sum(t1[t2>allow_bout])
        total_time=total_time+ sum(bouts_lengths[(index[i]+1):(index[i+1])])
        tolerance_time1=tolerance_time
        tolerance_time=tolerance_time+ cur_tor_low+cur_tor_upp
        if(total_time1>=ttt1  &  tolerance_time1<total_time1*0.1 & (total_time>ttt2 |tolerance_time>total_time*0.1 | cur_tor_low> 3*f) | (total_time1>=ttt1  &   tolerance_time<total_time1*0.1 & total_time<ttt2 & i==(dd-1))){
          if(i>=2){
            bouts_lengths[index[tt]]=sum(bouts_lengths[(index[tt]):(index[i])])
            bouts_lengths[(index[tt]+1):(index[i])]=-1
          }
          if(i==1)  {
            print(i)
            bouts_lengths[index[1]]=sum(bouts_lengths[(index[1]):(index[2])])
            bouts_lengths[(index[1]+1):(index[2])]=-1
            #falg=1
          }
        }
        if( total_time>ttt2 | tolerance_time> total_time*0.1 |cur_tor_low>3*f){
          flag=1
        }
        i=i+1   
      }
    }
    bouts_values=bouts_values[bouts_lengths!=-1]
    bouts_lengths=bouts_lengths[bouts_lengths!=-1]
  }
  dd=length(bouts_values)
  tor<- list(values=bouts_values, lengths=bouts_lengths)
  return(tor)
}
