# new sequencing
bouts_length_filter <- function(seqss, timeline, file_name, 
                                epoch, validdays, mimwear, value, bts){
  x <- seqss
  vl <- length(value)
  collapse.factor=substring(timeline,1, 10)
  ucf <- unique(collapse.factor)
  nucf <- length(ucf) #number of unique days in aggregated values
  
  tolerance=1
  new_seq=NULL
  
  days=0
  f=60/epoch
  #print(f)
  #value=c(0,1,pacut)/(60/epoch)
  value=value/f #cutpoints for the specified (epoch length of aggregated values) epoch length
  tolerance=tolerance*(60/epoch)
  #bouts$lengths <- bouts$lengths/f
  length_code1=0
  length_code2=0
  long_barcoding=NULL
  short_barcoding=NULL
  long_barcoding_length=NULL
  short_barcoding_length=NULL
  complexiy=NULL
  ucfs=NULL   
  vd=rep(0,7)
  l1=0
  l2=0
  for (j in 1:nucf) {
    
    x.sub <- x[collapse.factor == ucf[j]]
    z <- findInterval(x.sub, vec = value/f, all.inside = F)
    bouts <- rle(z)
    
    wertime=length(x.sub)
    noweattime=sum( bouts$lengths[ bouts$length>=60*f &  bouts$values==1]) #non-wear time is => 60 minutes consequetive sedentary behavior
    wertime=wertime-  noweattime
    tt1= bouts$values[bouts$lengths<60*f]
    tt2=bouts$lengths[bouts$lengths<60*f]
    
    ### calculate the weekday and only keep one weekday
    if( j%%7 ==0){ttttt=vd[7]}
    else(ttttt=vd[j%%7])
    
    if(wertime>480*f & ttttt==0 ){ #valid day = 480/60 = 8 hours
      l1=l1+sum(tt2)
      days=days+1
      if(j%%7==0){vd[7]=1}
      else{vd[j%%7]=1}
      
      ucfs=c(ucfs,ucf[j])
      # bb<- tor(tt1,tt2,6,tolerance*3,10*f*3)
      #bb<- tor(bb$values,bb$lengths,6,tolerance,10*f)
      #bb<- tor(bb$values,bb$lengths,6,tolerance/2,5*f)
      # bb<- tor(bb$values,bb$lengths,5,tolerance*3,10*f*3)
      # bb<- tor(bb$values,bb$lengths,5,tolerance,10*f)
      #bb<- tor(bb$values,bb$lengths,5,tolerance/2,5*f)

      bb<- tor_flex_constant(tt1,tt2,4,10*f*3,10*f*6,f)
      bb<- tor_flex_constant(bb$values,bb$lengths,4,10*f,10*f*3,f)
      bb<- tor_flex_constant(bb$values,bb$lengths,4,5*f,10*f,f)
      bb<- tor_flex_below(bb$values,bb$lengths,3,10*f*3,10*f*6,f)
      bb<- tor_flex_below(bb$values,bb$lengths,3,10*f,10*f*3,f)
      bb<- tor_flex_below(bb$values,bb$lengths,3,5*f,10*f,f)
      bb<- tor_flex_below(bb$values,bb$lengths,2,10*f*3,10*f*6,f)
      bb<- tor_flex_below(bb$values,bb$lengths,2,10*f,10*f*3,f)
      bb<- tor_flex_below(bb$values,bb$lengths,2,5*f,10*f,f)
      #all1=bb$lengths[bb$values==5]all1=bb$lengths[bb$values==5]
      barcode_calculation=bar_flex(bb$values,bb$lengths,f, bts)
      sub_length<- bb$lengths
      sub_barcode<- barcode_calculation
      long_barcoding=c(long_barcoding,sub_barcode)
      short_barcoding=shorting.barcode(short_barcoding,sub_barcode)
      long_barcoding_length=c(long_barcoding_length,sub_length)
      short_barcoding_length=shorting.barcode( short_barcoding_length,sub_length)
      l2<- l2+length(barcode_calculation)
    }
    if(length(long_barcoding)==0) {
      long_barcoding=0
      long_barcoding_length=0
    }
  }
  
  if (length(ucfs)>0) {
    row.names(short_barcoding)=paste(file_name,ucfs,sep="_")
    row.names( short_barcoding_length)=paste(file_name,ucfs,sep="_")
  }
  result<- list(days=days,l1=l1,l2=l2,vd=vd,long_barcoding=long_barcoding,short_barcoding=short_barcoding,long_barcoding_length=long_barcoding_length,short_barcoding_length=short_barcoding_length)
  return(result)
}
