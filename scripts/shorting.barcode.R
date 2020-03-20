# get the new sequciencing of every day
shorting.barcode<-function(short_barcoding,sub_barcode){
  d=1440
  # the trick.we assume the new sequencing is less than 1440. Actually, it shoud be as 1440*f in worst case. However, 1440 is far far enough
  tt=rep(0,1440)
  if (length(sub_barcode)>1440)
  {
    tt=sub_barcode[1:1440]
  }
  else{
    tt[1:length(sub_barcode)]=sub_barcode
  }
  short_barcoding=rbind(short_barcoding,tt)  
  return(short_barcoding)
}
# shorting.barcode<-function(short_barcoding,sub_barcode){
#   d=1000
#   tt=rep(0,1000)
#   if (length(sub_barcode)>1000)
#   {
#     tt=sub_barcode[1:1000]
#   }
#   else{
#     tt[1:length(sub_barcode)]=sub_barcode
#   }
#   short_barcoding=rbind(short_barcoding,tt)  
#   return(short_barcoding)
# }
