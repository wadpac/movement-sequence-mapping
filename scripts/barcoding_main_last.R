barcoding_main_last<- function(file_list, path_input, fileid = "test", 
                               epochsize = 15,  which="y", rescale.epoch = 15,
                               cutpoints=c(0,100,2296,4012),
                               bts = c(0, 5, 10, 30),
                               collapse.by = "%Y-%m-%d",
                               keep.error = FALSE){
  #lists<- filelist_generation()
  #file_list<- lists$file_list
  # meta_data<- lists$meta_data
  
  n=length(file_list)
  barcodes=NULL
  all_length=NULL
  file_names=NULL
  barcodes_length<- NULL
  
  barcodes_all<-NULL
  days=NULL
  short_barcoding=NULL
  long_barcoding=NULL
  short_barcoding_length=NULL
  long_barcoding_length=NULL
  vd=NULL

  tz = "Europe/London"
  for(i in 1:n){ # loop over files
    cat(paste0("\n",i,": "))
    
    file_name=gsub(".csv", "", file_list[i], ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
    file_name=gsub("CSV", "", file_name, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, use= FALSE)
    cat("read data...")
    # Why is timezone hardcoded to Europe/London? The data used was from the Denmark?
    temp<-  readdata(file_list[i], path_input, fileid, tz = tz,sparse = FALSE, fault = 32767)
    #HIER KAN EEN IF STATEMENT: if rescale.epoch %% epoch == 0 then aggAccFile, else aggAccFile_self?
    #test<- aggAccFile_self(temp, by =epoch, keep.error = FALSE,which="y")
    test<- aggAccFile(temp, by =epochsize, keep.error = keep.error,which="y")
    # test is now a list with two vectors:
    # - outcome: numeric count values from the data file
    # - ts_agg: POSIXct timestamps

    cat("process data...")
    calculation<-bouts_length_filter(counts = test$outcome, timeline = test$ts_agg,
                                     file_list[i], epochsize, validdays,
                                     mimwear, cutpoints, bts, tz=tz)
    

    short_barcoding=rbind(  short_barcoding,  calculation$short_barcoding)
    long_barcoding= barcodeMapping::long.barcode(long_barcoding,    calculation$long_barcoding)
    short_barcoding_length=rbind(  short_barcoding_length,    calculation$short_barcoding_length)
    long_barcoding_length=barcodeMapping::long.barcode(long_barcoding_length,    calculation$long_barcoding_length)
    days=c(days,calculation$days)
    cat("done")
  }
  results=list(short_sequence=short_barcoding,long_sequence=long_barcoding)
  #save.image("below11052017_15_teatske.Rdata")
  #save.image("barcoding_2101.Rdata")
  return(results)
}
