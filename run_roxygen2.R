roxygen2::roxygenise()
locationRcode = "/home/vincent/projects/movement-sequence-mapping/R" 
ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
for (i in 1:length(ffnames)) {
  source(paste(locationRcode,"/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
}