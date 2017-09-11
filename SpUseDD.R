DDBMI <- read.csv("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Data Files/OriginalDatafromDavid_CSV.csv", header=TRUE, sep=",")
SpUse <- colnames(DDBMI[7:length(DDBMI)])
SpUse <- gsub("\\.", " ", SpUse)
