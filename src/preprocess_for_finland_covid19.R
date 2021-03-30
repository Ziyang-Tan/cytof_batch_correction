source('preprocessing.R', chdir = TRUE)
source('harmony.R', chdir = TRUE)
source('plot.R', chdir = TRUE)

library(readr)
dataDir = "/home/rstudio/host/finland_covid19_proj/EXP-20-DG3609"
fileInfo = read_csv(file.path('/home/rstudio/host/finland_covid19_proj',
                              'Combined sample list_Covid pjct_Kanth_Santtu.csv'))
# filtering
fileInfo = fileInfo[fileInfo$`Sample ID for CyTOF` != 'No sample',]
#fileInfo = fileInfo[fileInfo$`Date of ICU admission` != 'Non-ICU patient',]
#fileInfo = fileInfo[fileInfo$`Subject ID` %in% c('COV-34', 'COV-40', 'COV-32', 'COV-36'),]
fileInfo = fileInfo[fileInfo$`Subject ID` %in% c('COV-40'),]
#fileInfo = fileInfo[!(fileInfo$`Subject ID` %in% c('COV-34', 'COV-40', 'COV-32', 'COV-36')),]
#fileInfo = fileInfo[29:56,]

fileNames = file.path(dataDir,'renamed', paste0(fileInfo$`Sample ID for CyTOF`, '.fcs'))
labelNames = file.path(dataDir,'classified', paste0(fileInfo$`Sample ID for CyTOF`, '.csv'))

files = readInfiles(fileNames, "fcs")
labels = readInfiles(labelNames, "csv")
dat = lapply(files, channelFilter)
dat = lapply(dat, function(x){apply(x, 2, removeOutliers)})


dat_trans = lapply(dat, function(x){asinh(x/5)})
#dat = lapply(dat, scale)
dat_trans = lapply(dat_trans, scale)

# subsampling
selectid = lapply(dat_trans, function(x){sample(1:dim(x)[1], min(40000, dim(x)[1]))})
dat_trans_sub = lapply(c(1:length(labels)), function(x){dat_trans[[x]][selectid[[x]], ]})
label_sub = lapply(c(1:length(labels)), function(x){labels[[x]][selectid[[x]], ]})

for (i in 1:length(dat_trans_sub)){
  filepath_dat = paste0('/home/rstudio/host/finland_covid19_proj/preprocessed_data/', 
                    gsub("fcs", "csv", names(dat_trans[i])))
  write.csv(dat_trans_sub[[i]], filepath_dat, row.names = FALSE) 
  filepath_label = paste0('/home/rstudio/host/finland_covid19_proj/preprocessed_data/labels/', 
                        gsub("fcs", "csv", names(labels[i])))
  write.csv(label_sub[[i]], filepath_label, row.names = FALSE) 
}
#Dat = bindlist(dat_trans_sub) # choose whether to do asinh transfer here
#datlist = saveCorrectedFiles(Dat, '/home/rstudio/host/preprocessed')

# umap.harmony = umap(Dat.harmony)
# umap.dat = umap(Dat)
# 
# gp1 = umapPlot(umap.dat, "Raw", "batch")
# gp2 = umapPlot(umap.dat, "Raw", "level1")
# gp3 = umapPlot(umap.harmony, "Harmony", "batch")
# gp4 = umapPlot(umap.harmony, "Harmony", "level1")
