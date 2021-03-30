source('preprocessing.R', chdir = TRUE)
source('harmony.R', chdir = TRUE)
source('plot.R', chdir = TRUE)

# 1. Preprocessing
# Readinfiles: expression data (.fcs), cell type data (.csv)
# Read marker names and remove empty channels
# Remove outliers 
# Scaling

# 2. Batch correction
# Bind the expression data of all batches into a single matrix
# Bind the cell type labels of all batches into a single dataframe
# Input the batch number
# MetaData: a dataframe with variables to integrate including batch numbers and cell type labels
# Harmony batch effect correction
# Save the batch-corrected files separately

library(readr)
dataDir = "/home/rstudio/host/cytof_data_raw"
fileInfo = read_csv(file.path('/home/rstudio/host/', 'sampleInfo_PID_cut.csv'))
fileNames = file.path(dataDir,fileInfo$`Expt ID`, 'renamed', 
                      paste0(fileInfo$`Barcoded Batch`, '_01_0_cells'), 
                      paste0(fileInfo$`Sample ID`, '_d_', fileInfo$`Barcode key`, '.fcs'))
labelNames = file.path(dataDir,fileInfo$`Expt ID`, 'classified', 
                      paste0(fileInfo$`Barcoded Batch`, '_01_0_cells'), 
                      paste0(fileInfo$`Sample ID`, '_d_', fileInfo$`Barcode key`, '.csv'))

files = readInfiles(fileNames, "fcs")
labels = readInfiles(labelNames, "csv")
dat = lapply(files, channelFilter)
dat = lapply(dat, function(x){apply(x, 2, removeOutliers)})


dat_trans = lapply(dat, function(x){asinh(x/5)})
dat = lapply(dat, scale)
dat_trans = lapply(dat_trans, scale)

# subsampling
selectid = lapply(dat_trans, function(x){sample(1:dim(x)[1], 40000)})
dat_trans_sub = lapply(c(1:length(labels)), function(x){dat_trans[[x]][selectid[[x]], ]})
label_sub = lapply(c(1:length(labels)), function(x){labels[[x]][selectid[[x]], ]})

for (i in 1:length(dat_trans_sub)){
  filepath_dat = paste0('/home/rstudio/host/preprocessed/', 
                    gsub("fcs", "csv", names(dat_trans[i])))
  write.csv(dat_trans_sub[[i]], filepath_dat, row.names = FALSE) 
  filepath_label = paste0('/home/rstudio/host/labels/', 
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
