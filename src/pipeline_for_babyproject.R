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
fileInfo = read_csv(file.path('/home/rstudio/host/', 'sample_info_cut.csv'))
fileNames = file.path(dataDir,fileInfo$`Expt ID`, 'renamed', 
                      paste0(fileInfo$`Barcoded Batch #`, '_con_0_cells'), 
                      paste0(fileInfo$`Sample ID/Barcode`, '_d_', fileInfo$`Barcode key`, '.fcs'))
labelNames = file.path(dataDir,fileInfo$`Expt ID`, 'classified', 
                      paste0(fileInfo$`Barcoded Batch #`, '_con_0_cells'), 
                      paste0(fileInfo$`Sample ID/Barcode`, '_d_', fileInfo$`Barcode key`, '.csv'))

file.copy(labelNames, file.path('/home/rstudio/host/', "labels"))

files = readInfiles(fileNames, "fcs")
labels = readInfiles(labelNames, "csv")
dat = lapply(files, channelFilter)
dat = lapply(dat, function(x){apply(x, 2, removeOutliers)})


dat_trans = lapply(dat, function(x){asinh(x/5)})
dat = lapply(dat, scale)
dat_trans = lapply(dat_trans, scale)

Dat = bindlist(dat_trans) # choose whether to do asinh transfer here
Label = bindlist(labels)
Label$batch = unlist(lapply(c(1:nrow(fileInfo)), 
                            function(x){rep(fileInfo$`Barcoded Batch #`[x], 
                                            nrow(dat[[x]]))}
                            ))
MetaData = data.frame(batch = Label$batch, level1 = Label$level1, level2 = Label$level2)
Dat.harmony = HarmonyMatrix(Dat, MetaData, "batch", do_pca = FALSE)
dat.harmony = saveCorrectedFiles(Dat.harmony, file.path('/home/rstudio/host/cytof_data_corrected',
                                                        "output", "corrected_trans"))

# umap.harmony = umap(Dat.harmony)
# umap.dat = umap(Dat)
# 
# gp1 = umapPlot(umap.dat, "Raw", "batch")
# gp2 = umapPlot(umap.dat, "Raw", "level1")
# gp3 = umapPlot(umap.harmony, "Harmony", "batch")
# gp4 = umapPlot(umap.harmony, "Harmony", "level1")
