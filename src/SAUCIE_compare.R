source('preprocessing.R', chdir = TRUE)
source('harmony.R', chdir = TRUE)
source('plot.R', chdir = TRUE)
library(umap)
library(ggpubr)

library(readr)
dataDir = "/home/rstudio/host/cytof_data_raw"
fileInfo = read_csv(file.path('/home/rstudio/host/', 'sample_info_cut.csv'))
fileNames = file.path(dataDir,fileInfo$`Expt ID`, 'renamed', 
                      paste0(fileInfo$`Barcoded Batch #`, '_con_0_cells'), 
                      paste0(fileInfo$`Sample ID/Barcode`, '_d_', fileInfo$`Barcode key`, '.fcs'))
fileNames.saucie = file.path('/home/rstudio/host/SAUCIE_corrected/batch_corrected', 
                      paste0(fileInfo$`Sample ID/Barcode`, '_d_', fileInfo$`Barcode key`, '.csv'))
labelNames = file.path(dataDir,fileInfo$`Expt ID`, 'classified', 
                       paste0(fileInfo$`Barcoded Batch #`, '_con_0_cells'), 
                       paste0(fileInfo$`Sample ID/Barcode`, '_d_', fileInfo$`Barcode key`, '.csv'))

files = readInfiles(fileNames, "fcs")
files.saucie = readInfiles(fileNames.saucie, 'csv')
labels = readInfiles(labelNames, "csv")
dat = lapply(files, channelFilter)
dat = lapply(dat, function(x){apply(x, 2, removeOutliers)})


# dat_trans = lapply(dat, function(x){asinh(x/5)})
# dat = lapply(dat, scale)
# dat_trans = lapply(dat_trans, scale)

Dat = bindlist(dat) # choose whether to do asinh transfer here
Dat.saucie = bindlist(files.saucie)
Label = bindlist(labels)
Label$batch = factor(unlist(lapply(c(1:nrow(fileInfo)), 
                            function(x){rep(fileInfo$`Barcoded Batch #`[x], 
                                            nrow(dat[[x]]))}
)))

# subsampling
sid = sample.int(dim(Dat)[1], 20000)
Dat.saucie.s = Dat.saucie[sid,]
Dat.s = Dat[sid,]
Label.s = Label[sid,]

# Dat.saucie.s = asinh(Dat.saucie.s/5)
Dat.saucie.s = scale(Dat.saucie.s)
# Dat.s = asinh(Dat.s/5)
Dat.s = scale(Dat.s)

umap.saucie.s = umap(Dat.saucie.s)
umap.dat.s = umap(Dat.s)

gp1 = layoutPlot(umap.saucie.s$layout, Label.s, 'SAUCIE_subsample', 'level1')
gp2 = layoutPlot(umap.saucie.s$layout, Label.s, 'SAUCIE_subsample', 'batch')
gp3 = layoutPlot(umap.dat.s$layout, Label.s, 'Raw_subsample', 'level1')
gp4 = layoutPlot(umap.dat.s$layout, Label.s, 'Raw_subsample', 'batch')
gplist = list(gp1, gp2, gp3, gp4)

ggarrange(plotlist = gplist, ncol = 2, nrow = 2)

