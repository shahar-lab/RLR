#this code generates from a gui select list data_path and model_path for a working model

#load path string for the model you are working on
#note that in order no to create code duplicates you can
#change the model name in 'working model.txt'
mymodels_list=read.table('./modeling/working_model.rdata')



mymodel   =dlg_list(mymodels_list, multiple = TRUE)$res
data_path =paste0('./data/model_',mymodel)
model_path=paste0('./modeling/model_',mymodel,'/',mymodel,'_')
cat(paste0(mymodel,
           ' is the current working model',
           '\n',
           '\ndata  folder: ',data_path,
           '\nmodel folder: ',model_path))
