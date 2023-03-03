##

library(ggplot2)
library(vioplot)
library(ggforce)


##
# cell type: CD4_Naive
# cytokine_condition: Resting, Th0
# stimulation_time : 16h

selected_cell_type = "CD4_Naive"
selected_stimulation_time = "16h"
selected_cytokine_conditions = c("Resting","Th0","Th1","Th2","Th17","iTreg","IFNB")

#naming
genename = "CD74"
genecode_CD74 = "ENSG00000019582"

#import data
df <- NCOMMS.19.7936188_bulk_RNAseq_metadata

# filter expression matrix genecode_CD74
count_mt <- NCOMMS.19.7936188_bulk_RNAseq_raw_counts
count_mt_f <- count_mt[which(rownames(count_mt) == genecode_CD74),]

  # select data based on conditions
  df_f <- df[which(df$cell_type %in% c(selected_cell_type)
                   & df$cytokine_condition %in% selected_cytokine_conditions
                   & df$stimulation_time %in% c(selected_stimulation_time)),]
  sample_ids <-  df_f$sample_id
  
  # select sample id from result above
  count_cd74_condition <- count_mt_f[which(colnames(count_mt_f) %in% sample_ids)]
  
  # transport
  count_cd74_condition_t<- as.data.frame(t(count_cd74_condition))
  
  # add column with sample id
  count_cd74_condition_t$sample_id<-rownames(count_cd74_condition_t)
  
  #rename the column name
  colnames(count_cd74_condition_t)[1] <- genename
  
  #sort by sample_id
  count_cd74_selected <- count_cd74_condition_t[order(count_cd74_condition_t$sample_id),]
  
  #add column cytokine_condition
  count_cd74_selected$cytokine_condition  <- c(df_f[order(df_f$sample_id),"cytokine_condition"])
  
  #fix the order
  count_cd74_selected$cytokine_condition <- factor(count_cd74_selected$cytokine_condition, levels = selected_cytokine_conditions)
 
   #plot 
  p <- ggplot(count_cd74_selected,aes(x=cytokine_condition,y=CD74)) +
    geom_violin(trim=FALSE, fill='lightblue', color="palevioletred") +
    geom_boxplot(width=0.1) +   geom_sina() +
    labs(title="Violin CD74 of Resting & Th0") + theme_minimal()
  print(p)
