nma_grade_prep <- function(nma_obj,nma_obj_imp,rob,high_rob){
  
  nma_tmp12 <- netsplit(nma_obj)
  nma_db <- nma_obj$data
  
  nma_tmp_g <- data.frame(nma_tmp12$random[,c(1,2,4,5)])
  colnames(nma_tmp_g)[2:4] <- paste(colnames(nma_tmp_g)[2:4], "nma", sep = "_")
  nma_tmp_d <- data.frame(nma_tmp12$direct.random[,c(1,2,4,5,12)])
  colnames(nma_tmp_d)[2:5] <- paste(colnames(nma_tmp_d)[2:5], "direct", sep = "_")
  colnames(nma_tmp_d)[5] <- "a1_inconsistency_I2"
  nma_tmp_d[,5] <- round(100*nma_tmp_d[,5],digits=1)
  nma_tmp_i <- data.frame(nma_tmp12$indirect.random[,c(1,2,4,5)])
  colnames(nma_tmp_i)[2:4] <- paste(colnames(nma_tmp_i)[2:4], "indirect", sep = "_")
  dif_tests <- data.frame("c1_proportion_direct"=nma_tmp12$prop.random,nma_tmp12$compare.random[,c(1,2,7)])
  colnames(dif_tests)[3] <- paste("c2",colnames(dif_tests)[3], "difference", sep = "_")
  colnames(dif_tests)[4] <- paste("c2",colnames(dif_tests)[4], "incoherence", sep = "_")
  nma_tmp_r <- left_join(nma_tmp_g,nma_tmp_d)
  nma_tmp_r <- left_join(nma_tmp_r,nma_tmp_i)
  nma_tmp_r <- left_join(nma_tmp_r,dif_tests)
  
  colnames(nma_db)[colnames(nma_db) == rob] <- "rob_v"
  
  comp_summary <- nma_db %>% group_by(Treatment1,Treatment2) %>% summarise(n_studies=n(),n_participants=sum(n1+n2),a1_n_studies_high_rob=sum(ifelse(rob_v==high_rob,1,0)))
  comp_summary1 <- comp_summary %>% mutate(comparison=paste0(Treatment1,":",Treatment2))
  comp_summary2 <- comp_summary %>% mutate(comparison=paste0(Treatment2,":",Treatment1))
  comp_summary <- rbind(comp_summary1,comp_summary2)
  comp_summary <- comp_summary[,3:6]
  comp_summary <- comp_summary %>% group_by(comparison) %>% summarise(n_studies=sum(n_studies),n_participants=sum(n_participants),a1_n_studies_high_rob=sum(a1_n_studies_high_rob))
  nma_tmp_r <- left_join(nma_tmp_r,comp_summary)
  
  a4 <- data.frame("comparison"=c(),"b1_first_order_loop"=c(),"b1_n_studies_first_order_loop"=c(),"b1_n_participants_first_order_loop"=c())
  
  for(i in 1:nrow(nma_tmp_r)){
    comp <- nma_tmp_r$comparison[i]
    t1 <- sub(":.*", "", comp)
    t2 <- sub(".*:", "", comp)
    
    a1 <- nma_db %>% filter(Treatment1==t1 | Treatment2==t1)
    a1$b1_first_order_loop <- paste0(a1$Treatment1,a1$Treatment2)
    a1$b1_first_order_loop <- str_replace(a1$b1_first_order_loop, t1, "")
    a1 <- a1 %>% group_by(b1_first_order_loop) %>% summarise(b1_n_studies_first_order_loop=n(),b1_n_participants_first_order_loop=sum(n1+n2))
    
    a2 <- nma_db %>% filter(Treatment1==t2 | Treatment2==t2)
    a2$b1_first_order_loop <- paste0(a2$Treatment1,a2$Treatment2)
    a2$b1_first_order_loop <- str_replace(a2$b1_first_order_loop, t2, "")
    a2 <- a2 %>% group_by(b1_first_order_loop) %>% summarise(b1_n_studies_first_order_loop=n(),b1_n_participants_first_order_loop=sum(n1+n2))
    
    a3 <- inner_join(a1,a2,by="b1_first_order_loop")
    a3 <- a3 %>% mutate(b1_n_studies_first_order_loop=b1_n_studies_first_order_loop.x+b1_n_studies_first_order_loop.y)
    a3 <- a3 %>% mutate(b1_n_participants_first_order_loop=b1_n_participants_first_order_loop.x+b1_n_participants_first_order_loop.y)
    a3 <- a3[order(-a3$b1_n_participants_first_order_loop), ]
    a3 <- a3 %>% select(b1_first_order_loop,b1_n_studies_first_order_loop,b1_n_participants_first_order_loop)
    a3 <- a3[1,]
    a3 <- data.frame("comparison"=comp,a3)
    
    a4 <- rbind(a4,a3)
    
  }
  
  nma_tmp_r <- left_join(nma_tmp_r,a4)
  
  
  aa4 <- data.frame("comparison"=c(),"b1_second_order_loop"=c(),"b1_n_studies_second_order_loop"=c(),"b1_n_participants_second_order_loop"=c())
  
  for(i in 1:nrow(nma_tmp_r)){
    comp <- nma_tmp_r$comparison[i]
    t1 <- sub(":.*", "", comp)
    t2 <- sub(".*:", "", comp)
    
    aa1 <- nma_db %>% filter(Treatment1==t1 | Treatment2==t1)
    aa1$b1_second_order_loop <- paste0(aa1$Treatment1,aa1$Treatment2)
    aa1$b1_second_order_loop <- str_replace(aa1$b1_second_order_loop, t1, "")
    aa1 <- aa1 %>% group_by(b1_second_order_loop) %>% summarise(b1_n_studies_second_order_loop=n(),b1_n_participants_second_order_loop=sum(n1+n2))
    
    aa2 <- nma_db %>% filter(Treatment1==t2 | Treatment2==t2)
    aa2$b1_second_order_loop <- paste0(aa2$Treatment1,aa2$Treatment2)
    aa2$b1_second_order_loop <- str_replace(aa2$b1_second_order_loop, t2, "")
    aa2 <- aa2 %>% group_by(b1_second_order_loop) %>% summarise(b1_n_studies_second_order_loop=n(),b1_n_participants_second_order_loop=sum(n1+n2))
    
    aa3 <- inner_join(aa1,aa2,by="b1_second_order_loop")
    aa3 <- aa3 %>% mutate(b1_n_studies_second_order_loop=b1_n_studies_second_order_loop.x+b1_n_studies_second_order_loop.y)
    aa3 <- aa3 %>% mutate(b1_n_participants_second_order_loop=b1_n_participants_second_order_loop.x+b1_n_participants_second_order_loop.y)
    aa3 <- aa3[order(-aa3$b1_n_participants_second_order_loop), ]
    aa3 <- aa3 %>% select(b1_second_order_loop,b1_n_studies_second_order_loop,b1_n_participants_second_order_loop)
    aa3 <- aa3[2,]
    aa3 <- data.frame("comparison"=comp,aa3)
    
    aa4 <- rbind(aa4,aa3)
    
  }
  
  nma_tmp_r <- left_join(nma_tmp_r,aa4)
  
    
    nma_tmpp12 <- netsplit(nma_obj_imp)
    
    nma_tmpp_g <- data.frame(nma_tmpp12$fixed[,c(1,2,4,5)])
    nma_tmpp_g$lower_cat <- ifelse(nma_tmpp_g[,3]< -0.8, -3, ifelse(nma_tmpp_g[,3]< -0.5, -2, ifelse(nma_tmpp_g[,3]< -0.2, -1, ifelse(nma_tmpp_g[,3]< 0.2,0, ifelse(nma_tmpp_g[,3]<0.5, 1, ifelse(nma_tmpp_g[,3]<0.8,2,3))))))
    nma_tmpp_g$upper_cat <- ifelse(nma_tmpp_g[,4]< -0.8, -3, ifelse(nma_tmpp_g[,4]< -0.5, -2, ifelse(nma_tmpp_g[,4]< -0.2, -1, ifelse(nma_tmpp_g[,4]< 0.2,0, ifelse(nma_tmpp_g[,4]<0.5, 1, ifelse(nma_tmpp_g[,4]<0.8,2,3))))))
    nma_tmpp_g$n_cat_FE <- (nma_tmpp_g$upper_cat - nma_tmpp_g$lower_cat)+1
    colnames(nma_tmpp_g)[2:7] <- paste("d",colnames(nma_tmpp_g)[2:7], "nma", sep = "_")
    nma_tmpp_d <- data.frame(nma_tmpp12$direct.fixed[,c(1,2,4,5)])
    nma_tmpp_d$lower_cat <- ifelse(nma_tmpp_d[,3]< -0.8, -3, ifelse(nma_tmpp_d[,3]< -0.5, -2, ifelse(nma_tmpp_d[,3]< -0.2, -1, ifelse(nma_tmpp_d[,3]< 0.2,0, ifelse(nma_tmpp_d[,3]<0.5, 1, ifelse(nma_tmpp_d[,3]<0.8,2,3))))))
    nma_tmpp_d$upper_cat <- ifelse(nma_tmpp_d[,4]< -0.8, -3, ifelse(nma_tmpp_d[,4]< -0.5, -2, ifelse(nma_tmpp_d[,4]< -0.2, -1, ifelse(nma_tmpp_d[,4]< 0.2,0, ifelse(nma_tmpp_d[,4]<0.5, 1, ifelse(nma_tmpp_d[,4]<0.8,2,3))))))
    nma_tmpp_d$n_cat_FE <- (nma_tmpp_d$upper_cat - nma_tmpp_d$lower_cat)+1
    colnames(nma_tmpp_d)[2:7] <- paste("a2",colnames(nma_tmpp_d)[2:7], "direct", sep = "_")
    nma_tmpp_i <- data.frame(nma_tmpp12$indirect.fixed[,c(1,2,4,5)])
    nma_tmpp_i$lower_cat <- ifelse(nma_tmpp_i[,3]< -0.8, -3, ifelse(nma_tmpp_i[,3]< -0.5, -2, ifelse(nma_tmpp_i[,3]< -0.2, -1, ifelse(nma_tmpp_i[,3]< 0.2,0, ifelse(nma_tmpp_i[,3]<0.5, 1, ifelse(nma_tmpp_i[,3]<0.8,2,3))))))
    nma_tmpp_i$upper_cat <- ifelse(nma_tmpp_i[,4]< -0.8, -3, ifelse(nma_tmpp_i[,4]< -0.5, -2, ifelse(nma_tmpp_i[,4]< -0.2, -1, ifelse(nma_tmpp_i[,4]< 0.2,0, ifelse(nma_tmpp_i[,4]<0.5, 1, ifelse(nma_tmpp_i[,4]<0.8,2,3))))))
    nma_tmpp_i$n_cat_FE <- (nma_tmpp_i$upper_cat - nma_tmpp_i$lower_cat)+1
    colnames(nma_tmpp_i)[2:7] <- paste("c3",colnames(nma_tmpp_i)[2:7], "indirect", sep = "_")
    nma_tmpp_r <- left_join(nma_tmpp_g,nma_tmpp_d)
    nma_tmpp_r <- left_join(nma_tmpp_r,nma_tmpp_i)
    nma_tmpp_r <- nma_tmpp_r[,c(1,7,13,19)]
    
    
    
    #nma_tmppp_g <- data.frame(nma_tmpp12$random[,c(1,2,4,5)])
    #nma_tmppp_g$lower_cat <- ifelse(nma_tmppp_g[,3]< -0.8, -3, ifelse(nma_tmppp_g[,3]< -0.5, -2, ifelse(nma_tmppp_g[,3]< -0.2, -1, ifelse(nma_tmppp_g[,3]< 0.2,0, ifelse(nma_tmppp_g[,3]<0.5, 1, ifelse(nma_tmppp_g[,3]<0.8,2,3))))))
    #nma_tmppp_g$upper_cat <- ifelse(nma_tmppp_g[,4]< -0.8, -3, ifelse(nma_tmppp_g[,4]< -0.5, -2, ifelse(nma_tmppp_g[,4]< -0.2, -1, ifelse(nma_tmppp_g[,4]< 0.2,0, ifelse(nma_tmppp_g[,4]<0.5, 1, ifelse(nma_tmppp_g[,4]<0.8,2,3))))))
    #nma_tmppp_g$n_cat_RE <- (nma_tmppp_g$upper_cat - nma_tmppp_g$lower_cat)+1
    #colnames(nma_tmppp_g)[2:7] <- paste(colnames(nma_tmppp_g)[2:7], "nma", sep = "_")
    nma_tmppp_d <- data.frame(nma_tmpp12$direct.random[,c(1,2,4,5)])
    nma_tmppp_d$lower_cat <- ifelse(nma_tmppp_d[,3]< -0.8, -3, ifelse(nma_tmppp_d[,3]< -0.5, -2, ifelse(nma_tmppp_d[,3]< -0.2, -1, ifelse(nma_tmppp_d[,3]< 0.2,0, ifelse(nma_tmppp_d[,3]<0.5, 1, ifelse(nma_tmppp_d[,3]<0.8,2,3))))))
    nma_tmppp_d$upper_cat <- ifelse(nma_tmppp_d[,4]< -0.8, -3, ifelse(nma_tmppp_d[,4]< -0.5, -2, ifelse(nma_tmppp_d[,4]< -0.2, -1, ifelse(nma_tmppp_d[,4]< 0.2,0, ifelse(nma_tmppp_d[,4]<0.5, 1, ifelse(nma_tmppp_d[,4]<0.8,2,3))))))
    nma_tmppp_d$n_cat_RE <- (nma_tmppp_d$upper_cat - nma_tmppp_d$lower_cat)+1
    colnames(nma_tmppp_d)[2:7] <- paste(colnames(nma_tmppp_d)[2:7], "direct", sep = "_")
    #nma_tmppp_i <- data.frame(nma_tmpp12$indirect.random[,c(1,2,4,5)])
    #nma_tmppp_i$lower_cat <- ifelse(nma_tmppp_i[,3]< -0.8, -3, ifelse(nma_tmppp_i[,3]< -0.5, -2, ifelse(nma_tmppp_i[,3]< -0.2, -1, ifelse(nma_tmppp_i[,3]< 0.2,0, ifelse(nma_tmppp_i[,3]<0.5, 1, ifelse(nma_tmppp_i[,3]<0.8,2,3))))))
    #nma_tmppp_i$upper_cat <- ifelse(nma_tmppp_i[,4]< -0.8, -3, ifelse(nma_tmppp_i[,4]< -0.5, -2, ifelse(nma_tmppp_i[,4]< -0.2, -1, ifelse(nma_tmppp_i[,4]< 0.2,0, ifelse(nma_tmppp_i[,4]<0.5, 1, ifelse(nma_tmppp_i[,4]<0.8,2,3))))))
    #nma_tmppp_i$n_cat_RE <- (nma_tmppp_i$upper_cat - nma_tmppp_i$lower_cat)+1
    #colnames(nma_tmppp_i)[2:7] <- paste(colnames(nma_tmppp_i)[2:7], "indirect", sep = "_")
    #nma_tmppp_r <- left_join(nma_tmppp_g,nma_tmppp_d)
    #nma_tmppp_r <- left_join(nma_tmppp_r,nma_tmppp_i)
    #nma_tmppp_r <- nma_tmppp_r[,c(1,7,13,19)]
    nma_tmppp_r <- nma_tmppp_d[,c(1,7)]
    
    nma_tmpp_r  <- left_join(nma_tmpp_r,nma_tmppp_r)
    nma_tmpp_r$a1_inconsistency_dif_n_cat <- nma_tmpp_r$n_cat_RE_direct - nma_tmpp_r$a2_n_cat_FE_direct
    nma_tmpp_r <- nma_tmpp_r[,c(1:4,6)]
    nma_tmpp_r
  
    nma_tmp_r <- left_join(nma_tmp_r,nma_tmpp_r)
  
  nma_tmp_r <- nma_tmp_r[, c("comparison","n_studies","n_participants","TE_direct","lower_direct","upper_direct","a1_n_studies_high_rob","a1_inconsistency_I2","a1_inconsistency_dif_n_cat","a2_n_cat_FE_direct","b1_first_order_loop","b1_n_studies_first_order_loop","b1_n_participants_first_order_loop","b1_second_order_loop","b1_n_studies_second_order_loop","b1_n_participants_second_order_loop","c1_proportion_direct","c2_TE_difference","c2_p_incoherence","TE_indirect","lower_indirect","upper_indirect","c3_n_cat_FE_indirect","TE_nma","lower_nma","upper_nma","d_n_cat_FE_nma")]
  
  #### Graph for Direct, Indirect and NMA estimates
  
  #graph_prepare <- nma_tmp_r[,c(1:7,9:11)]
  #graph_prepare <- graph_prepare %>% filter(!is.na(TE_direct) & !is.na(TE_indirect))
  #gp <- graph_prepare %>% pivot_longer(!comparison,names_to="type",values_to="value")
  #gp <- gp %>% separate(col="type",into=c("estimate","comparison_type"),sep = "_")
  #gp <- gp %>% pivot_wider(names_from=estimate,values_from = value)
  #gp <- gp %>% mutate(comparison_estimate=paste(comparison,comparison_type))
  
  #gp$comparison_estimate <- factor(gp$comparison_estimate,gp$comparison_estimate)
  #ggplot(gp, aes(y = comparison_estimate, x = TE))+  geom_point()+   geom_errorbar(aes(xmin = lower, xmax = upper,colour=comparison_type))+  geom_vline(xintercept = 0, linetype = "dashed")+scale_y_discrete(limits=rev)+ylab(NULL) + theme(legend.title = element_blank())
  
  ### Graph excluding studies with high RoB
  
  #graph_prepare <- d1 %>% filter(!is.na(TE_direct) & a1_n_studies_high_rob>0 & a1_n_studies_high_rob < n_studies)
  #graph_prepare <- graph_prepare[,c(1,4:6)]
  
  #graph_prepare1 <- update(nma_rqlq_sar,subset=rob!="H",data=nma_rqlq_sar$data)
  #nma_tmp12 <- netsplit(graph_prepare1)
  
  #nma_tmp_d <- data.frame(nma_tmp12$direct.random[,c(1,2,4,5)])
  #colnames(nma_tmp_d)[2:4] <- paste(colnames(nma_tmp_d)[2:4], "sens", sep = "_")
  #nma_tmp_d <- nma_tmp_d %>% filter(!is.na(TE_sens))
  
  #rob_graph <- left_join(graph_prepare,nma_tmp_d)
  
  #gp <- rob_graph %>% pivot_longer(!comparison,names_to="type",values_to="value")
  #gp <- gp %>% separate(col="type",into=c("estimate","comparison_type"),sep = "_")
  #gp <- gp %>% pivot_wider(names_from=estimate,values_from = value)
  #gp <- gp %>% mutate(comparison_estimate=paste(comparison,comparison_type))
  
  #gp$comparison_estimate <- factor(gp$comparison_estimate,gp$comparison_estimate)
  #ggplot(gp, aes(y = comparison_estimate, x = TE))+  geom_point()+   geom_errorbar(aes(xmin = lower, xmax = upper,colour=comparison_type))+  geom_vline(xintercept = 0, linetype = "dashed")+scale_y_discrete(limits=rev)+ylab(NULL) + theme(legend.title = element_blank())
  
  
  nma_tmp_r
}

#### Interpretation of the columns in the generated dataframe:
 ### TE_global, lower_global and upper_global: treatment effect, CI LB and CI UB for the global (direct+indirect) NMA estimates
 ### TE_direct, lower_direct and upper_direct: treatment effect, CI LB and CI UB for the direct NMA estimates
 ### TE_indirect, lower_indirect and upper_indirect: treatment effect, CI LB and CI UB for the indirect NMA estimates
 ### I2_direct: I2 for each direct comparison
 ### prop_direct: Proportion of direct evidence from all evidence
 ### TE_dif: Difference in treatment estimate between direct and indirect evidence
 ### p_dif: Inconsistency p-value for the difference in direct and indirect estimates
 ### k: N primary studies per direct comparison
 ### n_participants: N participants per direct comparison
 ### b1_first_order_loop, b1_n_studies_first_order_loop, b1_n_participants_first_order_loop: Loop with the largest number of participants for each comparison (comparator of the respective loop, n primary studies in that loop and n participants in that loop)