
# Empty environment.
rm(list=ls())

# Load libraries.
library(dplyr)
library(readr)
library(stringr)
library(survey)
library(writexl)



# Model1 and Model3 estimations.----
for (mm in c("M1","M3")) {
  for (dd in c("Q1","Q4")) {
    #load data.
    df <- read_csv(file = paste0("Data\\prepped\\Data_",dd,"_v2.csv"))
    cnt.list <- unique(as.character(df$CNTRYID))
    
    #select model and outcome.
    if(mm == "M1"){
      if(dd == "Q1"){
        mod <- as.formula("LowIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2")
      }else{
        mod <- as.formula("HighIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2")
      }
    }else{
      if(dd=="Q1"){
        mod <- as.formula("LowIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2 + ZPVLN1 + EDCAT41 + EDCAT42 + EDCAT43")
      }else{
        mod <- as.formula("HighIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2 + ZPVLN1 + EDCAT41 + EDCAT42 + EDCAT43")
      }
    }
    
    M_out <- data.frame()
    # Country based records.
    for (cnt in cnt.list) {
      # Filter data by CNTRYID.
      data <- df %>% filter(CNTRYID %in% c(cnt))
      
      # Jackknife.
      options(scipen=999)
      M_JKdata <- data.frame()
      for (n in paste0("SPFWT",c(1:80,0))) {
        #--Model run.
        dsgn <- svydesign(ids = ~1, strata = NULL, weights = data[,which(colnames(data)==n)],
                          data = data)
        qm.t <- svyglm(mod, dsgn, family = binomial("logit"))
        M_JKdata.t <- qm.t$coefficients
        M_JKdata <- rbind(M_JKdata, M_JKdata.t)
        names(M_JKdata) <- names(M_JKdata.t)
      }
      
      M <- M_JKdata
      model <- data.frame(c(1))
      names(model) <- "CNTRYID"
      model$CNTRYID <- data$CNTRYID[1]
      model <- cbind(model, JKtype=c(data$VEMETHOD[1]))
      model <- cbind(model, coef=c(names(M)[1:length(M)]))
      model <- cbind(model, M=c(as.vector(t(M[81,1:length(M)]))))
      
      
      #--SE calculations.
      c <- ifelse(data$VEMETHOD[1]=="JK2", 1, 
                  ifelse(data$VEMETHOD[1]=="JK1", ((data$VENREPS[1] - 1)/data$VENREPS[1]), NA))
      SE <- c()
      for (i in 1:length(M_JKdata)) {
        TrT0sqrd <- (M_JKdata[,i] - M_JKdata[81,i])^2
        SE.i <- sqrt(c*sum(TrT0sqrd[1:80]))
        SE <- c(SE, SE.i)
      }
      #append SE and calculate z and p values.
      model <- cbind(model, Std.Error=c(SE))
      model$z.value <- model$M/model$Std.Error
      model$p.value <- 2*pnorm(abs(model$z.value), lower.tail = F)
      M_out <- rbind(M_out, model)
      
    }
    #order by country.
    M_out <- M_out[order(M_out$CNTRYID),]
    names(M_out)[names(M_out) == "M"] <- mm
    
    #Export.
    write_xlsx(M_out, path = paste0("Output\\",mm,"_",dd,"_out.xlsx"))
    
  }
}


#
# KHB analysis.----
for (dd in c("Q1","Q4")) {
  #load data.
  df <- read_csv(file = paste0("Data\\prepped\\Data_",dd,"_v2.csv"))
  cnt.list <- unique(as.character(df$CNTRYID))
  
  #select model and outcome.
  if(dd == "Q1"){
    mod3 <- as.formula("LowIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2 + ZPVLN1 + EDCAT41 + EDCAT42 + EDCAT43")
    modred <- as.formula("LowIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2 + ZPVLN1.t.M1M3 + 
                                    EDCAT41.t.M1M3 + EDCAT42.t.M1M3 + EDCAT43.t.M1M3")
  }else{
    mod3 <- as.formula("HighIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2 + ZPVLN1 + EDCAT41 + EDCAT42 + EDCAT43")
    modred <- as.formula("HighIncome ~ FEMALE + AGEG10LFS2 + AGEG10LFS3 + PARED1 + PARED2 + BOOKS1 + BOOKS2 + ZPVLN1.t.M1M3 + 
                                    EDCAT41.t.M1M3 + EDCAT42.t.M1M3 + EDCAT43.t.M1M3")
  }
  
  # Country based records.
  khb.results <- as.data.frame(c(1:7))
  names(khb.results) <- "CNTRYID"
  KHB.output <- data.frame()
  
  for (cnt in cnt.list) {
    # Filter data by CNTRYID.
    data <- df %>% filter(CNTRYID %in% c(cnt))
    
    # KHB.
    options(scipen=999)
    M3_JKdata <- data.frame()
    redM3_JKdata <- data.frame()
    for (n in paste0("SPFWT",c(1:80,0))) {
      #--M3.
      dsgn <- svydesign(ids = ~1, strata = NULL, weights = data[,which(colnames(data)==n)],
                        data = data)
      M3.t <- svyglm(mod3, dsgn, family = binomial("logit"))
      M3_JKdata.t <- M3.t$coefficients
      M3_JKdata <- rbind(M3_JKdata, M3_JKdata.t)
      names(M3_JKdata) <- names(M3_JKdata.t)
      #--reduced M3.
      ##WEIGHTED Residuals for ZPVLN1 + EDCAT4 after predicted by the rest of the variables in M3.
      ZPVLN1.t.M1M3 <- svyglm(ZPVLN1 ~ FEMALE + AGEG10LFS + PARED + BOOKS, 
                              dsgn)
      EDCAT41.t.M1M3 <- svyglm(EDCAT41 ~ FEMALE + AGEG10LFS + PARED + BOOKS, 
                               dsgn, family = binomial("logit"))
      EDCAT42.t.M1M3 <- svyglm(EDCAT42 ~ FEMALE + AGEG10LFS + PARED + BOOKS, 
                               dsgn, family = binomial("logit"))
      EDCAT43.t.M1M3 <- svyglm(EDCAT43 ~ FEMALE + AGEG10LFS + PARED + BOOKS, 
                               dsgn, family = binomial("logit"))
      data$ZPVLN1.t.M1M3 <- ZPVLN1.t.M1M3$residuals
      data$EDCAT41.t.M1M3 <- EDCAT41.t.M1M3$residuals
      data$EDCAT42.t.M1M3 <- EDCAT42.t.M1M3$residuals
      data$EDCAT43.t.M1M3 <- EDCAT43.t.M1M3$residuals
      ## reduced_M1vsM3 after WEIGHTING: FEMALE + AGEG10LFS + PARED + BOOKS + ZPVLN1.t.M1M3 + EDCAT4[1-3].t.M1M3
      dsgn.extended <- svydesign(ids = ~1, strata = NULL, weights = data[,which(colnames(data)==n)],
                                 data = data)
      M1M3.t.reduced <- svyglm(modred, dsgn.extended, family = binomial("logit"))
      redM3_JKdata.t <- M1M3.t.reduced$coefficients
      redM3_JKdata <- rbind(redM3_JKdata, redM3_JKdata.t)
      names(redM3_JKdata) <- names(redM3_JKdata.t)
    }
    
    #--Difference = ReducedM3 - M3.
    Diff_JKdata <- as.data.frame(paste0("SPFWT",c(1:80,0)))
    names(Diff_JKdata) <- "weight"
    for (p in 2:8) {
      Diff_JKdata.p <- redM3_JKdata[p] - M3_JKdata[p]
      Diff_JKdata <- cbind(Diff_JKdata, Diff_JKdata.p)
    }
    
    M3 <- M3_JKdata
    Reduced.M3 <- redM3_JKdata
    Diff.data <- Diff_JKdata
    khb.results$CNTRYID <- data$CNTRYID[1]
    khb.results$JKtype <- data$VEMETHOD[1]
    khb.results$coeff <- names(Reduced.M3)[2:8]
    khb.results$Reduced.M3 <- as.vector(t(Reduced.M3[81,2:8]))
    khb.results$M3 <- as.vector(t(M3[81,2:8]))
    khb.results$Difference <- as.vector(t(Diff_JKdata[81,2:8]))
    
    #--SE calculations.
    c <- ifelse(data$VEMETHOD[1]=="JK2", 1, 
                ifelse(data$VEMETHOD[1]=="JK1", ((data$VENREPS[1] - 1)/data$VENREPS[1]), NA))
    
    Diff_JKdata$TrT0sqrd_FEMALE <- (Diff_JKdata$FEMALE - Diff_JKdata$FEMALE[81])^2
    SE_FEMALE <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_FEMALE[1:80]))
    
    Diff_JKdata$TrT0sqrd_AGEG10LFS2 <- (Diff_JKdata$AGEG10LFS2 - Diff_JKdata$AGEG10LFS2[81])^2
    SE_AGEG10LFS2 <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_AGEG10LFS2[1:80]))
    
    Diff_JKdata$TrT0sqrd_AGEG10LFS3 <- (Diff_JKdata$AGEG10LFS3 - Diff_JKdata$AGEG10LFS3[81])^2
    SE_AGEG10LFS3 <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_AGEG10LFS3[1:80]))
    
    Diff_JKdata$TrT0sqrd_PARED1 <- (Diff_JKdata$PARED1 - Diff_JKdata$PARED1[81])^2
    SE_PARED1 <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_PARED1[1:80]))
    
    Diff_JKdata$TrT0sqrd_PARED2 <- (Diff_JKdata$PARED2 - Diff_JKdata$PARED2[81])^2
    SE_PARED2 <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_PARED2[1:80]))
    
    Diff_JKdata$TrT0sqrd_BOOKS1 <- (Diff_JKdata$BOOKS1 - Diff_JKdata$BOOKS1[81])^2
    SE_BOOKS1 <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_BOOKS1[1:80]))
    
    Diff_JKdata$TrT0sqrd_BOOKS2 <- (Diff_JKdata$BOOKS2 - Diff_JKdata$BOOKS2[81])^2
    SE_BOOKS2 <- sqrt(c*sum(Diff_JKdata$TrT0sqrd_BOOKS2[1:80]))
    
    Std.Error <- c(SE_FEMALE, SE_AGEG10LFS2, SE_AGEG10LFS3, SE_PARED1, SE_PARED2, SE_BOOKS1, SE_BOOKS2)
    khb.results$Std.Error <- Std.Error
    khb.results$z.value <- khb.results$Difference/khb.results$Std.Error
    khb.results$p.value <- 2*pnorm(abs(khb.results$z.value), lower.tail = F)
    
    KHB.output <- rbind(KHB.output, khb.results)
  }
  #order by country.
  KHB.output <- KHB.output[order(KHB.output$CNTRYID),]
  
  #Export.
  write_xlsx(KHB.output, path = paste0("Output\\KHB_",dd,"_out.xlsx"))
  
}


#