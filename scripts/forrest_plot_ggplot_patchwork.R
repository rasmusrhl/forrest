library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(patchwork)

            
# commit 1

#Generate some random data
data <- data.frame(
  type = rep(c("type1", "type2", "type3", "type4"), 4),
  sev = rep(c("sev1", "sev2", "sev3", "sev4"), each=4),
  HR = round(runif(16, min=0.8, max=1.2),2),
  lower = round(runif(16, min=0.2, max=0.69),2),
  upper = round(runif(16, min=1.31, max=2),2)
) %>% 
  mutate(label = paste(HR, " [", lower, "-", upper, "]", sep=""),
         sev = factor(sev, levels=c("sev4", "sev3", "sev2", "sev1"))) #just to order them around


data <-
data |> tibble()

# Overall sevs column
tab0 <- data %>% 
  filter(type=="type1") %>% #this selection here doesn't really matter
  ggplot()+
  geom_text(aes(y = sev, x = 1.2, label=sev, hjust=0), size = 4) +
  ylab(NULL) + xlab(NULL) +
  theme_void()+
  xlim(1,1.5)





#define function for graphical parts of the whole product
grapher <- function(typestr, #tells how to subset the data
                    bottom=F, #for the x-axis title
                    bound_lower=.5, #these bounds are for the arrowheads (you can also change them when calling the function out in the bottom part of the code)
                    bound_upper=1.9){
  data <- data %>% 
    filter(type==typestr) #filter out the wished subset
  res <- data %>% 
    ggplot(aes(y = sev, x = HR)) +
    geom_vline(xintercept = 1, color = "#11c232", linetype = "dashed", cex = .7, alpha = 1) +
    geom_point(shape = 18, size = 3) +  
    scale_x_continuous(name = "HR [95% CI]", limits = c(bound_lower, bound_upper))+
    ylab(" ") + 
    theme_void()
  #x-axis title only for graphs in the bottom part of the whole combined graph
  if(bottom==F){
    res <- res +
      theme(axis.title.x = element_blank(),
            axis.text.x.bottom = element_blank(),
            axis.ticks.x = element_blank())
  }
  #arrows for those CI-s which exceed limits
  #the loop basically checks for each datapoint, if its bounds exceed the limits given (bound_lower and bound_upper)
  lower <- data$lower
  upper <- data$upper
  sev <- data$sev
  for(i in 1:nrow(data)){
    if(is.na(lower[i])) { #do we have info on the CI (will leave empty line in the graph if TRUE)?
      #do nothing
    } else{
      if(lower[i] > bound_lower & upper[i] < bound_upper){ #are all bounds between boundaries?
        res <- res +
          geom_errorbarh(aes_(xmin = lower[i], xmax = upper[i],y = sev[i]), height = 0.25)
      } else if(lower[i] <= bound_lower & upper[i] >= bound_upper){ #are both bounds out of boundaries?
        res <- res +
          geom_segment(aes_(x = bound_lower, xend = bound_upper,y = sev[i], yend = sev[i]),
                       arrow = arrow(length=unit(0.20,"cm"), ends="both", type = "open"))
      } else if(lower[i] > bound_lower & upper[i] >= bound_upper){ #is upper bound out of scope?
        res <- res+
          geom_segment(aes_(x = lower[i], xend = bound_upper, y = sev[i], yend = sev[i]),
                       arrow = arrow(length=unit(0.20,"cm"), ends = "last", type = "open")) +
          geom_errorbarh(aes_(xmin = lower[i], xmax = lower[i], y = sev[i]), height = 0.25)
      } else{ #is lower bound out of scope?
        res <- res+
          geom_segment(aes_(x = bound_lower, xend = upper[i], y = sev[i], yend = sev[i]),
                       arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
          geom_errorbarh(aes_(xmin = upper[i], xmax = upper[i], y = sev[i]), height = 0.25)
      }
    }
  }
  res
}



#function for the textual parts of the whole graph
tabber <- function(typestr, title){
  data %>% 
    filter(type==typestr) %>% 
    ggplot()+
    geom_text(aes(y = sev, x = 1.05, label=label, hjust=0), size = 4) +
    ylab(NULL) + xlab(NULL) +
    theme_void()+
    xlim(1, 1.5)+
    ggtitle(title)
}


#test it out
tab0 + grapher("type1") + tabber("type1", "type1") +
  grapher("type2") + tabber("type2", "type2") +
  tab0 +
  grapher("type3", bottom=T) + tabber("type3", "type3") +
  grapher("type4", bottom=T) + tabber("type4", "type4") +
  plot_layout(ncol=5,
              widths = c(.15, rep(.3, 4))) #plot_layout adjusts the characteristics of the whole combined graph



