
sellingPrice = 50#$/unit
salvage = 0
productionCost = 35#$/unit

co = productionCost - salvage
cu = sellingPrice - productionCost

demand <- c(3,4,5,6,7,8,9) # Wedding bouquets
probabilities <- c(0.05,0.12,0.2,0.24,0.17,0.14,0.08) # Wedding bouquets

results <- data.frame(demand,probabilities)


Q = 6
results$NumOver <- ifelse(Q - demand >= 0, (Q - demand)*results$probabilities, 0)
results$NumUnder <- ifelse(demand - Q >= 0, (demand - Q)*results$probabilities, 0)
results$CostOver <- co*results$NumOver
results$CostUnder <- cu*results$NumUnder
print(results)














#------------------------------
# Compute costs for Q values
#------------------------------
datalist = list()
index <- 1
for(Q in demand){
  
  results$NumOver <- ifelse(Q - demand >= 0, (Q - demand)*results$probabilities, 0)
  results$NumUnder <- ifelse(demand - Q >= 0, (demand - Q)*results$probabilities, 0)
  results$CostOver <- co*results$NumOver
  results$CostUnder <- cu*results$NumUnder
  
  dat <- data.frame( Q = Q, 
                     totalCostO = sum(results$CostOver), 
                     totalCostU = sum(results$CostUnder),
                     TotalCost = sum(results$CostOver) + sum(results$CostUnder))
  datalist[[index]] <- dat
  index <- index + 1
}
TotalCost = do.call(rbind, datalist)


#------------------------------
# Graph
#------------------------------
library(ggplot2)

p <- ggplot() + 
  geom_line(data = TotalCost, aes(x = Q, y = totalCostO), color = "blue") +
  geom_point(data = TotalCost, aes(x = Q, y = totalCostO), color = "blue") +
  geom_line(data = TotalCost, aes(x = Q, y = totalCostU), color = "red") +
  geom_point(data = TotalCost, aes(x = Q, y = totalCostU), color = "red") +
  geom_line(data = TotalCost, aes(x = Q, y = TotalCost), color = "black") +
  geom_point(data = TotalCost, aes(x = Q, y = TotalCost), color = "black") +
  scale_x_continuous(breaks=TotalCost$Q) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab('Q') +
  ylab('Cost ($)')

print(p)
