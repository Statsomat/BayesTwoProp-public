# Generate dataset and file biontech.csv
s1 <- 8 # Covid for vaccinated 
n1 <- 18198 # vaccinated 
s2 <- 162 # Covid for non-vaccinated 
n2 <- 18325 # Non vaccinated 
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("Vaccine",n1), rep("Placebo",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/biontech.csv")



# Generate dataset and file myocarditis_vaccinated.csv
# https://www.nejm.org/doi/full/10.1056/NEJMoa2110475
s1 <- 21 # myocarditis vaccinated 
n1 <- 938812 # vaccinated
s2 <- 6 # myocarditis non-vaccinated 
n2 <- 938812 # non-vaccinated  
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("vaccinated ",n1), rep("non-vaccinated",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/myocarditis_vaccinated.csv")



# Generate dataset and file myocarditis_infected.csv
# https://www.nejm.org/doi/full/10.1056/NEJMoa2110475
s1 <- 19 # myocarditis infected 
n1 <- 183710 # infected
s2 <- 1 # myocarditis non-infected
n2 <- 183710 # non-infected  
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("infected",n1), rep("non-infected",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/myocarditis_infected.csv")


# Generate dataset and file AstraZeneca_SDSD.csv
s1 <- 27 # Covid for vaccinated 
n1 <- 4440 # vaccinated 
s2 <- 71 # Covid for non-vaccinated 
n2 <- 4455 # Non vaccinated 
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("Vaccine",n1), rep("Placebo",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/AstraZeneca_SDSD.csv")


# Generate dataset and file AstraZeneca_LDSD.csv
s1 <- 3 # Covid for vaccinated 
n1 <- 1367 # vaccinated 
s2 <- 30 # Covid for non-vaccinated 
n2 <- 1374 # Non vaccinated 
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("Vaccine",n1), rep("Placebo",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/AstraZeneca_LDSD.csv")


# Generate dataset and file Moderna-1.csv
s1 <- 5 # Covid for vaccinated 
n1 <- 14134 # vaccinated 
s2 <- 90 # Covid for non-vaccinated 
n2 <- 14073 # Non vaccinated 
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("Vaccine",n1), rep("Placebo",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/Moderna-1.csv")


# Generate dataset and file Moderna-2.csv
s1 <- 11 # Covid for vaccinated 
n1 <- 14134 # vaccinated 
s2 <- 185 # Covid for non-vaccinated 
n2 <- 14073 # Non vaccinated 
y <- c(rep(1,s1), rep(0,n1-s1), rep(1,s2), rep(0,n2-s2))
s <- c(rep("Vaccine",n1), rep("Placebo",n2))
dataset <- data.frame(y,s)
write.csv(dataset,"G:/REYAR/Statsomat/Development/Bayes/datasets/Moderna-2.csv")