# Performances of HIEL on all projects from one repository
HIEL <- c(0.6173,0.6945,0.7273,0.5723,0.5364)
# Performances of other model on all projects from the same repository
# For the same repository, update the list 'OtherModel' with the performances of each utilised model
OtherModel <- c(0.6378,0.6391,0.6485,0.5702,0.6046)

# Ciff's Delta
CliffsDeltaTest = cliff.delta(HIEL, OtherModel, return.dm = TRUE)
print(CliffsDeltaTest)

# Wilcoxson paired samples test
WilcoxsonTest <- wilcox.test(HIEL, OtherModel)
print(WilcoxsonTest)
