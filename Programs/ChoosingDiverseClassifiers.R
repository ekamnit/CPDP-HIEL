##############################################
# At β = 0.1
##############################################
Project1 <- c(0.8749,0.8897,0.8745,0.9023,0.9129,0.9321)
Project2 <- c(0.7925,0.8097,0.8612,0.8624,0.8704,0.9033)
Project3 <- c(0.9302,0.8859,0.866,0.8707,0.9173,0.9261)
Project4 <- c(0.9375,0.9352,0.9283,0.931,0.9315,0.9551)
Project5 <- c(0.8656,0.8675,0.8518,0.8702,0.8781,0.8827)
Project6 <- c(0.8088,0.7923,0.7806,0.8088,0.8088,0.8101)
Project7 <- c(0.8809,0.8412,0.8539,0.8665,0.8779,0.8915)
Project8 <- c(0.8544,0.8701,0.8737,0.8764,0.8886,0.8919)
Project9 <- c(0.8641,0.8779,0.9518,0.9498,0.8788,0.8855)
Project10 <- c(0.9081,0.9132,0.9149,0.9207,0.9224,0.9231)
Project11 <- c(0.8241,0.8285,0.8512,0.8437,0.8566,0.8589)
Project12 <- c(0.9138,0.9181,0.9208,0.92,0.9291,0.9298)
png(file = "MistakesInPWMV-0.1.jpg")
# Plot the bar chart.
plot(Project1, type = "o",col = "red", xlab = "Diverse Classifiers", ylab = "F-Measure Values",
     ylim=c(0.765,0.96), xlim=c(1,8), lwd = 2)
lines(Project2, type = "o", col = "blue", lwd = 2)
lines(Project3, type = "o", col = "orange", lwd = 2)
lines(Project4, type = "o", col = "darkgreen", lwd = 2)
lines(Project5, type = "o", col = "brown", lwd = 2)
lines(Project6, type = "o", col = "black", lwd = 2)
lines(Project7, type = "o", col = "gold", lwd = 2)
lines(Project8, type = "o", col = "hotpink", lwd = 2)
lines(Project9, type = "o", col = "purple", lwd = 2)
lines(Project10, type = "o", col = "Turquoise", lwd = 2)
lines(Project11, type = "o", col = "violet", lwd = 2)
lines(Project12, type = "o", col = "grey", lwd = 2)
# Save the file.
legend("topright", 
       legend = c("Ant-1.7", " JEdit-4.3", "Redaktor", 
                  "Synapse-1.0", "Tomcat", "Velocity-1.6", "Xalan-2.4",
                  "Xerces-1.3", "MW1", "JM1", "Eclipse", "Mylyn"), 
       col = c("red", "blue", "orange", "darkgreen", "brown", "black", "gold",
               "hotpink", "purple", "Turquoise", "violet", "grey"), 
       pch = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F)
dev.off()

##############################################
# At β = 0.2
##############################################
Project1 <- c(0.8719,0.8829,0.8659,0.8837,0.9042,0.9147)
Project2 <- c(0.7925,0.8093,0.8546,0.8469,0.8679,0.8997)
Project3 <- c(0.9302,0.8704,0.861,0.862,0.8881,0.9196)
Project4 <- c(0.9375,0.9315,0.9246,0.9198,0.9315,0.9387)
Project5 <- c(0.8643,0.8666,0.8487,0.8637,0.8689,0.8802)
Project6 <- c(0.8088,0.7836,0.7833,0.7879,0.7967,0.7955)
Project7 <- c(0.8809,0.8412,0.8467,0.8649,0.8694,0.8899)
Project8 <- c(0.8439,0.8671,0.8597,0.875,0.8836,0.8852)
Project9 <- c(0.8559,0.8642,0.948,0.948,0.8679,0.8591)
Project10 <- c(0.9076,0.9132,0.9086,0.9207,0.9217,0.9231)
Project11 <- c(0.8159,0.8239,0.8505,0.8432,0.8509,0.8586)
Project12 <- c(0.9085,0.918,0.8987,0.9025,0.8939,0.8975)
png(file = "MistakesInPWMV-0.2.jpg")
# Plot the bar chart.
plot(Project1, type = "o",col = "red", xlab = "Diverse Classifiers", ylab = "F-Measure Values",
     ylim=c(0.765,0.96), xlim=c(1,8), lwd = 2)
lines(Project2, type = "o", col = "blue", lwd = 2)
lines(Project3, type = "o", col = "orange", lwd = 2)
lines(Project4, type = "o", col = "darkgreen", lwd = 2)
lines(Project5, type = "o", col = "brown", lwd = 2)
lines(Project6, type = "o", col = "black", lwd = 2)
lines(Project7, type = "o", col = "gold", lwd = 2)
lines(Project8, type = "o", col = "hotpink", lwd = 2)
lines(Project9, type = "o", col = "purple", lwd = 2)
lines(Project10, type = "o", col = "Turquoise", lwd = 2)
lines(Project11, type = "o", col = "violet", lwd = 2)
lines(Project12, type = "o", col = "grey", lwd = 2)
# Save the file.
legend("topright", 
       legend = c("Ant-1.7", " JEdit-4.3", "Redaktor", 
                  "Synapse-1.0", "Tomcat", "Velocity-1.6", "Xalan-2.4",
                  "Xerces-1.3", "MW1", "JM1", "Eclipse", "Mylyn"), 
       col = c("red", "blue", "orange", "darkgreen", "brown", "black", "gold",
               "hotpink", "purple", "Turquoise", "violet", "grey"), 
       pch = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F)
dev.off()

##############################################
# At β = 0.3
##############################################
Project1 <- c(0.871,0.8829,0.8655,0.8808,0.9012,0.9143)
Project2 <- c(0.8996,0.8063,0.8495,0.8436,0.8664,0.8946)
Project3 <- c(0.9302,0.8684,0.861,0.8659,0.8885,0.9196)
Project4 <- c(0.9375,0.9283,0.9236,0.9198,0.9315,0.9387)
Project5 <- c(0.8656,0.8648,0.8463,0.8633,0.8658,0.885)
Project6 <- c(0.8088,0.7836,0.7756,0.7879,0.7967,0.7911)
Project7 <- c(0.8809,0.8412,0.8467,0.8631,0.8654,0.8842)
Project8 <- c(0.8431,0.8671,0.8597,0.875,0.8836,0.8919)
Project9 <- c(0.8549,0.8619,0.948,0.948,0.8657,0.8591)
Project10 <- c(0.9075,0.9118,0.9086,0.9207,0.9217,0.9231)
Project11 <- c(0.8151,0.8263,0.8369,0.8358,0.8334,0.8439)
Project12 <- c(0.9065,0.8749,0.8985,0.8987,0.9027,0.9099)
png(file = "MistakesInPWMV-0.3.jpg")
# Plot the bar chart.
plot(Project1, type = "o",col = "red", xlab = "Diverse Classifiers", ylab = "F-Measure Values",
     ylim=c(0.765,0.96), xlim=c(1,8), lwd = 2)
lines(Project2, type = "o", col = "blue", lwd = 2)
lines(Project3, type = "o", col = "orange", lwd = 2)
lines(Project4, type = "o", col = "darkgreen", lwd = 2)
lines(Project5, type = "o", col = "brown", lwd = 2)
lines(Project6, type = "o", col = "black", lwd = 2)
lines(Project7, type = "o", col = "gold", lwd = 2)
lines(Project8, type = "o", col = "hotpink", lwd = 2)
lines(Project9, type = "o", col = "purple", lwd = 2)
lines(Project10, type = "o", col = "Turquoise", lwd = 2)
lines(Project11, type = "o", col = "violet", lwd = 2)
lines(Project12, type = "o", col = "grey", lwd = 2)
# Save the file.
legend("topright", 
       legend = c("Ant-1.7", " JEdit-4.3", "Redaktor", 
                  "Synapse-1.0", "Tomcat", "Velocity-1.6", "Xalan-2.4",
                  "Xerces-1.3", "MW1", "JM1", "Eclipse", "Mylyn"), 
       col = c("red", "blue", "orange", "darkgreen", "brown", "black", "gold",
               "hotpink", "purple", "Turquoise", "violet", "grey"), 
       pch = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F)
dev.off()

