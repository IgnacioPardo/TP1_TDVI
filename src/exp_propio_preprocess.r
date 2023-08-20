
yes_no <- function(x) {
    if (x) {
        return("YES")
    } else {
        return("NO")
    }
}

# Read student data from "student-por.csv"
st <- read.csv("data/student-por.csv", header = TRUE, sep = ";")
noised_in_st <- st
numeric_keys <- c("famrel", "freetime", "goout", "Dalc", "Walc", "health", "absences", "traveltime", "studytime", "failures", "Medu", "Fedu")

# For each numeric column in the student data
# Find min and max values
# Generate noise from a uniform distribution with the same range
# With a certain probability, for each value in the column, replace it with the noise

for (key in numeric_keys) {
    min_val <- min(noised_in_st[, key])
    max_val <- max(noised_in_st[, key])
    noise <- runif(nrow(noised_in_st), min_val, max_val)
    noise <- round(noise)

    p <- runif(1, 0, 1)
    
    if (p < 0.2) {
        noised_in_st[, key] <- noise
    }
}

# Add a new column "GM" to store the average grade of G1, G2, G3
noised_in_st$GM <- (noised_in_st$G1 + noised_in_st$G2 + noised_in_st$G3) / 3

# Add a new column "pass" to noised_in_st the pass/fail result
noised_in_st$pass <- sapply(noised_in_st$GM >= 12, yes_no)

# Remove -G1, -G2, -G3, -GM
out <- noised_in_st[, !(names(noised_in_st) %in% c("G1", "G2", "G3", "GM"))]

# Write the result to "students.csv" without quotation marks 
write.csv(out, "data/students_noised_input.csv", quote = FALSE)


noised_out_st <- st

# Grades go from 0 to 20

grades_keys <- c("G1", "G2", "G3")

# For each grade column in the student data
# Generate noise from a uniform distribution 0 to 20
# With a certain probability, for each value in the column, replace it with the noise

for (key in grades_keys) {
    noise <- runif(nrow(noised_out_st), 0, 20)
    noise <- round(noise)

    p <- runif(1, 0, 1)
    
    if (p < 0.2) {
        noised_out_st[, key] <- noise
    }
}

# Add a new column "GM" to store the average grade of G1, G2, G3
noised_out_st$GM <- (noised_out_st$G1 + noised_out_st$G2 + noised_out_st$G3) / 3

# Add a new column "pass" to noised_out_st the pass/fail result
noised_out_st$pass <- sapply(noised_out_st$GM >= 12, yes_no)

# Remove -G1, -G2, -G3, -GM
out <- noised_out_st[, !(names(noised_out_st) %in% c("G1", "G2", "G3", "GM"))]

write.csv(out, "data/students_noised_output.csv", quote = FALSE)
