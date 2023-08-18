yes_no <- function(x) {
    if (x) {
        return("YES")
    } else {
        return("NO")
    }
}

# Read student data from "student-por.csv"
st <- read.csv("data/student-por.csv", header = TRUE, sep = ";")

# Add a new column "GM" to store the average grade of G1, G2, G3
st$GM <- (st$G1 + st$G2 + st$G3) / 3

# Add a new column "pass" to store the pass/fail result
st$pass <- sapply(st$GM >= 12, yes_no)

# Remove -G1, -G2, -G3, -GM
out <- st[, !(names(st) %in% c("G1", "G2", "G3", "GM"))]

# Write the result to "students.csv"
write.csv(out, "data/students.csv")
