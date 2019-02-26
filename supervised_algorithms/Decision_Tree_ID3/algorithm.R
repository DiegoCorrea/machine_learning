library(plyr)
class_a = 5
class_b = 9
class_total = 14

entropy <- function(class_a, class_b, total) {
  (-(class_a/total)*log2(class_a/total) - (class_b/total)*log2(class_b/total))
}

gain <- function(set_entropy, class_list, label_list) {
}

print(entropy(class_a, class_b, class_total))
