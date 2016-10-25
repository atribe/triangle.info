is.valid.triangle <- function(a, b, c) {
  # returns true or false based if the triangle is valid or not
  side.vector <- c(a, b, c)
  return(sum(side.vector) - max(side.vector) > max(side.vector))
}

is.right.triangle <- function(a, b, c) {
  # created a sorted vector, largest side (hypotenuse) will always be last element
  ordered.vector <- sort(c(side.a(),side.b(),side.c()))
  
  # pathagorean theorem, if true right triangle
  # a^2 + b^2 == c^2?
  # result is returned
  ordered.vector[1]^2 + ordered.vector[2]^2 == ordered.vector[3]^2
}

law.of.cosines <- function(a, b, c) {
  # Law of cosines, returns the angle opposite of c
  gamma <- acos((a^2 + b^2 - c^2)/(2*a*b))
}

x.pos <- function(gamma, h) {
  # x position of a right triangle with the angle gamma and side length h
  x <- h * sin(pi/2 - gamma)
}

y.pos <- function(gamma, h) {
  # y position of a right triangle with the angle gamma and side length h
  y <- h * sin(gamma)
}