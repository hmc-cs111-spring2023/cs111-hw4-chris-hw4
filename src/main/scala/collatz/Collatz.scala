/** Divide by two if the input is even, multiply by three and add one if it's odd */
def collatz(n : Int) = n % 2 match
    case 0 => n / 2
    case 1 => 3*n + 1

/** Return how many times it takes to run collatz on the given input and reach one*/
def collatzCount(n : Int): Int = n match
    case 1 => 0
    case _ => collatzCount(collatz(n))+1