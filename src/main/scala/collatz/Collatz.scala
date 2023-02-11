def collatz(n : Int) = n % 2 match
    case 0 => n / 2
    case 1 => 3*n + 1


def collatzCount(n : Int): Int = n match
    case 1 => 0
    case _ => collatzCount(collatz(n))+1