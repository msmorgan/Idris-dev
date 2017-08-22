module UsefulProofs

total multTwoLeftPlusSame : (n : Nat) -> 2 * n = n + n
multTwoLeftPlusSame n = cong $ plusZeroRightNeutral n

total multTwoRightPlusSame : (n : Nat) -> n * 2 = n + n
multTwoRightPlusSame n = rewrite multCommutative n 2 in
                                 multTwoLeftPlusSame n


